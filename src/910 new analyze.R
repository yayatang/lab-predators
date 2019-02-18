library(tidyverse)
library(lubridate)
library(zoo)
library(purrr)
library(plotly)

source(here::here('src/yaya_fxns.r'))

trt_key <- read_csv(here::here('data/0_trt-key.csv')) %>%
  arrange(trt_order)
trt_key$trt <- factor(trt_key$trt, levels=trt_key$trt[order(trt_key$trt_order)])
all_trt <- trt_key$trt

data0_raw <- read_csv(here::here('data/IRGA/clean/clean_all_samp.csv'))
data0_raw$trt <- factor(data0_raw$trt, levels=trt_key$trt[order(trt_key$trt_order)])
data0_raw$sampleID <- as.factor(data0_raw$sampleID)

max_p1 <- get_phase1_max(data0_raw)
data1_orig <-  data0_raw %>% 
  mutate(add_days = if_else(phase==2, max_p1, 0),
         exp_count = incub_count + add_days) %>%
  select(-add_days, -rack, -position, -date_flush, -date_msre)

# info to be used later
irga_days <- unique(data1_orig$exp_count)
max_exp <- max(data1_orig$exp_count)
all_days <- seq(max_exp)
all_tubes <- unique(data1_orig$sampleID)

tube_trt_info <- na.omit(unique(data1_orig[c('sampleID', 'trt','rep')])) %>% 
  left_join(trt_key, by= c('trt'))
exp_counting <- tibble(exp_count = as.numeric(seq(max_exp))) %>% 
  mutate(phase = if_else(exp_count <= max_p1, 1, 2),
         phase_count = if_else(phase==2, exp_count-max_p1, exp_count))

# known standard gas CO2 ppm value
std_ppm <- 1997
# amount the ppm adjusts with each 5ml co2-free air injection
inj_constant <- 1.096
# converting to CO2 micromoles, then micrograms-C, divide for ppm->microM
co2c_const <- 0.05 * 12.0011 / 22.4

# gross value of true ppm
data1_orig$samp_by_std <- (std_ppm * (data1_orig$integral/data1_orig$std_vector))*(inj_constant^data1_orig$inject_num)

# calculate _final_ CO2 ppm rate per day, in units CO2-C (microg C/g/h)
# dsoil is in g, merge from table later
data1_orig$actual_dsoil <- 5
data1_orig$samp_co2_total <- data1_orig$samp_by_std * co2c_const / data1_orig$actual_dsoil
data1_orig$samp_co2_rate <- data1_orig$samp_co2_total / data1_orig$incub_hours
data1_orig <- mutate(data1_orig, tube_perday = samp_co2_rate*24)

data2_clean <- data1_orig %>% 
  rename(phase_count = incub_count) %>% 
  select('sampleID', 'exp_count', 'phase', 'phase_count', 'trt', 'rep', 'tube_perday')

control_lookup <- data2_clean %>%
  filter(trt=='C') %>%
  select(exp_count, tube_perday) %>%
  group_by(exp_count) %>% 
  summarize_at(vars(tube_perday),funs(mean(., na.rm = TRUE), se)) %>% 
  rename(ctrl_mean = mean) %>% 
  arrange(exp_count) %>% 
  mutate(ctrl_se = na.approx(se))
  
ctrl_infer <- as_tibble(expand.grid(exp_count = all_days)) %>% 
  left_join(control_lookup, by=c('exp_count')) %>% 
  arrange(exp_count) %>% 
  mutate(infer_ctrl = na.approx(ctrl_mean),
         ctrl_cumul = cumsum(infer_ctrl))

# find diff for tubes
data3_individual <- data2_clean %>%
  left_join(control_lookup, by = c("exp_count")) %>% 
  mutate(tube_diff = tube_perday - ctrl_mean)

grid_vals <- as_tibble(expand.grid(all_tubes, all_days))
colnames(grid_vals) <- c("sampleID", "exp_count")

data4_gapped <- left_join(grid_vals, data3_individual, by=c('sampleID','exp_count'))
data4_gapped <- mutate(data4_gapped, phase = if_else(data4_gapped$exp_count <= max_p1, 1, 2))

data5_inferred <- data4_gapped %>%
  group_by(sampleID) %>%
  arrange(exp_count) %>%
  mutate(infer_tube_total_daily = na.approx(tube_perday),
         infer_tube_diff_daily = na.approx(tube_diff)) %>% 
  select(-phase_count, -trt, -rep)

data6_cumul <- data5_inferred %>% 
  group_by(sampleID) %>% 
  arrange(exp_count) %>% 
  mutate(by_tube_diff_cumul = cumsum(infer_tube_diff_daily),
         by_tube_total_cumul = cumsum(infer_tube_total_daily),
         check_diff_from_ctrl = by_tube_total_cumul - by_tube_diff_cumul) %>% 
  left_join(ctrl_infer[,c('exp_count', 'ctrl_cumul')], by=c('exp_count')) # %>% 
  # select(sampleID, exp_count, by_tube_total_cumul, by_tube_diff_cumul, check_diff_from_ctrl, ctrl_cumul, everything())

data7_filled <- data6_cumul %>% 
  left_join(tube_trt_info, all.x=TRUE, by=c("sampleID")) %>%
  left_join(exp_counting, by = c("exp_count","phase")) %>% 
  select(sampleID, exp_count, phase, phase_count, everything())

summ_by_trt_daily <- data7_filled %>% 
  group_by(trt, exp_count) %>% 
  summarize_at(vars(infer_tube_diff_daily),funs(mean(., na.rm = TRUE), se)) %>% 
  rename(by_trt_daily_mean = mean,
         by_trt_daily_se = se) %>% 
  ungroup()

summ_by_trt_cumul <- data7_filled %>% 
  group_by(trt, exp_count) %>% 
  summarize_at(vars(by_tube_diff_cumul),funs(mean(., na.rm = TRUE), se)) %>% 
  rename(by_trt_cumul_mean = mean,
         by_trt_cumul_se = se) %>% 
  ungroup()

all_summarized_by_trt <- full_join(summ_by_trt_daily, summ_by_trt_cumul, by=c('trt','exp_count'))

data8_to_graph <- left_join(data7_filled, all_summarized_by_trt, by=c('trt', 'exp_count'))

daily_plot <- function(graph_unit, graph_data, max_p1) {
  # unit 1 = tube
  # unit 2 = treatment
  
  graph_unit <- 1
  graph_data <- data8_to_graph
  
  if(graph_unit==1){
    user_input <- readline("Which treatment?")  
    trt_to_plot <- as.character(user_input)
  
    graph_data <- filter(graph_data, trt==trt_to_plot)
    
    plot_by_tube <- ggplot(graph_data, aes(exp_count, infer_tube_diff_daily, color=sampleID)) +
      # facet_grid(~phase, scales="free") +
      geom_vline(xintercept=max_p1) +
      geom_hline(yintercept=0) +
      geom_line(size=0.5) +
      geom_point(size=0.7) +
      geom_errorbar(aes(ymin=infer_tube_diff_daily-ctrl_se, ymax=infer_tube_diff_daily+ctrl_se), width=0.3) +
      labs(x="Experimental days lapsed", y="Daily CO2-C") +
      ggtitle(paste('Daily relative CO2-C values by tube'))
    ggplotly(plot_by_tube)
  } else {
    plot_by_treatment <- ggplot(graph_data, aes(exp_count, by_trt_daily_mean, color=trt)) +
      # facet_grid(~phase, scales="free") +
      geom_vline(xintercept=max_p1) +
      geom_hline(yintercept=0) +
      geom_line(aes(group=trt), size=0.5) +
      geom_point(size=0.7) +
      geom_errorbar(aes(ymin=by_trt_daily_mean-se_reps, ymax=by_trt_daily_mean+se_reps), width=0.3) +
      labs(x="Experimental days lapsed", y="Daily CO2-C") +
      ggtitle(paste('Daily relativeCO2-C values by treatment'))
    ggplotly(plot_by_treatment)
  }
}

daily_plot(1, data8_to_graph, max_p1)

# think about whether it's obvious when there are geom_point and when there arent..
summarize_replicates <- function(trt_2sum, clean_data) {
  summ_data <- clean_data %>%
    filter(trt == trt_2sum) %>%
    select(exp_count, rep, tube_perday) %>%
    group_by(exp_count)%>%
    summarize_at(vars(tube_perday),funs(mean(., na.rm = TRUE), se)) %>% 
    ungroup()
  
  summ_data %>% mutate(trt = trt_2sum)
}

# to average replicates of each treatment per sampling day
data7_summarized <- map_dfr(all_trt, summarize_replicates, clean_data = data1_orig) %>%
  rename(gross_mean = mean, se_reps = se) %>%
  left_join(control_lookup, by=c("exp_count")) %>%
  mutate(net_mean = gross_mean - ctrl_mean)



gross_plots <- ggplot(setup_filled_data, aes(exp_count, gross_mean, color=trt)) +
  # facet_grid(~phase, scales="free") +
  geom_line(aes(group=trt), size=0.5) +
  geom_vline(xintercept=max_p1) +
  geom_hline(yintercept=0) +
  geom_point(size=0.7) +
  geom_errorbar(aes(ymin=gross_mean-se_reps, ymax=gross_mean+se_reps), width=0.3) +
  geom_hline(yintercept=0) +
  labs(x="Day per phase", y="Daily CO2-C") +
  ggtitle(paste('Daily CO2-C gross values'))
ggplotly(gross_plots)



# # this is the plot for daily trt vals, both phases
# new_plot <- ggplot(data=graph_data, aes(x=incub_count, y=net_mean, group=trt, color=trt))+
#   facet_grid(~phase, scales="free") +
#   geom_point()+
#   geom_line(aes(group=trt))+
#   geom_errorbar(aes(ymin = net_mean - se_reps, ymax= net_mean + se_reps), width=0.3) +
#   scale_color_brewer(palette = "Paired")+
#   ggtitle(paste("Daily C summarized by treatment"))
# ggplotly(new_plot)

# interpolate samples


graph_data <- setup_filled_data %>%
  filter(trt != 'R')

daily_min <- min(graph_data$net_mean)
daily_max <- max(graph_data$net_mean)

daily_plots <- ggplot(graph_data, aes(exp_count, net_mean, ymin=daily_min, ymax=daily_max, color=trt)) +
  # facet_grid(~phase, scales="free") +
  geom_line(aes(group=trt), size=0.5) +
  geom_vline(xintercept = max_p1) +
  geom_hline(yintercept=0) +
  geom_point(size=0.7) +
  geom_errorbar(aes(ymin=net_mean-se_reps, ymax=net_mean+se_reps), width=0.3) +
  geom_hline(yintercept=0) +
  labs(x="Day per phase", y="Daily CO2-C") +
  ggtitle(paste('Daily CO2-C difference from C'))
ggplotly(daily_plots)


# ==========================================================================

cumul_min <- min(graph_data$net_mean)
cumul_max <- max(graph_data$net_mean)

cumul_plots <- ggplot(cumul_graph, aes(exp_count, interp_cumul, ymin=cumul_min, ymax=cumul_max, color=trt)) +
  # facet_grid(.~phase, space="free_x") +
  geom_line(size=0.5) +
  geom_vline(xintercept=max_p1) +
  geom_hline(yintercept=0) +
  geom_point(size=0.7) +
  geom_errorbar(aes(ymin=interp_cumul-se_reps, ymax=interp_cumul+se_reps), width=0.3) +
  geom_hline(yintercept=0) +
  labs(x="Experiment day", y="Cumulative CO2-C") +
  ggtitle(paste('Cumulative CO2-C'))
ggplotly(cumul_plots)
