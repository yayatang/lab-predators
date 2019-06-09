library(tidyverse)
library(lubridate)
library(zoo)
library(purrr)
library(plotly)

source(here::here('src/yaya_fxns.r'))

# import treatment data and convert it to factors with levels
trt_key <- read_csv(here::here('data/0_trt-key.csv')) %>%
  arrange(trt_order)
trt_key$trt <- factor(trt_key$trt, levels=trt_key$trt[order(trt_key$trt_order)])
all_trt <- trt_key$trt

# import sampling data and merge with treatment
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
# dsoil is in g, merge from table of actual values later
data1_orig$actual_dsoil <- 5
data1_orig$samp_co2_total <- data1_orig$samp_by_std * co2c_const / data1_orig$actual_dsoil
data1_orig$samp_co2_rate <- data1_orig$samp_co2_total / data1_orig$incub_hours
data1_orig <- mutate(data1_orig, tube_perday = samp_co2_rate*24)

# adjust by biomass, un/comment for adjusted values
amendments_raw <- read_csv('data/IRGA prep/00_trt_mass.csv')
colnames(amendments_raw) <- c('trt', 'rep', 'before', 'after')
amendments_raw$trt <- as.factor(amendments_raw$trt)
amendments_raw$rep <- as.numeric(amendments_raw$rep)
amendments_orig <- amendments_raw %>%
  left_join(tube_trt_info, all.x=TRUE, by=c('trt', 'rep')) %>%
  mutate(amend_mass = after - before)

amendments_orig$amend_mass[is.na(amendments_orig$amend_mass)] <- 1
amendments_clean <- amendments_orig %>%
  select(sampleID, amend_mass)

data1_amends <- inner_join(data1_orig, amendments_clean, by=c('sampleID'))
data1_amends$tube_perday <- data1_amends$tube_perday/data1_amends$amend_mass

data2_clean <- data1_amends %>%
# data2_clean <- data1_orig %>%
  rename(phase_count = incub_count) %>% 
  select('sampleID', 'exp_count', 'phase', 'phase_count', 'trt', 'rep', 'tube_perday')

control_lookup <- data2_clean %>%
  filter(trt=='C') %>%
  select(exp_count, tube_perday) %>%
  group_by(exp_count) %>% 
  summarize_at(vars(tube_perday), list(~mean(., na.rm = TRUE), ~se(.))) %>% 
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
  # mutate(tube_diff = tube_perday - ctrl_mean)
  mutate(tube_diff = tube_perday) # this is to plot without subtracting controls

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
         by_tube_total_cumul = cumsum(infer_tube_total_daily)) %>% 
  left_join(ctrl_infer[,c('exp_count', 'ctrl_cumul')], by=c('exp_count')) # %>% 

data7_filled <- data6_cumul %>% 
  left_join(tube_trt_info, all.x=TRUE, by=c("sampleID")) %>%
  left_join(exp_counting, by = c("exp_count","phase")) %>% 
  select(sampleID, exp_count, phase, phase_count, everything())

summ_by_trt_daily <- data7_filled %>% 
  group_by(trt, exp_count) %>% 
  summarize_at(vars(infer_tube_diff_daily), list(~mean(., na.rm = TRUE), ~se(.))) %>% 
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

# #un comment to make daily plots by tube
# # for one plot with conditional to make it tube vs trt
# daily_plot <- function(graph_unit, graph_data, max_p1) {
#   # unit 1 = tube
#   # unit 2 = treatment
#   
#   graph_unit <- 1
#   graph_data <- data8_to_graph
#   
#   if(graph_unit==1){
#     user_input <- readline("Which treatment?")  
#     trt_to_plot <- as.character(user_input)
#     
#     graph_data <- filter(graph_data, trt==trt_to_plot)
#     
#     plot_by_tube <- ggplot(graph_data, aes(exp_count, infer_tube_diff_daily, color=sampleID)) +
#       # facet_grid(~phase, scales="free") +
#       geom_vline(xintercept=max_p1) +
#       geom_hline(yintercept=0) +
#       geom_line(size=0.5) +
#       geom_point(size=0.7) +
#       geom_errorbar(aes(ymin=infer_tube_diff_daily-ctrl_se, ymax=infer_tube_diff_daily+ctrl_se), width=0.3) +
#       labs(x="Experimental days lapsed", y="Daily CO2-C") +
#       ggtitle(paste('Daily relative CO2-C values by tube'))
#     ggplotly(plot_by_tube)
#   } else {
#     plot_by_treatment <- ggplot(graph_data, aes(exp_count, by_trt_daily_mean, color=trt)) +
#       # facet_grid(~phase, scales="free") +
#       geom_vline(xintercept=max_p1) +
#       geom_hline(yintercept=0) +
#       geom_line(aes(group=trt), size=0.5) +
#       geom_point(size=0.7) +
#       geom_errorbar(aes(ymin=by_trt_daily_mean-se_reps, ymax=by_trt_daily_mean+se_reps), width=0.3) +
#       labs(x="Experimental days lapsed", y="Daily CO2-C") +
#       ggtitle(paste('Daily relativeCO2-C values by treatment'))
#     ggplotly(plot_by_treatment)
#   }
# }
# 
# daily_plot(1, data8_to_graph, max_p1)

# uncomment to re-functionalize
# all_plots <- function(graph_data, max_p1) {

graph_data <- data8_to_graph %>% 
  filter(trt!='R',
         trt!='WN',
         trt!='WS',
         trt!='WW')

var_to_graph <- c('infer_tube_total_daily',
                  'by_tube_total_cumul',
                  'infer_tube_diff_daily',
                  'by_tube_diff_cumul',
                  'by_trt_daily_mean',
                  'by_trt_cumul_mean')
se_to_graph <- c(rep(c('ctrl_se'),4),
                 'by_trt_daily_se',
                 'by_trt_cumul_se')
graph_group <- c(rep(c('sampleID'),4), rep(c('trt'),2))
y_titles <- rep(c('Daily CO2-C','Cumulative CO2-C'),3)
plot_titles <- c('Daily CO2 Total by Tube',
                 'Cumulative CO2 Total by tube',
                 'Daily CO2 difference from control, by tube',
                 'Cumulative CO2 by tube',
                 'Daily CO2 difference by treatment, adjusted',
                 'Cumulative CO2 by treatment, biomass adjusted')
dynamic_data <- tibble(var_to_graph, se_to_graph, graph_group, y_titles, plot_titles)

# graph_data
#   exp_count
#   var to graph
#   sampleID
#   se var
#   y axis title
#   graph overall title
#   
#   1 = daily total by tube
#   2 = cumul total by tube
#   3 = daily diff by tube
#   4 = cumul diff by tube
#   5 = daily diff by treatment mean
#   6 = cumul diff by treatment mean
#     
#   infer_tube_total_daily
#   infer_tube_diff_daily
#   by_tube_diff_cumul
#   by_tube_total_cumul
#   by_trt_daily_mean (+ by_trt_daily_se)
#   by_trt_cumul_mean (+ by_trt_cumul_se)

# all_plots <- htmltools::tagList()

# for (i in seq_along(length(dynamic_data))){


# i is the type of graph, according to the titles above
i <- 6
selected_data <- graph_data %>% 
  select(sampleID, exp_count, trt, rep, !!dynamic_data$var_to_graph[[i]], 
         !!dynamic_data$se_to_graph[[i]], !!dynamic_data$graph_group[[i]], animal_group)
renamed_data <- selected_data %>% 
  rename(graph_yvar = !!dynamic_data$var_to_graph[[i]],
         graph_se = !!dynamic_data$se_to_graph[[i]])

if (i >= 5) {
  plot_data <- renamed_data %>% 
    ungroup() %>% 
    filter(rep==1) %>% 
    rename(Treatment = trt) %>% 
    select(-sampleID, -rep)
  
} else {
  plot_data <- renamed_data %>%
    rename(graph_unit = sampleID) %>% 
    select(-trt)}

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")

any_plot <- ggplot(plot_data, aes(exp_count, graph_yvar, color=Treatment)) +
  # facet_grid(~phase, scales="free") +
  geom_vline(xintercept=max_p1, color="grey", size = 0.3) +
  geom_hline(yintercept=0) +
  geom_line(size=0.5, aes(linetype = animal_group)) +
  # geom_point(size=1) +
  # geom_errorbar(aes(ymin = graph_yvar - graph_se,
                #     ymax = graph_yvar + graph_se),
                # width=0.3) +
  labs(x="Experimental days lapsed", y=dynamic_data$y_titles[[i]]) +
  ggtitle(paste(dynamic_data$plot_titles[[i]])) +
  scale_color_manual(values = cbbPalette) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

# any_plot$labels$fill <- "Soil input treatment" 

any_plot
ggsave(paste0('results/NEW',i,'_by_', dynamic_data$graph_group[i], '_with_amends_.png'), width=5, height=4, dpi=1000)
# ggsave(paste0('results/NEW',i,'_by_', dynamic_data$graph_group[i], '_no_amends_.png'), width=5, height=4, dpi=1000)

# ggplotly(any_plot)
# all_plots[[i]] <- as_widget(ggplotly(any_plot))
# all_plots[[i]] <- any_plot
# }
# }

# invoke(all_plots, ggplotly)
