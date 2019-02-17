library(tidyverse)
library(lubridate)
library(purrr)

se <- function(vals_err_calc){
  # assume length = number of obs
  # standard error calculation excluding NAs
  val <- sd(vals_err_calc, na.rm=TRUE)/sqrt(sum(!is.na(vals_err_calc)))
}

data_0raw <- read_csv(here::here('data/IRGA/clean/clean_all_samp.csv'))
data_0raw$trt <- as.factor(data_0raw$trt)
all_trt <- unique(data_0raw$trt) 

# known standard gas CO2 ppm value
std_ppm <- 1997
# amount the ppm adjusts with each 5ml co2-free air injection
inj_constant <- 1.096
# converting to CO2 micromoles, then micrograms-C, divide for ppm->microM
co2c_const <- 0.05 * 12.0011 / 22.4

# gross value of true ppm
data_orig <- data_0raw
data_orig$samp_by_std <- (std_ppm * (data_orig$integral/data_orig$std_vector))*(inj_constant^data_orig$inject_num)

# calculate _final_ CO2 ppm rate per day, in units CO2-C (microg C/g/h)
# dsoil is in g, merge from table later
data_orig$actual_dsoil <- 5
data_orig$samp_co2_gross <- data_orig$samp_by_std * co2c_const / data_orig$actual_dsoil
data_orig$samp_co2_rate <- data_orig$samp_co2_gross / data_orig$incub_hours
data_orig <- mutate(data_orig, samp_co2_perday = samp_co2_rate*24)

data_orig <- data_orig %>% 
  select('phase', 'incub_count', 'sampleID', everything())

summarize_replicates <- function(trt_2sum, clean_data) {
  summ_data <- clean_data %>%
    filter(trt == trt_2sum) %>%
    select(phase, incub_count, rep, samp_co2_perday) %>%
    group_by(phase, incub_count)%>%
    summarize_at(vars(samp_co2_perday),funs(mean(., na.rm = TRUE), se)) %>% 
    ungroup()
  
  summ_data %>% mutate(trt = trt_2sum)
}

# to average replicates of each treatment per sampling day
all_summ <- map_dfr(all_trt, summarize_replicates, clean_data = data_orig) %>%
  rename(gross_mean = mean,
         se_reps = se)

# map_dfr(all_trt, ~summarize_stuff(trt_2sum = .x, 

# clean_data = trt_data))
summarized_data <- all_summ

control_lookup <- summarized_data %>%
  filter(trt=='C') %>%
  rename(ctrl_mean = gross_mean) %>%
  select(-trt, -se_reps)

all_summ_net <- left_join(summarized_data, control_lookup, by = c('phase', 'incub_count')) %>%
  mutate(net_mean = gross_mean - ctrl_mean)

# trt_ref <- unique(all_summ_net$trt) %>%
#   mutate(name = c("Reference","Control","Whole prey","Mantis Excreta",
#                   "Mantis prey remains","Mantis All","Widow All","Widow Excreta",
#                   "Widow Prey Remains NO silk","Widow Prey Remains With silk",
#                   "Widow Capture silk","Widow Nest Silk"))

graph_data <- all_summ_net %>%
  filter(trt != 'R')

daily_min <- min(graph_data$net_mean)
daily_max <- max(graph_data$net_mean)

test_plots <- ggplot(graph_data, aes(incub_count, net_mean, ymin=daily_min, ymax=daily_max, color=trt)) +
  facet_grid(~phase, scales="free") +
  geom_line(aes(group=trt), size=0.5) +
  geom_point(size=0.7) +
  geom_errorbar(aes(ymin=net_mean-se_reps, ymax=net_mean+se_reps), width=0.3) +
  geom_hline(yintercept=0) +
  labs(x="Day per phase", y="Daily CO2-C") +
  ggtitle(paste('Daily CO2-C'))
ggplotly(test_plots)


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
phase1 <- filter(all_summ_net, phase == 1)
max_p1 <- max(phase1$incub_count)

setup_filled_data <-  all_summ_net %>% 
  mutate(add_days = if_else(phase==2, max_p1, 0),
         exp_count = incub_count + add_days)
# exp_count_max <- map_int(as.integer(all_summ_net[,c('phase', 'incub_count')], ~get_exp_day))

max_exp <- max(setup_filled_data$exp_count)
all_days <- seq(max_exp)

grid_vals <- as_tibble(expand.grid(all_trt, all_days))
colnames(grid_vals) <- c("trt", "exp_count")

data_interp <- left_join(grid_vals, setup_filled_data, by=c('trt','exp_count'))
data_interp <- mutate(data_interp, phase = if_else(data_interp$exp_count <= max_p1, 1, 2))

cumul_graph <- data_interp %>%
  group_by(trt) %>%
  arrange(exp_count) %>% 
  mutate(interp_daily = na.approx(net_mean),
         interp_cumul = cumsum(interp_daily)) %>% 
  select(trt, exp_count, interp_daily, interp_cumul, se_reps, everything())


cumul_min <- min(graph_data$net_mean)
cumul_max <- max(graph_data$net_mean)

cumul_graph <- filter(cumul_graph, trt != 'R')

cumul_plots <- ggplot(cumul_graph, aes(exp_count, interp_cumul, ymin=cumul_min, ymax=cumul_max, color=trt)) +
  facet_grid(~phase, scales="free") +
  geom_line(size=0.5) +
  geom_point(size=0.7) +
  geom_errorbar(aes(ymin=interp_cumul-se_reps, ymax=interp_cumul+se_reps), width=0.3) +
  geom_hline(yintercept=0) +
  labs(x="Experiment day", y="Cumulative CO2-C") +
  ggtitle(paste('Cumulative CO2-C'))
ggplotly(cumul_plots)

