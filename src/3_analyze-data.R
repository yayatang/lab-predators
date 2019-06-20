library(tidyverse)
library(lubridate)
library(zoo)
source(here::here('src/yaya_fxns.r'))

#====
# known standard gas CO2 ppm value
std_ppm <- 1997
# amount the ppm adjusts with each 5ml co2-free air injection
inj_constant <- 1.096
# converting to CO2 micromoles, then micrograms-C, divide for ppm->microM
co2c_const <- 0.05 * 12.0011 / 22.4

# gross value of true ppm
data1_orig$samp_by_std <- (std_ppm * (data1_orig$integral/data1_orig$std_vector))*(inj_constant^data1_orig$inject_num)

# calculate _final_ CO2 ppm rate per day, in units CO2-C (microg C/g/h)
data1_orig$samp_co2_total <- data1_orig$samp_by_std * co2c_const / data1_orig$soil_dry
data1_orig$samp_co2_rate <- data1_orig$samp_co2_total / data1_orig$incub_hours
data1_orig <- mutate(data1_orig, tube_perday = samp_co2_rate*24)

ss

data2_clean <- data1_amends %>%
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

# merge tube data with control values and calc tube diff
data3_individual <- data2_clean %>%
  left_join(control_lookup, by = c("exp_count")) %>% 
  mutate(tube_diff = tube_perday - ctrl_mean,
         real_data = TRUE)
  # mutate(tube_diff = tube_perday) # this is to plot without subtracting controls

# create an empty tibble for all tubes across all days (to infer/calc)
grid_vals <- as_tibble(expand.grid(all_tubes, all_days))
colnames(grid_vals) <- c("sampleID", "exp_count")

# merge data with empty tibble, and add phase variable
data4_gapped <- left_join(grid_vals, data3_individual,by=c('sampleID','exp_count'))
data4_gapped <- data4_gapped %>% 
  mutate(phase = if_else(data4_gapped$exp_count <= max_p1, 1, 2),
         real_data =  if_else(is.na(data4_gapped$real_data), FALSE, TRUE))

# fill out tibble with inferred data for each tube
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
  summarize_at(vars(by_tube_diff_cumul), list(~mean(., na.rm = TRUE), ~se(.))) %>% 
  rename(by_trt_cumul_mean = mean,
         by_trt_cumul_se = se) %>% 
  ungroup()

all_summarized_by_trt <- full_join(summ_by_trt_daily, summ_by_trt_cumul, by=c('trt','exp_count'))

data8_to_graph <- left_join(data7_filled, all_summarized_by_trt, by=c('trt', 'exp_count'))
data8_to_graph[which(is.na(data8_to_graph$se)),]$by_trt_cumul_se <- NA
data8_to_graph[which(is.na(data8_to_graph$se)),]$by_trt_daily_se <- NA

write_csv(data8_to_graph, "results/data_to_graph.csv")
saveRDS(data8_to_graph, "results/data_to_graph.rds") # for saving factor levels
