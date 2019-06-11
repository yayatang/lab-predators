library(tidyverse)

library(lubridate)
library(zoo)
library(plotly)
source(here::here('src/yaya_fxns.r'))

# import treatment data and convert it to factors with levels
trt_key <- read_csv(here::here('data/0_trt-key.csv')) %>%
  arrange(trt_order)
trt_key$trt <- factor(trt_key$trt, levels=trt_key$trt[order(trt_key$trt_order)])
# all_trt <- trt_key$trt

# read imported and clean data
data0_raw <- read_csv(here::here('results/all_samp_clean.csv')) %>% 
  arrange(sampleID)
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

# calculate the DRY soil added from WHC values
loess_moisture <- read_csv(here::here('data/IRGA prep/00_moisture_soil.csv')) %>% 
  select(gmc.fresh, whc100.fresh)

loess_gmc <- loess_moisture$gmc.fresh
loess_whc <- loess_moisture$whc100.fresh

# convert "fresh" soil values into actual dry soil
data1_orig <- data1_orig %>% 
  mutate(soil_dry = round(data1_orig$soil_actual - (data1_orig$soil_actual*(loess_gmc/100)), digits=4))

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

# adjust by biomass, un/comment for adjusted values
amendments_raw <- read_csv(here::here('data/IRGA prep/1_trt_mass.csv'))
colnames(amendments_raw) <- c('trt', 'rep', 'before', 'after')
amendments_raw$trt <- factor(amendments_raw$trt, levels=trt_key$trt[order(trt_key$trt_order)])
amendments_raw$rep <- as.integer(amendments_raw$rep)
amendments_orig <- amendments_raw %>%
  left_join(tube_trt_info, all.x=TRUE, by=c('trt', 'rep')) %>%
  mutate(amend_mass = if_else(trt != 'C' & trt != 'R', after - before, NA_real_))

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
  summarize_at(vars(by_tube_diff_cumul), list(~mean(., na.rm = TRUE), ~se(.))) %>% 
  rename(by_trt_cumul_mean = mean,
         by_trt_cumul_se = se) %>% 
  ungroup()

all_summarized_by_trt <- full_join(summ_by_trt_daily, summ_by_trt_cumul, by=c('trt','exp_count'))

data8_to_graph <- left_join(data7_filled, all_summarized_by_trt, by=c('trt', 'exp_count'))

