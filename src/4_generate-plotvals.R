library(tidyverse)
library(plotly)
source(here::here('src/yaya_fxns.R'))

imported_data <- readRDS("results/3_data_to_graph.rds") %>% 
  filter(trt != 'R')
trt_key <- unique(imported_data$trt)

graph_data <- imported_data %>% 
  filter(trt!='WN')

#=====
# prepping data into two tables for graphing
# tube table
# 0) daily values ready
# 1) calculate cumulative vals, 
# 2) calculate phase cumulative vals

by_tube <- graph_data %>%
  group_by(tubeID) %>%
  arrange(exp_count) %>%
  add_phase() %>% 
  mutate(cumul_gross = order_by(exp_count, cumsum(infer_tube_total_daily)),
         cumul_diff = order_by(exp_count, cumsum(infer_tube_diff_daily))) %>%
  rename(tube_se = ctrl_se) %>% # *** check cumul variable names
  group_by(tubeID, phase) %>%
  mutate(cumul_phase_gross = order_by(exp_count, cumsum(infer_tube_total_daily)),
         cumul_phase_diff = order_by(exp_count, cumsum(infer_tube_diff_daily))) %>% 
  ungroup()

saveRDS(by_tube, here::here('results/4_tubes_to_plot.rds'))

#------------------------------------

# treatment table
# 1) generate mean daily gross values, and se by treatment
# 2) sum up cumulative values by phase and by exp_count
# for merging treatment meta data
trt_meta <- unique(by_tube[,c('trt', 'ghop_fate', 'exp_count', 'real_data')])
tubes_meta <- unique(by_tube[,c('tube_num', 'trt', 'ghop_fate', 'real_data','origin_dry')]) %>% 
  na.omit()


# filter (inferred) daily data for control tubes only
c_tube_daily <- by_tube %>% 
  filter(trt == 'C') %>% 
  rename(c_daily_gross = infer_tube_total_daily,
         c_cumul_gross = by_tube_total_cumul, # c_cumul_phase = cumul_phase_gross,
         c_daily_se = tube_se) %>% 
  # select(tubeID, ghop_fate, trt, exp_count, phase, phase_count, real_data, 
  # c_daily_gross, c_cumul_gross, c_daily_se) %>% 
  ungroup() %>% 
  add_phase()

#=========
# for UNADJUSTED values

# calculate avg control tube daily values
c_trt_daily <- c_tube_daily %>%
  group_by(trt, exp_count) %>%
  summarise_each(list(~mean(., na.rm=TRUE), ~se), c_daily_gross) %>% 
  rename(c_daily_mean = mean,
         c_daily_se = se) 

# calculate avg cumulative CO2 for control tubes
c_trt_cumul <- c_tube_daily%>% 
  group_by(trt, exp_count) %>% 
  summarise_each(list(~mean(., na.rm=TRUE), ~se), c_cumul_gross) %>% 
  rename(c_cumul_mean = mean,
         c_cumul_se = se) %>% 
  ungroup()

# merge cumulative values with daily values for a mega control table
c_trt_summarized <- c_trt_cumul %>% 
  left_join(trt_meta) %>% 
  left_join(c_trt_daily) %>%
  select(-trt, -ghop_fate)

# get_ghop_fate <- unique(by_tube[, c('trt','ghop_fate')])

# unadjusted starts here
by_trt_daily <- by_tube %>% 
  # left_join(get_ghop_fate) %>%  #makes sure there's no NAs for ghop_fate
  group_by(trt, exp_count) %>% 
  summarise_each(list(~mean(., na.rm=TRUE), ~se), infer_tube_total_daily) %>% 
  rename(trt_daily_gross = mean,
         trt_daily_se = se) %>% 
  # select(-ghop_fate) %>%
  left_join(c_trt_summarized) %>% 
  ungroup()

by_trt_cumul <- by_tube %>% 
  group_by(trt, exp_count) %>% 
  summarise_each(list(~mean(., na.rm=TRUE), ~se), cumul_gross) %>% 
  rename(trt_cumul_gross = mean,
         trt_cumul_se = se)

trt_summ <- full_join(by_trt_daily, by_trt_cumul) 
trt_summ <- trt_summ %>% 
  left_join(trt_meta) %>% 
  ungroup()

trt_summ <- trt_summ %>%
  left_join(c_trt_cumul) %>%  # merge control trt data
  mutate(trt_daily_net = trt_daily_gross - c_daily_mean,
         trt_cumul_net = trt_cumul_gross - c_cumul_mean) %>% 
  select(trt, exp_count, ghop_fate, everything()) 

trt_summ[trt_summ$real_data == FALSE,]$c_cumul_se <- NA
trt_summ[trt_summ$real_data == FALSE,]$trt_daily_se <- NA
trt_summ[trt_summ$real_data == FALSE,]$trt_cumul_se <- NA

trt_summ <- add_phase(trt_summ)

saveRDS(trt_summ, here::here('results/4_trts_to_plot.rds'))

#======================
# for adjusting predator treatments to ghop input (thus no NET values)
# for getting the right origin_dry data
by_tube_a <- by_tube %>% 
  filter(trt != 'C') %>% 
  select(-trt, -ghop_fate, -origin_dry, -real_data) %>% 
  left_join(tubes_meta, by='tube_num') %>%
  mutate(daily_gross_a = infer_tube_total_daily/origin_dry,
         cumul_gross_a = cumul_gross/origin_dry)

by_trt_daily_a <- by_tube_a %>% 
  group_by(trt, exp_count) %>% 
  summarise_each(list(~mean(., na.rm=TRUE), ~se), daily_gross_a) %>% 
  rename(trt_daily_gross = mean,
         trt_daily_se = se) %>% 
  left_join(c_trt_summarized)

by_trt_cumul_a <- by_tube_a %>% 
  group_by(trt, exp_count) %>% 
  summarise_each(list(~mean(., na.rm=TRUE), ~se), cumul_gross_a) %>% 
  rename(trt_cumul_gross = mean,
         trt_cumul_se = se) %>% 
  ungroup()

trt_summ_a <- full_join(by_trt_daily_a, by_trt_cumul_a) 
trt_summ_a <- trt_summ_a %>% 
  left_join(tubes_meta) 

trt_summ_a <- trt_summ_a %>%
  left_join(c_trt_cumul) %>%  # merge control trt data
  select(trt, exp_count, ghop_fate, everything()) 

trt_summ_a[trt_summ_a$real_data == FALSE,]$c_cumul_se <- NA
trt_summ_a[trt_summ_a$real_data == FALSE,]$trt_daily_se <- NA
trt_summ_a[trt_summ_a$real_data == FALSE,]$trt_cumul_se <- NA

trt_summ_a <- add_phase(trt_summ_a)

saveRDS(trt_summ_a, here::here('results/4_trts_to_plot_adj.rds'))

#======================
# for adjusting predator poop to poop mass
by_tube_p <- by_tube %>% 
  filter(trt != 'C') %>% 
  select(-trt, -ghop_fate, -origin_dry, -real_data) %>% 
  left_join(tubes_meta, by='tube_num') %>%
  mutate(daily_gross_p = infer_tube_total_daily/trt_added_mass,
         cumul_gross_p = cumul_gross/trt_added_mass)

by_trt_daily_p <- by_tube_p %>% 
  group_by(trt, exp_count) %>% 
  summarise_each(list(~mean(., na.rm=TRUE), ~se), daily_gross_p) %>% 
  rename(trt_daily_gross = mean,
         trt_daily_se = se) %>% 
  left_join(c_trt_summarized)

by_trt_cumul_p <- by_tube_p %>% 
  group_by(trt, exp_count) %>% 
  summarise_each(list(~mean(., na.rm=TRUE), ~se), cumul_gross_p) %>% 
  rename(trt_cumul_gross = mean,
         trt_cumul_se = se) %>% 
  ungroup()

trt_summ_p <- full_join(by_trt_daily_p, by_trt_cumul_p) 
trt_summ_p <- trt_summ_p %>% 
  left_join(tubes_meta) 

trt_summ_p <- trt_summ_p %>%
  left_join(c_trt_cumul) %>%  # merge control trt data
  select(trt, exp_count, ghop_fate, everything()) 

trt_summ_p[trt_summ_p$real_data == FALSE,]$c_cumul_se <- NA
trt_summ_p[trt_summ_p$real_data == FALSE,]$trt_daily_se <- NA
trt_summ_p[trt_summ_p$real_data == FALSE,]$trt_cumul_se <- NA

trt_summ_p <- add_phase(trt_summ_p)

saveRDS(trt_summ_p, here::here('results/4_trts_to_plot_adj_poop.rds'))