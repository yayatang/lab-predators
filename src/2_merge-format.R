# =========================
# Merge IRGA data with prep data, and format factors
# June 2019
# =========================

library(tidyverse)
library(lubridate)
source(here::here('src/yaya_fxns.r'))


#====
# import treatment data and convert it to factors with levels
trt_key <- read_csv(here::here('data/0_trt-key.csv')) %>%
  arrange(trt_order)
trt_key$trt <- factor(trt_key$trt, levels=trt_key$trt[order(trt_key$trt_order)])

# read imported and declare factor levels
data0_raw <- read_csv(here::here('results/all_samp_clean.csv')) %>% 
  arrange(sampleID) %>% 
  rename(phase_count = incub_count)
data0_raw$trt <- factor(data0_raw$trt, levels=trt_key$trt[order(trt_key$trt_order)])
data0_raw$sampleID <- as.factor(data0_raw$sampleID)

max_p1 <- get_phase1_max(data0_raw)

data1_orig <-  data0_raw %>% 
  mutate(exp_count = if_else(phase==2, phase_count + max_p1, phase_count)) %>% 
  # mutate(add_days = if_else(phase==2, max_p1, 0),
  #        exp_count = incub_count + add_days) %>%
  select(-rack, -position, -date_flush, -date_msre)

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

#==== 
#calculate the DRY soil added from WHC values
loess_moisture <- read_csv(here::here('data/IRGA prep/00_moisture_soil.csv')) %>% 
  select(gmc.fresh, whc100.fresh)
loess_gmc <- loess_moisture$gmc.fresh
loess_whc <- loess_moisture$whc100.fresh
# convert "fresh" soil values into actual dry soil
data1_orig <- data1_orig %>% 
  mutate(soil_dry = round(data1_orig$soil_actual - (data1_orig$soil_actual*(loess_gmc/100)), digits=4))


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
data1_amends$tube_perday <- data1_amends$tube_perday/data1_amends$amend_ma