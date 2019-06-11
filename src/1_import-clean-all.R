# =========================
# Import and clean IRGA data from datasheets
# June 2019
# =========================

library(here)
library(readr)
library(dplyr)
library(lubridate)
library(zoo)
source(here::here('src/yaya_fxns.R'))

# read all files in the directory + flatten
file_list <- list.files(path=here::here("data/IRGA/raw"), pattern="*.csv", full.names = TRUE)
all_samp <- lapply(file_list, get_info)

all_samp2 <- bind_rows(all_samp) %>%
  arrange(phase, incub_count)

trt_index <- read_csv(here::here('data/IRGA prep/00_trt_index.csv')) %>%
  select(-rep)

all_data <- left_join(all_samp2, trt_index, by=c('sampleID'))

# import dry soil data
soil_raw <- read_csv(here::here('data/IRGA prep/00_setup_2-tubes-soil.csv'))
colnames(soil_raw) <- c('trt', 'rep', 'tube', 'target_soil', 'target_tube_soil', 'actual_tube_soil')
soil_raw$rep <- as.integer(soil_raw$rep)

soil_added <- soil_raw %>% 
  mutate(soil_actual = actual_tube_soil - tube) %>% 
  select(trt, rep, soil_actual)

# merge soil with IRGA data
all_data2 <- left_join(all_data, soil_added, by=c('trt', 'rep'))

# fix reference + control tubes label switches in phase 1
switch_list <- read_csv(here::here('data/1_tubes_switch.csv'))
switch_phase <- 1
all_data3 <- switch_tubes(all_data2, switch_phase, switch_list)

# import treatment added biomass data
prey_raw <- read_csv(here::here('data/IRGA prep/1_trt_mass.csv'))
prey_raw$rep <- as.integer(prey_raw$rep)
colnames(prey_raw) <- c('trt', 'rep', 'tube_soil', 'tube_soil_trt')
prey_added <- prey_raw %>% 
  mutate(trt_added = tube_soil_trt - tube_soil) %>% 
  select(trt, rep, trt_added)
all_data4 <- left_join(all_data3, prey_added, by=c('trt', 'rep'))

write_csv(all_data4, here::here('results/all_samp_clean.csv'))