#==========================================================
# Import and clean IRGA data from datasheets
# June 2019
#==========================================================

library(here)
library(readr)
library(dplyr)
library(lubridate)
library(zoo)
source(here::here('src/yaya_fxns.R'))

#==========================================================
# 1. PREY PROPERTIES
#==========================================================

prey_feed1 <- read_csv(here::here("data/feedings/1 feeding1-ghops.csv"))
prey_feed1 <- prey_feed1[which(!(prey_feed1$feeding == 1 & prey_feed1$ghop_num == 25)),]

prey_feed2 <- read_csv(here::here("data/feedings/1 feeding2-ghops.csv")) %>% 
  mutate(notes = NA)
prey_feed3 <- read_csv(here::here("data/feedings/1 feeding3-ghops.csv")) %>% 
  rename(notes = comments)

prey_all <- rbind(prey_feed1, prey_feed2, prey_feed3) %>% 
  rename(predatorID = predator_ID,
         tube_ghop_wet = tube_ghop_mass) %>% 
  mutate(preyID = paste0("G", ghop_num, "F", feeding))

# still need dry data

#==========================================================
# 2. PREDATOR PROPERTIES
#==========================================================



#==========================================================
# 4. IRGA DATA
#==========================================================
#=== read IRGA treatment allocation by tube

tube_trts <- read_csv(here::here("data/IRGA prep/0_irga_treatments_by.tube.csv"))


#====
# read all files in the directory + flatten, add labels for treatments
file_list <- list.files(path=here::here("data/IRGA/"), pattern="*.csv", full.names = TRUE)
all_samp <- lapply(file_list, get_info)

all_samp2 <- bind_rows(all_samp) %>%
  arrange(phase, incub_count)


#====
# import sampleID label info
trt_index <- read_csv(here::here('data/IRGA prep/00_trt_index.csv')) %>%
  select(-rep)

all_data <- left_join(all_samp2, trt_index, by=c('sampleID'))

#====
# import dry soil data
soil_raw <- read_csv(here::here('data/IRGA prep/00_setup_2-tubes-soil.csv'))
colnames(soil_raw) <- c('trt', 'rep', 'tube', 'target_soil', 'target_tube_soil', 'actual_tube_soil')
soil_raw$rep <- as.integer(soil_raw$rep)

soil_added <- soil_raw %>% 
  mutate(soil_actual = actual_tube_soil - tube) %>% 
  select(trt, rep, soil_actual)

# merge soil with IRGA data
all_data2 <- left_join(all_data, soil_added, by=c('trt', 'rep'))

#====
# fix reference + control tubes label switches in phase 1
switch_list <- read_csv(here::here('data/1_tubes_switch.csv'))
all_data3 <- switch_tubes(all_data2, switch_list)

#====
# import ghop mass fed to predators


#====
# import treatment added biomass data
prey_raw <- read_csv(here::here('data/IRGA prep/1_trt_mass.csv'))
prey_raw$rep <- as.integer(prey_raw$rep)
colnames(prey_raw) <- c('trt', 'rep', 'tube_soil', 'tube_soil_trt')
prey_added <- prey_raw %>% 
  mutate(trt_added = tube_soil_trt - tube_soil) %>% 
  select(trt, rep, trt_added)
all_data4 <- left_join(all_data3, prey_added, by=c('trt', 'rep')) %>% 
  select(sampleID, trt, rep, tube_num, everything())


write_csv(all_data4, here::here('results/all_samp_clean.csv'))