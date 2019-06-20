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

prey_raw <- read_csv(here::here("data/1_prey_data.csv"))

prey_g <- prey_raw %>% 
  rename(preyID = ghopID,
         ghop_num = ghop.num,
         ghop_type = ghop.type,
         mass_tube = mass.ghop.tube,
         mass_tube_wet = mass.ghop.wet,
         ghop_instar = ghop.instar,
         mass_tube_dry.G = mass.ghop.dry.G,
         analytical_fate = analytical.fate)
# removed the one fed but abandoned ghop
prey_g <- prey_g[which(!(prey_g$feeding == 1 & prey_g$ghop_num == 25)),]

#==========================================================
# 2. PREDATOR PROPERTIES
#==========================================================

pred_m <- read_csv(here::here("data/1_predator_mantid.csv")) %>% 
  rename(pred_product = sample.type,
         mass_tube = mass.tube,
         mass_tube_wet = mass.tube.wet,
         mass_tube_dry = mass.tube.dry,
         mass_tube.g = mass.g.tube,
         mass_tube.g_wet = mass.g.tube.wet, #only a few 
         mass_tube.g_dry = mass.g.tube.dry.after)
pred_s <- read_csv(here::here("data/1_predator_spider.csv"))

# fxn to fix missing values for mg masses


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