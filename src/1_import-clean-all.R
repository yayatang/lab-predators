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
  rename(sampleID = ghopID,
         ghop_num = ghop.num,
         ghop_fate = ghop.fate,
         ghop_instar = ghop.instar,
         mass_tube = mass.ghop.tube,
         mass_tube_wet = mass.ghop.wet,
         mass_tube_dry = mass.ghop.dry.MG,
         analytical_fate = analytical.fate,
         IRGA_trt = IRGA.trt,
         IRGA_trt_rep = IRGA.rep,
         IRGA_tube = IRGA.tube) %>% 
  mutate(product_type = "ghop")
prey_g$mass_tube <- prey_g$mass_tube/1000
prey_g$mass_tube_wet <- prey_g$mass_tube_wet/1000
prey_g$mass_tube_dry <- prey_g$mass_tube_dry/1000
# removed the mantid that was fed but abandoned ghop
prey_g <- prey_g[which(!(prey_g$feeding == 1 & prey_g$ghop_num == 25)),]


#==========================================================
# 2. PREDATOR PROPERTIES
#==========================================================

properties_m <- read_csv(here::here("data/1_feeding.obs_mantid.csv")) %>% 
  rename(mass_lid = mass.lid,
         mass_lid_mantid_start = mass.lid.mantid.start,
         mass_lid_mantid_12hr = mass.lid.mantid.12hr,
         eating_notes = eating.notes,
         ooth_tube = ooth.tube.empty,
         ooth_tube_wet = ooth.tube.wet,
         ooth_tube_dry = ooth.tube.dry)

#==========================================================
# 3. PREDATOR PRODUCTS
#==========================================================

pred_m <- read_csv(here::here("data/1_predator_mantid.csv")) %>% 
  rename(product_type = sample.type,
         mass_tube = mass.tube,
         mass_tube_wet = mass.tube.wet,
         mass_tube_dry = mass.tube.dry,
         analytical_fate = analytical.fate,
         IRGA_trt = IRGA.trt,
         IRGA_trt_rep = IRGA.trt.rep,
         IRGA_tube = IRGA.tube)
pred_s <- read_csv(here::here("data/1_predator_spider.csv")) %>% 
  rename(product_type = sample.type,
         mass_tube = mass.tube,
         mass_tube_wet = mass.tube.wet,
         mass_tube_dry = mass.tube.dry,
         analytical_fate = analytical.fate,
         IRGA_trt = IRGA.trt,
         IRGA_trt_rep = IRGA.trt.rep,
         IRGA_tube = IRGA.tube) %>% 
  arrange(feeding, product_type, predatorID)

pred_prod <- full_join(pred_m, pred_s)
pred_prod$mass_tube <- pred_prod$mass_tube/1000
pred_prod$mass_tube_wet <- pred_prod$mass_tube_wet/1000
pred_prod$mass_tube_dry <- pred_prod$mass_tube_dry/1000

#==========================================================
# 4. IRGA DATA
#==========================================================

#=== 
# read tube treatment allocations
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

irga_data1 <- left_join(all_samp2, trt_index, by=c('sampleID'))

#====
# import dry soil data
soil_raw <- read_csv(here::here('data/IRGA prep/00_setup_2-tubes-soil.csv'))
colnames(soil_raw) <- c('trt', 'rep', 'tube', 'target_soil', 'target_tube_soil', 'actual_tube_soil')
soil_raw$rep <- as.integer(soil_raw$rep)

soil_added <- soil_raw %>% 
  mutate(soil_actual = actual_tube_soil - tube) %>% 
  select(trt, rep, soil_actual)

# merge soil with IRGA data
irga_data2 <- left_join(irga_data1, soil_added, by=c('trt', 'rep'))

#====
# fix reference + control tubes label switches in phase 1
switch_list <- read_csv(here::here('data/1_tubes_switch.csv'))
irga_data3 <- switch_tubes(irga_data2, switch_list)

#====
# import treatment added biomass data
prey_raw <- read_csv(here::here('data/IRGA prep/1_trt_mass.csv'))
prey_raw$rep <- as.integer(prey_raw$rep)
colnames(prey_raw) <- c('trt', 'rep', 'tube_soil', 'tube_soil_trt')
sample_added <- prey_raw %>% 
  mutate(trt_added = tube_soil_trt - tube_soil) %>% 
  select(trt, rep, trt_added)
irga_data <- left_join(irga_data3, sample_added, by=c('trt', 'rep')) %>% 
  select(sampleID, trt, rep, tube_num, everything()) %>% 
  rename(tubeID = sampleID)

# later, merge dry ghop mass fed "through" each predator/tube

#==========================================================
# 4. WRITE COMBINED FILE
#==========================================================
#=== write to file, 
write_csv(prey_g, here::here('results/1_prey.csv'))
write_csv(pred_prod, here::here('results/1_products.csv'))
write_csv(properties_m, here::here('results/1_properties.csv'))
write_csv(irga_data, here::here('results/1_irga.csv'))
