#==========================================================
# Import and clean IRGA data from datasheets
# June 2019
#==========================================================

library(here)
library(tidyverse)
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
         IRGA_tube = IRGA.tube)
# calculate wet and dry weights in grams
prey_g$ghop_wet <- (prey_g$mass_tube_wet - prey_g$mass_tube)/1000
prey_g$ghop_dry <- (prey_g$mass_tube_dry - prey_g$mass_tube)/1000

# considered removing mantid that was fed but abandoned ghop
# prey_g <- prey_g[which(!(prey_g$feeding == 1 & prey_g$ghop_num == 25)),]


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

#***get spider feeding data!***
# properties_s <- read_csv(here::here())

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
         IRGA_tube = IRGA.tube) %>% 
  mutate(ghop_fate = "mantid")
pred_s <- read_csv(here::here("data/1_predator_spider.csv")) %>% 
  rename(product_type = sample.type,
         mass_tube = mass.tube,
         mass_tube_wet = mass.tube.wet,
         mass_tube_dry = mass.tube.dry,
         analytical_fate = analytical.fate,
         IRGA_trt = IRGA.trt,
         IRGA_trt_rep = IRGA.trt.rep,
         IRGA_tube = IRGA.tube) %>% 
  mutate(ghop_fate = "spider") %>% 
  arrange(feeding, product_type, predatorID)

pred_prod <- full_join(pred_m, pred_s)
pred_prod$mass_tube <- pred_prod$mass_tube/1000
pred_prod$mass_tube_wet <- pred_prod$mass_tube_wet/1000
pred_prod$mass_tube_dry <- pred_prod$mass_tube_dry/1000

#==========================================================
# 4. IRGA LOOKUP TABLE
#==========================================================

#=== 
# read tube treatment allocations
tube_trts <- read_csv(here::here("data/IRGA prep/0_irga_treatments_by.tube.csv"))
# format tubes receiving multiple products into list
tube_trts2 <- tube_trts %>% 
  rename(productID = animal_feeding,
         tube_num = tubeID) %>% 
  mutate(product_type = str_split(product_type, ";"))
tube_trts2$rep <- as.numeric(tube_trts2$rep)

#====
# import dry soil data, rename col
soil_raw <- read_csv(here::here('data/IRGA prep/00_setup_2-tubes-soil.csv'))
colnames(soil_raw) <- c('tube_num', 'mass_tube', 'target_soil', 'target_tube_soil', 'actual_tube_soil')

# remove tube values 
soil_added <- soil_raw %>% 
  mutate(soil_actual = actual_tube_soil - mass_tube) %>% 
  select(tube_num, soil_actual)

lookup1 <- left_join(tube_trts2, soil_added, by=c('tube_num'))

#===
#import amendment data
prey_raw <- read_csv(here::here('data/IRGA prep/1_trt_mass.csv'))
colnames(prey_raw) <- c('tube_num', 'trt', 'rep', 'tube_soil', 'tube_soil_trt')

sample_added <- prey_raw %>% 
  mutate(trt_added_mass = tube_soil_trt - tube_soil) %>% 
  select(trt, rep, trt_added_mass)

lookup2 <- left_join(lookup1, sample_added)

#==========================================================
# 5. IRGA DAILY DATA, merged with lookup
#==========================================================
#====
# read all files in the directory + flatten, add labels for treatments
file_list <- list.files(path=here::here("data/IRGA/"), pattern="*.csv", full.names = TRUE)
all_samp <- lapply(file_list, get_info)

# # TROUBLESHOOTING
# file_path <- here::here('data/IRGA/PP-IRGA 1-08.csv')
# all_samp <- get_info(file_path)

all_samp2 <- bind_rows(all_samp) %>%
  arrange(phase, incub_count) %>% 
  select(-sampleID, -rep)

# merge with lookup table, which includes all the prep data per tube
all_samp3 <- left_join(all_samp2, lookup2, by="tube_num") %>% 
  select(sampleID, trt, rep, tube_num, rack, position, everything())

# fix reference + control tubes label switches
# requires vars on phase, 
switch_list <- read_csv(here::here('data/1_tubes_switch.csv'))
irga_daily <- switch_tubes(all_samp3, switch_list)

lookup_names <- colnames(lookup2)
tube_lookup <- unique(irga_daily[lookup_names])

#==========================================================
# 6. WRITE COMBINED FILE
#==========================================================
# # #=== write to file,
# write_csv(prey_g, here::here('results/1_prey.csv'))
# write_csv(pred_prod, here::here('results/1_products.csv'))
# write_csv(properties_m, here::here('results/1_properties.csv'))
# saveRDS(irga_daily, here::here('results/1_irga.rds'))
# saveRDS(tube_lookup, here::here('results/1_tubes.rds'))