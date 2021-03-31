# =========================
# Merge IRGA data with prep data, and format factors
# June 2019
# =========================
library(here)
library(tidyverse)
source(here::here('src/yaya_fxns.r'))

# objectives:
# match ghops to mantids/ultimately IRGA tubes
# merge w/ predator data
# later, merge dry ghop mass fed 'through' each predator/tube

prey_g <- read_csv(here::here('results/1_prey.csv'))
p_products <- read_csv(here::here('results/1_products.csv'))
p_properties<- read_csv(here::here('results/1_properties.csv'))
tubes <- readRDS(here::here('results/1_tubes.rds'))
irga <- readRDS(here::here('results/1_irga.rds'))

#==========================================================
# 1. STANDARDIZE VARIABLE NAMES + CLEAR UNNECC VARS
#==========================================================
data_tubes <- tubes %>% 
  rename(tubeID = sampleID) %>% 
  unnest_legacy()
all_tubeID <- unique(data_tubes[,c('tube_num', 'tubeID')])

data_prey <- prey_g %>% 
  rename(tubeID = IRGA_tube,
         productID = sampleID,
         mass_wet = ghop_wet,
         mass_dry = ghop_dry) %>% 
  mutate(product_type = 'ghop') %>% 
  select(-IRGA_trt, -IRGA_trt_rep, -mass_tube, -mass_tube_wet, -mass_tube_dry) %>% 
  left_join(all_tubeID)
data_prey <- select(data_prey, tube_num, tubeID, predatorID, productID, everything())

data_pred <- p_properties %>% 
  mutate(mantid_mass_start = mass_lid_mantid_start - mass_lid,
         mantid_mass_ate = mass_lid_mantid_12hr - mass_lid) %>% 
  select(-mass_lid, -mass_lid_mantid_start, -mass_lid_mantid_12hr)

#  this is for all products, including non-IRGA
data_products <- p_products %>% 
  rename(tubeID = IRGA_tube,
         productID = sampleID) %>% 
  mutate(mass_wet = mass_tube_wet - mass_tube,
         mass_dry = mass_tube_dry - mass_tube) %>% 
  select(-IRGA_trt,-IRGA_trt_rep, -mass_tube, -mass_tube_dry, -mass_tube_wet) %>%
  left_join(all_tubeID)
data_products <- select(data_products, tube_num, tubeID, predatorID, productID, everything())

data_irga <- irga %>% 
  rename(tubeID = sampleID)

#====
#FOR CHECKING COLUMN NAMES
# a <- colnames(capture_silk)
# b <- colnames(data_products)
# setdiff(a,b)
# intersect(a,b)

#==========================================================
# 2. TRANSFORM AND GENERATE NEW VARS for PRODUCT ANALYSIS
#==========================================================

#====
#update products data to include capture silk
capture_silk <- data_tubes %>%
  filter(product_type == 'capture silk') %>%
  mutate(productID = paste0(productID, '-C'),
         analytical_fate = 'IRGA',
         ghop_fate = 'spider',
         mass_wet = trt_added_mass+0.0001, #silk has no moisture, need vals for fxn
         mass_dry = trt_added_mass,# dry data from tubes
         observations = NA) %>%
  select(-soil_actual, -trt_added_mass, -trt, -rep)
capture_silk <- select(capture_silk, tube_num, tubeID, predatorID, productID, everything())


# combine IRGA tube capture silk data with products
data_products2 <- data_products %>%
  left_join(all_tubeID) %>% 
  rbind(capture_silk)
data_products2[which(data_products2$mass_dry < 0),] <- NA

#====
# for calculating percent water content
# calculate percent water content to get inferrred values
dry_prey <- calc_dry(data_prey)
data_prey2 <- dry_prey[[1]]
prey_water <- dry_prey[[2]]
print(prey_water)

# calculating predator products water content
dry_prod <- calc_dry(data_products2)
data_products3 <- dry_prod[[1]]
products_water <- dry_prod[[2]]
print(products_water)

#====
# # UNNECESSARY
# # check the differences between dry values + inferred dry values
# compare_dry <- data_prey2 %>% 
#   filter(is.na(mass_dry) == FALSE) %>% 
#   select(tubeID, productID, feeding, mass_dry, infer_dry) %>% 
#   mutate(mass_dry_mg = mass_dry*1000,
#          dry_diff = infer_dry - mass_dry,
#          dry_diff_mg = dry_diff*1000)
# compare_avg <-  mean(compare_dry$dry_diff_mg)
# compare_se <- se(compare_dry$dry_diff_mg)
# cat('average diff betwn dry mass + inferred dry:', compare_avg, 'mg +-', compare_se)
# 
# plot(compare_dry$mass_dry, compare_dry$infer_dry)
# qqplot(compare_dry$mass_dry, compare_dry$infer_dry)

#====
# prepare ghop data to retain columns before the merg
prey4pred <- data_prey2 %>% 
  select(productID, predatorID, feeding, infer_dry) %>%
  rename(originID = productID,
         origin_infer = infer_dry)

# merge ghop inferred dry data with predator products
data_products4 <- left_join(data_products3, prey4pred, by=c('predatorID','feeding')) %>% 
  select(-observations, everything(), observations)
data_products4 <- data_products4[rowSums(is.na(data_products4)) != ncol(data_products4),]

#==========================================================
# 3. MAKING IRGA ONLY DATA
#==========================================================
#====
# merge all ghop, predator product, and predator properties into the IRGA dataset

# filter tube lookup for only products. lookup is good for tube metadata
# lookup_prods <- data_tubes %>% 
#   filter(product_type != 'ghop',
#          product_type != 'capture silk') %>%
#   select(-trt, -rep, -productID)

i_products <- data_products4 %>% 
  filter(analytical_fate == 'IRGA') %>% 
  select(-water_mass, -water_percent, -percent_mean) #%>% 
  # full_join(lookup_prods)

# # lookup info for ghop tubes
# data_tubes_prey <- data_tubes %>%
#   filter(product_type == 'ghop')

i_ghops <- data_prey2 %>% 
  filter(analytical_fate == 'IRGA') %>% 
  # left_join(data_tubes_prey) %>% 
  select(-ghop_num, -ghop_instar, -water_mass, -water_percent, -percent_mean, -infer_dry) %>% 
  mutate(ghop_fate = 'ghop',
         originID = productID,
         origin_infer = NA)

i_inputs <- rbind(i_products, i_ghops)

# create variables for calculations
i_inputs$effective_dry <- ifelse(is.na(i_inputs$mass_dry), i_inputs$origin_infer, i_inputs$mass_dry)
i_inputs$origin_dry <- ifelse(is.na(i_inputs$origin_infer), i_inputs$mass_dry, i_inputs$origin_infer)

i_inputs_summed <- i_inputs %>%
  group_by(tube_num) %>% 
  summarize(summed_inputs = sum(effective_dry))

i_inputs_netto <- i_inputs %>% 
  select(tube_num, tubeID, ghop_fate, originID, origin_dry) %>% 
  unique() %>% 
  left_join(i_inputs_summed)

#===
# merge ghop dry data with IRGA data
data_irga <- irga %>% 
  rename(tubeID = sampleID)

irga_input <- left_join(data_irga, i_inputs_netto)
amend_diff <- irga_input$trt_added_mass - irga_input$summed_inputs

#----
## export real IRGA data to RDS
# saveRDS(irga_input, here::here('results/2_irga.rds'))
