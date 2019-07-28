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
# later, merge dry ghop mass fed "through" each predator/tube

prey_g <- read_csv(here::here('results/1_prey.csv'))
p_products <- read_csv(here::here('results/1_products.csv'))
p_properties<- read_csv(here::here('results/1_properties.csv'))
tubes <- readRDS(here::here('results/1_tubes.rds'))
irga <- readRDS(here::here('results/1_irga.rds'))

# calc + trim predator properties
p_properties2 <- p_properties %>% 
  mutate(pred_wet_start = mass_lid_mantid_start - mass_lid) %>% 
  select(-mass_lid_mantid_start, -mass_lid, -observations, everything(), observations)


#===
# rename columns, calculate masses
prey_g2 <- prey_g %>% 
  rename(tubeID = IRGA_tube,
         mass_dry = ghop_dry,
         mass_wet = ghop_wet) %>% 
  mutate(product_type = 'ghop') %>% 
  select(-mass_tube, -mass_tube_wet, -mass_tube_dry, -IRGA_trt, -IRGA_trt_rep, -observations)

prey_g3 <- calc_dry(prey_g2)

p_products2 <- p_products %>% 
  rename(tubeID = IRGA_tube) %>% 
  mutate(mass_dry = mass_tube_dry - mass_tube,
         mass_wet = mass_tube_wet - mass_tube) %>% 
  select(-mass_tube, -mass_tube_dry, -mass_tube_wet, -IRGA_trt, -IRGA_trt_rep, -observations)

p_products3 <- p_products2 %>% 
  mutate(water_mass = mass_wet - mass_dry,
         water_percent = water_mass / mass_wet)

#====
# calculate product_type by group type
feces_water <- p_products3 %>% 
  filter(product_type == 'feces') %>% 
  select(sampleID, predatorID, tubeID, water_percent) %>% 
  na.omit()

feces_avg <- mean(feces_water$water_percent)
feces_sd <- sd(feces_water$water_percent)
feces_cv <- feces_sd/feces_avg
feces_se <- se(feces_water$water_percent)
cat("average feces water content:", feces_avg*100, "% +-", feces_se*100)

remains_water <- p_products3 %>% 
  filter(product_type == 'remains') %>% 
  select(sampleID, predatorID, tubeID , water_percent) %>% 
  na.omit()

remains_avg <- mean(remains_water$water_percent)
remains_sd <- sd(remains_water$water_percent)
remains_cv <- remains_sd/remains_avg
remains_se <- se(remains_water$water_percent)
cat("average remains water content:", remains_avg*100, "% +-", remains_se*100)

#====
# check the differences between dry values + inferred dry values
compare_dry <- prey_g3 %>% 
  filter(is.na(mass_dry) == FALSE) %>% 
  select(sampleID, feeding, tubeID, mass_dry, infer_dry) %>% 
  mutate(mass_dry_mg = mass_dry*1000,
         dry_diff = infer_dry - mass_dry,
         dry_diff_mg = dry_diff*1000)
compare_avg <-  mean(compare_dry$dry_diff)
compare_se <- se(compare_dry$dry_diff)
cat("average diff betwn dry mass + inferred dry:", compare_avg, "% +-", remains_se)

plot(compare_dry$mass_dry, compare_dry$infer_dry)
qqplot(compare_dry$mass_dry, compare_dry$infer_dry)

# prepare ghop data to retain columns before the merg
ghop2pred <- prey_g3 %>% 
  select(sampleID, ghop_fate, predatorID, feeding, analytical_fate, mass_dry, infer_dry) %>% 
  rename(tubeID = sampleID,
         ghop_origin_dry = mass_dry,
         ghop_origin_infer = infer_dry)

# merge ghop origin data with predator products
p_products4 <- left_join(p_products3, ghop2pred, by=c("predatorID", "feeding"))

#===
# merge all ghop, predator product, and predato properties into the IRGA dataset
  
tubes_no.p <- tubes %>% 
  filter(product_type != "ghop") %>% 
  rename(tubeID = sampleID) %>% 
  unnest(product_type)

i_products <- p_products4 %>% 
  filter(analytical_fate == "IRGA") %>% 
  select(-mass_wet, -water_mass, -water_percent) %>% 
  left_join(tubes_no.p)

tubes_g <- tubes %>%
  filter(product_type == "ghop") %>% 
  unnest(product_type) 
  
i_ghops <- ghop2pred %>% 
  filter(analytical_fate == "IRGA") %>% 
  rename(productID = ghop_originID,
         tubeID = sampleID) %>% 
  select(-ghop_fate) %>% 
  left_join(tubes_g, by = "productID") %>% 
  select(analytical_fate, productID, product_type, tube_num,
         tubeID, trt, rep, ghop_origin_dry, ghop_origin_infer, predatorID, 
         feeding, soil_atual, trt_added_mass, everything())



i_inputs <- rbind(i_products, i_ghops) %>% 
  select(-ghop_origin_dry)

#===
# merge ghop dry data with IRGA data
irga <- irga %>% 
  rename(tubeID = sampleID)

irga_input <- left_join(irga, i_inputs, by = "tubeID")
