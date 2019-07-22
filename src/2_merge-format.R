# =========================
# Merge IRGA data with prep data, and format factors
# June 2019
# =========================
library(here)
library(dplyr)
library(readr)

source(here::here('src/yaya_fxns.r'))

prey_g <- read_csv(here::here('results/1_prey.csv'))
p_products <- read_csv(here::here('results/1_products.csv'))
p_properties<- read_csv(here::here('results/1_properties.csv'))
irga <- read_csv(here::here('results/1_irga.csv'))

#===
# calc dry masses of all samples & convert to grams
prey_g$mass_dry <- prey_g$mass_tube_dry - prey_g$mass_tube
prey_g$mass_wet <- prey_g$mass_tube_wet - prey_g$mass_tube

p_products$mass_dry <- p_products$mass_tube_dry - p_products$mass_tube

p_properties$mass_wet_start <- p_properties$mass_lid_mantid_start - p_properties$mass_lid
p_properties

# match ghops to mantids/ultimately IRGA tubes
# merge w/ predator data


#===
# prepare original ghop masses to add to IRGA data

i_products <- p_products %>% 
  filter(analytical_fate == "IRGA") %>% 
  select(sampleID, predatorID, feeding, mass_dry, analytical_fate, IRGA_trt, IRGA_trt_rep, IRGA_tube)

i_ghops <- calc_dry(prey_g)

# i_ghops <- i_ghops %>% 
  # select(sampleID, feeding, mass_dry, analytical_fate, IRGA_trt, IRGA_trt_rep,  IRGA_tube, ghop_infer_origin)



i_inputs <- full_join(i_ghops, i_products) %>% 
  group_by(IRGA_tube) %>% 
  summarize(amend_all = sum(mass_dry, na.rm=TRUE)) %>%  #convert to grams
  ungroup() 

#===
# merge ghop dry data with IRGA data
irga_input <- left_join(irga, i_inputs, by = c("tubeID" = "IRGA_tube"))


