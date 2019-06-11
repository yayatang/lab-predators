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
# switch_phase <- 1

# all_data3 <- switch_tubes(all_data2, switch_phase, switch_list)

#==========TROUBLE SHOOTING, eventually move to fxn
all_tubes <- all_data2
# i <- 4

# all_tubes_p1 <- filter(all_data2, phase==1)
# all_tubes_p2 <- filter(all_data2, phase==2)

for (i in 1:nrow(switch_list)){
  tube1 <- as.character(switch_list[i, 1])
  tube2 <- as.character(switch_list[i, 2])
  
  tube1_meta <- tibble(sampleID = tube1, trt = substr(tube1, 1, 1), rep = as.numeric(substr(tube1, 3, 4)))
  tube2_meta <- tibble(sampleID = tube2, trt = substr(tube2, 1, 1), rep = as.numeric(substr(tube2, 3, 4)))
  
  # find corresponding tube num
  tube1_meta$tube_num <- all_tubes[which(all_tubes$sampleID==tube1 & all_tubes$phase==1),]$tube_num[1]
  tube2_meta$tube_num <- all_tubes[which(all_tubes$sampleID==tube2 & all_tubes$phase==1),]$tube_num[1]
  
  # phase 1, switch ALL tube info, phase 2 switch all BUT sampleID info
  all_tubes[which(all_tubes$tube_num==tube1_meta$tube_num & all_tubes$phase == 1),]$sampleID <- tube2_meta$sampleID
  all_tubes[which(all_tubes$tube_num==tube2_meta$tube_num & all_tubes$phase == 1),]$sampleID <- tube1_meta$sampleID
  all_tubes[which(all_tubes$tube_num==tube1_meta$tube_num),]$trt <- tube2_meta$trt
  all_tubes[which(all_tubes$tube_num==tube2_meta$tube_num),]$trt <- tube1_meta$trt
  all_tubes[which(all_tubes$tube_num==tube1_meta$tube_num),]$rep <- tube2_meta$rep
  all_tubes[which(all_tubes$tube_num==tube2_meta$tube_num),]$rep <- tube1_meta$rep
}

all_data3 <- all_tubes
#========================

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
