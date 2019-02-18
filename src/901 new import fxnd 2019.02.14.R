# ======= header ============------------------------

# For importing and cleaning IRGA data from datasheets
# Feb 2019

library(dplyr)
library(ggplot2)
library(lubridate)
library(here)
library(zoo)
library(RColorBrewer)

setwd(here::here())
source(here::here('src/yaya_fxns.R'))

# Import all tables and flatten all samplings into one data frame---------
# read all files in the directory + flatten
file_list <- list.files(path=here::here("data/IRGA/raw"), pattern="*.csv", full.names = TRUE)
all_samp <- lapply(file_list, get_info)

all_samp2 <- bind_rows(all_samp) %>%
  arrange(phase, incub_count) %>%
  select(-rep)

switch_list <- read_csv(here::here('data/tubes_switch.csv'))
switch_phase <- 1
all_samp3 <- all_samp2

for (i in 1:length(switch_list)){
  tube1 <- as.character(switch_list[i, 1])
  tube2 <- as.character(switch_list[i, 2])
  
  all_samp3[which(all_samp3$sampleID==tube1 & all_samp3$phase==switch_phase),]$sampleID <- 'dummy1'
  all_samp3[which(all_samp3$sampleID==tube2 & all_samp3$phase==switch_phase),]$sampleID <- 'dummy2'
  
  all_samp3[which(all_samp3$sampleID=='dummy1'),]$sampleID <- tube2
  all_samp3[which(all_samp3$sampleID=='dummy2'),]$sampleID <- tube1
}

trt_index <- read.csv(here::here('data/IRGA prep/00_trt_index.csv'))
all_data <- left_join(all_samp3, trt_index, by=c('sampleID'))


write_csv(all_data, here::here('data/IRGA/clean/clean_all_samp.csv'))

# ============================================================================
