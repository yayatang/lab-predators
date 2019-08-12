library(tidyverse)
library(readxl)

siel1 <- read_excel(here::here('data/SIEL data/YT1670-1_TotalCN_Rep, SIEL SO#1670 TRM.xls'), skip=2)
siel1 <- siel1[,colSums(is.na(siel1))<nrow(siel1)]

colnames(siel1) <- c('sampleID', 'mass_mg', 'percentN', 'percentC', 'CN_ratio')

siel2 <- read_excel(here::here('data/SIEL data/YT1670-2_TotalCN_Rep, SIEL SO#1670 TRM.xls'), skip=2)
siel2 <- siel2[,colSums(is.na(siel2))<nrow(siel2)]

colnames(siel2) <- c('sampleID', 'mass_mg', 'percentN', 'percentC', 'CN_ratio')

pp_raw <- bind_rows(siel1, siel2)

# samp_data <- read_xlsx(here::here('data/dry samples for CN analysis 2019.07.30.xlsx'), na='NA')
# samp_data <- samp_data[,2:6]
samp_data <- read_csv(here::here('data/CN_analysis_samples.csv'))
colnames(samp_data) <- c('sampleID', 'feeding', 'predatorID', 'predator_type', 'sample_type')

# samp_data[which(samp_data$sampleID=='SR33F1',] <- c('')

pp_mysamp <- pp_raw %>% 
  filter(sampleID != 'spinach') %>% 
  left_join(samp_data)


