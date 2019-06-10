# =========================
# Import and clean IRGA data from datasheets
# June 2019
# =========================

library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(here)
library(zoo)
library(RColorBrewer)

# setwd(here::here())
source(here::here('src/yaya_fxns.R'))

# Import all tables and flatten all samplings into one data frame------
# === read all files in the directory + flatten ===
# datasheet location for all phases csvs
file_list <- list.files(path=here::here("data/IRGA/raw"), pattern="*.csv", full.names = TRUE)
all_samp <- lapply(file_list, get_info)

all_samp2 <- bind_rows(all_samp) %>%
  arrange(phase, incub_count)

trt_index <- read_csv(here::here('data/IRGA prep/00_trt_index.csv')) %>%
  select(-rep)

all_data <- left_join(all_samp2, trt_index, by=c('sampleID'))
# all_data$sampleID <- as.factor(all_data$sampleID)

switch_list <- read_csv(here::here('data/1_tubes_switch.csv'))
switch_phase <- 1

all_data2 <- switch_tubes(all_data, switch_phase, switch_list)

write_csv(all_data, here::here('results/clean_all_samp.csv'))

# ============================================================================
