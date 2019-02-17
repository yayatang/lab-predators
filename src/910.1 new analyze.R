library(tidyverse)
library(lubridate)
library(purrr)

data_1raw <- read_csv(here::here('data/IRGA/clean/clean_all_samp.csv'))
data_1raw$trt <- as.factor(data_1raw$trt)
all_trt <- unique(data_1raw$trt)

# known standard gas CO2 ppm value
std_ppm <- 1997
# amount the ppm adjusts with each 5ml co2-free air injection
inj_constant <- 1.096
# converting to CO2 micromoles, then micrograms-C, divide for ppm->microM
co2c_const <- 0.05 * 12.0011 / 22.4

# gross value of true ppm
data_orig <- data_1raw
data_orig$samp_by_std <- (std_ppm * (data_orig$integral/data_orig$std_vector))*(inj_constant^data_orig$inject_num)

# calculate _final_ CO2 ppm rate per day, in units CO2-C (microg C/g/h)
# dsoil is in g, merge from table later
data_orig$actual_dsoil <- 5
data_orig$samp_co2_gross <- data_orig$samp_by_std * co2c_const / data_orig$actual_dsoil
data_orig$samp_co2_rate <- data_orig$samp_co2_gross / data_orig$incub_hours
data_orig <- mutate(data_orig, samp_co2_perday = samp_co2_rate*24)

data_orig <- data_orig %>% 
  select('phase', 'incub_count', 'sampleID', everything())

# this is just for knowing when to cut off the control tubes

trt_data <- data_orig
all_summ <- tibble()

for (i in 1:length(all_trt)){
  # summ_trtmt <- function(trt_data, all_trt[[i]]){
  summ_data <- trt_data %>%
    filter(trt == all_trt[[i]]) %>%
    select(phase, incub_count, rep, samp_co2_perday) %>%
    group_by(phase, incub_count)%>%
    summarise_at(vars(samp_co2_perday),funs(mean(., na.rm = TRUE), se))
  # }
  
  # multi_means <- function(summarized, trt){
  mean_var <- paste(all_trt[[i]], "mean", sep="_")
  se_var <- paste(all_trt[[i]], "se", sep="_")
  summ_data <- summ_data %>%
    mutate(!!mean_var := mean,
           !!se_var := se) %>%
    select(-mean, -se)
  
  all_summ <- bind_rows(summ_data)
}

# colSums(is.na(all_data))


net_data <- merge(data_orig, ctrl_data_unsummarised, by=c('incub_count')) %>%
  mutate(diff_perday_C = samp_co2_perday - ctrl_avg)

net_data <- diff_data_unsummarised %>%
  mutate(trt_ID=paste0(as.character(trt)))

irga_days <- data.frame(incub_count = unique(diff_data_unsummarised$incub_count)) %>% arrange(incub_count)
trt_vec <- sort(as.character(unique(diff_data_unsummarised$trt)))
tube_IDs <- unique(diff_data_unsummarised$sampleID)
tube_labels <- data.frame(sampleID = as.factor(tube_IDs),
                          trt = substr(tube_IDs, 1, nchar(tube_IDs)-3)) %>%
  arrange(trt)

max_days <- max(diff_data_unsummarised$incub_count)
days_all <- seq(1, max_days)

