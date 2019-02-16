# ======= header ============------------------------

# Clean + analyze IRGA data from datasheets
# Feb 2019

library(dplyr)
library(ggplot2)
library(lubridate)
library(here)
library(zoo)
library(RColorBrewer)

check_midnight <- function(test_date){
  new_dates <- if_else(hour(test_date) < 6, test_date + days(1), test_date)
}

# Import all tables and flatten all samplings into one data frame---------
# read all files in the directory + flatten
# file_list <- list.files(path=here::here("data/IRGA/raw"), pattern="*.csv", full.names = TRUE)

file_list <- here::here("data/IRGA/raw/PP-IRGA 1-8.csv") # debugging
fileloc <- file_list
# meta_data <- import_meta(fileloc)

# === imports meta-data into a clean table ===
meta_all <- scan(fileloc, nlines = 2, what = character(), sep = ",")

meta_raw <- meta_all[meta_all != ""] # remove empty cells
meta_raw <- matrix(meta_raw, nrow = 2, ncol = 7)
colnames(meta_raw) <- c(
  "rack_start", "sampling_num", "incub_count", "start_day", "day_flush",
  "day_msre", "std_ppm")
phase <- substr(meta_raw[1,2], 11,11)

meta_raw <- meta_raw[-1, ] # delete imported row of names
meta <- as_tibble(t(meta_raw))

meta$incub_count <- as.numeric(as.character(meta$incub_count))
meta$start_day <- dmy(meta$start_day)
meta$day_flush <- dmy(meta$day_flush)
meta$day_msre <- dmy(meta$day_msre)
meta$std_ppm <- as.numeric(meta$std_ppm)
meta$phase <- as.numeric(phase)

# ======================================================================================
# === imports samples and standard data ===
# reads file, removes empty and NA cols
raw_sampstd <- read.csv(fileloc, skip = 2, header = TRUE, na.strings = c(".", " ", "", "NA"),
                        stringsAsFactors = FALSE)
arr_sampstd_1raw <- raw_sampstd[!is.na(names(raw_sampstd))]
arr_sampstd_2whole <- arr_sampstd_1raw[colSums(!is.na(arr_sampstd_1raw)) > 0]
arr_sampstd_3clean <- as_tibble(arr_sampstd_2whole)

cols_keep <- c(
  "sampleID", "tube.num", "rep", "rack", "position", "time.flushed",
  "time.sampled", "integral", "inject.num", "std.int.time", "std.integral")
arr_sampstd <- arr_sampstd_3clean[cols_keep]
colnames(arr_sampstd) <- c(
  "sampleID", "tube_num", "rep", "rack", "position", "time_flush", "time_msre", "integral",
  "inject_num", "std_time_int", "std_integral")
# write.csv(arr_sampstd, file=here::here('csv1-5.csv'))


# === splits standards from sample data ===
std_0data <- select(arr_sampstd, std_time_int, std_integral)
std_0data$date_std <- ymd_hms(paste(meta$day_msre, std_0data$std_time_int))
std_0data$date_std <- check_midnight(std_0data$date_std)

# ========FUTURE STANDARD GROUPING AND INTERPOLATION FXN
# groups and summarizes true standard data
# quick and dirty way to average the last three values
# ***does not test how close the std vals are***
std_1raw <- std_0data %>%
  mutate(to_remove = is.na(std_integral),
         std_group = cumsum(is.na(std_integral))) %>%
  filter(to_remove == FALSE) %>%
  select(-to_remove)

# group standards by when they taken, and "rank" in order
# put in descending order to select the last three measurements
std_2grouped <- std_1raw %>%
  group_by(std_group) %>%
  arrange(desc(std_time_int)) %>% 
  mutate(rank = cumsum(!is.na(std_group)))

# remove standards that weren't the last three values
std_3vec <- std_2grouped %>%
  filter(rank < 4) %>%
  group_by(std_group) %>%
  summarize( mean_time = date_std[rank==2], 
             mean_integral = mean(std_integral)) %>%
  select(-std_group)

std_3vec$mean_time <- round_date(std_3vec$mean_time, unit='minute')

# ==========interpolating standards===========================
# create standard vector by minute from start to finish, w interpolated vals
std_min <- min(std_3vec$mean_time)
std_max <- max(std_3vec$mean_time)
filled <- seq.POSIXt(std_min, std_max, by='min')
filled.df <- data.frame(mean_time = filled)

std_all <- left_join(filled.df, std_3vec, by='mean_time')
std_all$std_vector <- na.approx(std_all$mean_int)
std_all <- std_all %>%
  # rename(std_time=mean_time) %>%
  select(-mean_integral)

#=======================================================================
# === makes clean imported sample data ===
# add the day== 
arr_samp <- arr_sampstd %>%
  mutate(date_flush = ymd_hms(paste(meta$day_flush, arr_sampstd$time_flush)),
         date_msre = ymd_hms(paste(meta$day_msre, arr_sampstd$time_msre))) %>%
  select(-std_time_int, -std_integral, -time_flush, -time_msre) %>%
  na.omit() 

#check all var types...
arr_samp$date_flush <- check_midnight(arr_samp$date_flush)
arr_samp$date_msre <- check_midnight(arr_samp$date_msre)
arr_samp$sampleID <- as.factor(as.character(arr_samp$sampleID))
arr_samp$integral <- as.numeric(arr_samp$integral)
arr_samp$inject_num <- as.numeric(arr_samp$inject_num)
inj_idx <- (arr_samp$inject_num != 0)
arr_samp$inject_num[!inj_idx] <- 1

arr_samp$phase <- meta$phase
arr_samp$incub_count <- meta$incub_count

arr_samp$incub_hours <- arr_samp$date_msre - arr_samp$date_flush
# maybe need something here to make sure time diff is in hours
arr_samp$date_for_std <- round_date(arr_samp$date_msre, unit='minute')

# ==============================\==============================================
# merge samples with standards + soil data (get later)
master_sampling <- left_join(arr_samp, std_all, by = c('date_for_std'='mean_time'))
# master_sampling <- master_sampling %>%
  # select(sampleID, phase, incub_count, date_msre, std_vector, integral, inject_num, incub_time)
  # select(-tube_num, -position, -rack)

write.csv(master_sampling, file=here::here('indiv_samp.csv'))


# ============================================================================
# ============================================================================