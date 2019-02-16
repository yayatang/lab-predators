# ======= header ============------------------------

# Clean + analyze IRGA data from datasheets
# August 2018

# ***to deal with switching the treatments:
# ***the line after a file is imported (or as soon as
# *** "sampleID" column is named) do the necessary switching

# Set working directory and clear workspace----------------------------------------

library(dplyr)
library(lubridate)
library(zoo)
library(here)
setwd(here::here())

# @@@ FUNCTION: to read and return data, interpolates standards ------------------
get_info <- function(fileloc){
  
  # fileloc <- here::here("data/IRGA","PP-IRGA 1-5.csv") # for DEBUGGING
  # === first import META DATA ===
  
  # extracts first line as meta data for one day's values
  meta_raw <- scan(fileloc, nlines=2, what=character(), sep=',')
  # meta_raw[[3]] # this is the incubation day/count
  
  # === cleans and arranges meta-data into a table ===
  meta_raw <- meta_raw[meta_raw != '']
  meta_raw <- matrix(meta_raw, nrow=2, ncol=7)
  colnames(meta_raw) <- c('rack_start', 'sampling_num', 'incub_count', 'start_day', 'day_flush', 'day_msre', 'std_ppm')
  meta_raw <- meta_raw[-1,]
  meta <- as.data.frame(t(meta_raw))
  
  # === ensures all days and numbers entered are in a standard format ===
  meta$incub_count <- as.numeric(as.character(meta$incub_count))
  meta$start_day <- dmy(meta$start_day)
  meta$day_flush <- dmy(meta$day_flush)
  meta$day_msre <- dmy(meta$day_msre)
  meta$std_ppm <- as.numeric(as.character(meta$std_ppm))
  
  # === checks the incubation day value ===
  # day1 <- strptime(meta$start_day, format='%Y-%m-%d')
  # day2 <- strptime(meta$day_flush, format='%Y-%m-%d')
  meta$incub_count_check <- round(difftime(meta$day_flush, meta$start_day, units="days")+ 1)
  
  # === imports sample integral data ===
  samp <- read.csv(fileloc, skip=1, header=TRUE, na.strings=c('.',' ','', 'NA'), stringsAsFactors=FALSE)
  colnames(samp) <- c('sampleID','tube_num', 'rep', 'rack', 'position', 'time_flush', 'time_msre', 'integral', 
                      'inject_num', 'std_time_int', 'std_integral', 'inj_redo', 'inj_2nd_int', 'notes')
  samp <- samp[colSums(!is.na(samp)) > 0]
  samp <- samp[-1,]
  
  # === many lines to prep for converting dates into the correct format ===
  samp$day_flush <- ymd(meta$day_flush)
  samp$day_msre <- ymd(meta$day_msre)
  samp$time_msre <- as.character(samp$time_msre)
  samp$time_flush <- as.character(samp$time_flush)
  samp$incub_count <- meta$incub_count_check

  # === make everything into a date = day + time ===
  samp$date_flush <- paste(samp$day_flush, samp$time_flush)
  samp$date_flush <- ymd_hms(samp$date_flush)
  samp$date_flush <- round_date(samp$date_flush, unit='minute')
  
  samp$date_msre <- ymd_hms(paste(samp$day_msre, samp$time_msre))
  samp$date_msre <- round_date(samp$date_msre, unit='minute')
  
  
  # === vectorize calibration values/standards ===
  # standards are separated into their own list
  # quick and dirty way to average the last three values, does not test how close the std vals are
  std_raw <- select(samp, std_time_int, std_integral) %>%
    mutate(to_remove = is.na(std_time_int),  # marking which rows to remove (i.e. NAs)
           std_group = cumsum(is.na(std_time_int))) %>% # groups of standards have the same number
    filter(to_remove==FALSE)
  std_raw$std_time_int <- round_date(ymd_hms(paste(samp$day_flush[1], std_raw$std_time_int)), unit='minute')
  
  # convert integrals to the right format
  std_raw$std_integral <- as.numeric(as.character(std_raw$std_integral))
  
  # group standards by when they taken, and "rank" in order
  std_grouped <- std_raw %>%
    group_by(std_group) %>%
    arrange(desc(std_time_int)) %>% # put in descending order to select the last three measurements
    mutate(rank = cumsum(!is.na(std_group)))
    
  # remove standards that weren't the last three values
  std_vec <- std_grouped %>%
    filter(rank < 4) %>%
    group_by(std_group) %>%
    summarize(mean_int = mean(std_integral), mean_time = std_time_int[rank==2]) %>%
    select(-std_group)
  std_vec <-  std_vec[,c(2,1)] #reorder to 
  
  # round all times to the minute
  std_vec$mean_time <- round_date(std_vec$mean_time, unit='minute')
  
  # std_vec2 <- zoo(std_vec) %>%
    # complete(mean_time = full_seq(mean+time, period =1), fill = lis)
  
  # create standard vector for every minute from start to finish time, with interpolated vals
  std_min <- min(std_vec$mean_time)
  std_max <- max(std_vec$mean_time)
  filled <- seq.POSIXt(std_min, std_max, by='min')
  filled.df <- data.frame(mean_time = filled)
  
  std_full <- left_join(filled.df, std_vec, by='mean_time')
  std_full$std_vector <- na.approx(std_full$mean_int)
  std_full <- std_full %>%
    rename(date_msre=mean_time) %>%
    select(-mean_int)
  
  # clean up unnecessary vars before join
  samp <- samp %>%
    na.omit() %>%
    select(sampleID, integral, inject_num, incub_count, date_flush, date_msre)
  
  # merge interpolated standards with the sample values
  samp <- left_join(samp, std_full, by = 'date_msre')

  # === calculate total time in hours the tubes were incubating ===
  samp$total_time_incub <- as.numeric(difftime(samp$date_msre, samp$date_flush, units="hours"))
  # as.numeric keeps it from being a difftime object and dropping the unncessary info of units=hours

  # === return relevant variables ===
  master <- select(samp, sampleID, incub_count, total_time_incub, integral, inject_num, std_vector)
}

# @@@ FUNCTION: calculate standard error ---------------------------------------------

se <- function(vals_err_calc){
  # standard error calculation excluding NAs
  val <- sd(vals_err_calc, na.rm=TRUE)/sqrt(sum(!is.na(vals_err_calc)))
  
  # this is if there are no NA values and you can assume length = number of obs
  # sqrt(var(vals_err_calc)/length(vals_err_calc))
}

# @@@ FUNCTION: t-test returning pval --------------------------

check_diff <- function(group1, group2){
  pval <- t.test(group1, group2)$p.value
  return(pval)
}


# Import all tables and flatten all samplings into one data frame------

# === read all files in the directory + flatten ===
file_list <- list.files(path=here::here("data/IRGA"), pattern="*.csv") # file_list <- 'samp1.04.csv' # debugging

# get meta data + sample data for all files in directory
all_master <- lapply(file_list, get_info)

# flatten list of data frames into one data frame
all_samp <- bind_rows(all_master)

# write to file: all standards interpolated, all samples properly labeled
write.csv(all_samp, file=here::here('results/all_samp.csv'))

# === import tube original soil values + merge ===
dsoil_raw <- read.csv('data/', header=T)
dsoil_table <- subset(dsoil_table, sampleID != 'GGR.5')


# === merge IRGA data with dry soil data ===
table_merged <- merge(all_samp, dsoil_table, by=c('sampleID'))
data_orig <- arrange(table_merged, sampleID, incub_count) # ordering the data in the table

# known standard gas CO2 ppm value
known_std <- 1997

# this is a statement that checks that no injection values are missing
inj_idx <- (data_orig$inject_num != 0)

# data_orig has all the information needed for adjusting to references
data_orig$inject_num[!inj_idx] <- 1

# Calculate gross daily ppm values [data_orig]------

# scaling the sample integral value to the calibration + injection number
# amount the ppm adjusts with each 5ml co2-free air injection
inj_constant <- 1.096

# sub-value of true ppm
data_orig$samp_co2_sub <- (known_std * (data_orig$integral/data_orig$std_vector))*(inj_constant^data_orig$inject_num)

# converting to CO2 micromoles, then micrograms-C, divide for ppm->microM
co2c_const <- 0.05 * 12.0011 / 22.4

# calculate _final_ CO2 ppm rate per day, in units CO2-C (microg C/g/h)
data_orig$samp_co2_tot <- data_orig$samp_co2_sub * co2c_const / data_orig$actual_dsoil
data_orig$samp_co2_rate <- data_orig$samp_co2_tot / data_orig$total_time_incub
data_orig <- mutate(data_orig, samp_co2_perday = samp_co2_rate*24)

# Calculate average reference values [ref_data, ctrl_data]-----------------------

# this is just for knowing when to cut off the control tubes
# summarize reference replicates
ref_data <- data_orig %>%
  filter(treatment=='R') %>%
  select(incub_count, MC, rep, samp_co2_perday) %>%
  group_by(MC, incub_count) %>%
  summarise(MC_ref_avg = mean(samp_co2_perday, na.rm=T))

# this is for differences between tubes
ctrl_data <- data_orig %>%
  filter(treatment=='C') %>%
  select(incub_count, MC, rep, samp_co2_perday) %>%
  group_by(MC, incub_count) %>%
  summarise(MC_ctrl_avg = mean(samp_co2_perday, na.rm=T))

# # ===FOR DEBUGGING===
# # plot the reference average values over time
# ggplot(ref_data, aes(x=incub_count, y=MC_ref_avg, color=factor(MC))) +
#   geom_point(shape=20, size=4) +
#   geom_line(size=1, aes(factor=(MC)))

# to find which observations have NA!
# diff_data[!complete.cases(diff_data),]

# Calculate DIFFERENCE/NET of treatment average from reference [data_summary, diff_data, diff_summary_C] ---------

# merge reference averages to rest of data
data_summary <- data_orig %>%
  group_by(MC, treatment, incub_count) %>%
  summarise_each(funs(mean(., na.rm = TRUE), se), samp_co2_perday)
data_summary <- rename(data_summary, mean_reps=mean, se_reps=se)

# merge control averages to rest of data
diff_summary_C <- merge(data_summary, ctrl_data, by=c('MC', 'incub_count')) %>%
  mutate(diff_perday_C = mean_reps - MC_ctrl_avg)

diff_summary_C <- diff_summary_C %>%
  mutate(trt_ID=paste0(as.character(MC), as.character(treatment)))

# # # ===GRAPHS FOR DEBUGGING===
# # plot reference data by one single MC/soil
# ggplot(ctrl_data %>% filter(MC=='GG'), aes(x= incub_count, y=MC_ctrl_avg), color=MC) +
#    geom_point() +
#    ggtitle('Control values per soil MC') +
#    geom_line()
#
# # plotting all four MCs at once
# ggplot(ctrl_data, aes(x= incub_count, y=MC_ctrl_avg), color=MC, group=MC) +
#    geom_point() +
#    ggtitle('ALL control respiration values for each MC') +
#    geom_line(aes(group=MC, color=MC))
#
# # FOR GENERAL DEBUGGING IN THIS SECTION USE THIS
# # plot each treatment ppm by day over time
# ggplot(diff_summary_C, aes(x=incub_count, y=mean_reps, color=factor(interaction(MC, treatment)))) +
#    geom_point(shape=20, size=4) +
#    ggtitle('Raw CO2 values for each treatment') +
#    geom_errorbar(aes(ymin=mean_reps-se_reps, ymax=mean_reps+se_reps), width=0.3) +
#    geom_line(aes(factor=interaction(MC, treatment)))
# # # ===END DEBUGGING===

# Declare variables for unique treatments, labels, tubes, for a simplified data table [basic_data_C]-----

# irga_days is a data frame bc it needs to merge later
irga_days <- data.frame(incub_count = unique(diff_summary_C$incub_count)) %>% arrange(incub_count)
trt_vec <- sort(as.character(unique(diff_summary_C$treatment)))
MC_vec <- as.character(unique(diff_summary_C$MC))
tube_IDs <- unique(diff_summary_C$trt_ID)
tube_labels <- data.frame(trt_ID = as.factor(tube_IDs),
                          MC = substr(tube_IDs, 1, 2),
                          treatment = substr(tube_IDs, 3, 3)) %>%
  arrange(MC,treatment)

max_days <- max(diff_summary_C$incub_count)
days_all <- seq(1, max_days)

# reduce number of variables carried, no need for diff_summary_C anymore
basic_data_C <- select(diff_summary_C, trt_ID, incub_count, diff_perday_C, se_reps)
# basic_data_C is the last table with non-inferred data

# Create blank grid and merge with data [gapped_full_C] ------------------------------------

# matrix for all treatments across all days (to fill in with interpolated values)
grid_vals <- expand.grid(tube_IDs, days_all)
colnames(grid_vals) <- c('trt_ID', 'incub_count')

# outer join blank table with CO2 data
gapped_data_C <- merge(grid_vals, basic_data_C, all.x=TRUE, by=c('trt_ID', 'incub_count'))

# include MC, and treatment columns
gapped_full_C <- merge(gapped_data_C, tube_labels, all.x=TRUE, by='trt_ID')

# Interpolate daily CO2 + running cumulative values [cumul_summary_C]---------------------------

# each trt_ID (i.e. tube reps summary) will go through its own interpolation
filled_full_C <- gapped_full_C %>%
  group_by(trt_ID) %>%
  arrange(incub_count) %>%
  mutate(infer_perday_C = na.approx(diff_perday_C))

# calculate cumulative CO2 respiration for each tube
cumul_summary_C <- filled_full_C %>%
  group_by(trt_ID) %>%
  mutate(infer_cumul_C = order_by(incub_count, cumsum(infer_perday_C)))# %>%
# rename(mean_reps = diff_perday_C)

# # ===use this to check values are OK
# temptube <- cumul_summary_C %>% filter(trt_ID=='BGR')
# ggplot(temptube, aes(x=incub_count, y=infer_perday_C)) +
#   geom_point(size=4, shape=20)
# # ===end use here

# Generate error bars for sampling days  [errorbar_cumul_C, errorbar_diff_C]-----

# these are the only days that get real bars, since these were true data days
errorbar_cumul_C <- merge(irga_days, gapped_full_C, all.x=TRUE)# %>% arrange(incub_count, treatment)
errorbar_diff_C <- merge(irga_days, diff_summary_C, all.x=TRUE)

# Loop for graphing results by MC---------

# the three things to change to graph between daily/cumulative:
# sub_data + sub_error: diff <-> cumul
# ggtitle: Daily <-> Cumulative

u_ymin <- -800 # irga 3-03:  -800/-60
u_ymax <- 2000 # irga 3-03: 2000/150

for (i in 1:length(MC_vec)){
  # (un)comment the appropriate lines to graph waht you want
  # i=4
  # sub_data <- diff_summary_C %>% filter(MC==MC_vec[i] & treatment!='C' & treatment!='R' ) %>% rename(plot_vals = infer_perday_C)
  sub_data <- cumul_summary_C %>% filter(MC==MC_vec[i] & treatment!='C' & treatment!='R' ) %>%
    rename(plot_vals = infer_cumul_C)
  
  print(
    ggplot(sub_data, aes(incub_count, plot_vals, color=factor(treatment), group=factor(treatment), ymin=u_ymin, ymax=u_ymax)) +
      geom_line(aes(group=treatment)) +
      geom_point(size=0.5) +
      geom_errorbar(aes(ymin=plot_vals-se_reps, ymax=plot_vals+se_reps), width=0.3) +
      geom_hline(yintercept=0) +
      # ggtitle(paste('Daily CO2-C by MC: ', MC_vec[i])))
      ggtitle(paste('Cumulative CO2-C by MC: ', MC_vec[i])))
  
  ggsave(paste0('by_MC-', MC_vec[i], '.pdf'), width=6, height=4, dpi=400)
  
  # used to find universal max for chart
  max_temp <- max(sub_data$diff_perday_C, na.rm=T)
  print(max_temp)
  min_temp <- min(sub_data$diff_perday_C, na.rm=T)
  print(min_temp)
}

# Loop for graphing results by treatment---------

# the three things to change to graph between daily/cumulative:
# sub_data + sub_error: diff <-> cumul
# ggtitle: Daily <-> Cumulative

# cumulative
u_ymin <- -400 # irga 3-03: cumul -300, daily -60
u_ymax <- 400 # irga 3-03: cumul 100, daily 150

# daily
# u_ymin <- -100 # irga 17: cumul -800, daily -60
# u_ymax <- 100 # irga 17: cumul 2000, daily 150

trt_vec <- trt_vec[1:9] # comment out if you want reference values, but not if you run this SECTION more than once

for (i in 1:length(trt_vec)){
  # ===to plot only ONE graph, start HERE!
  # i <- 4
  
  # create a subset of data for graphing
  sub_data <- cumul_summary_C %>% filter(treatment==trt_vec[i]) %>%
    # (un)comment these appropriate two lines for daily co2 vs cumulative
    rename(plot_vals = infer_cumul_C) # CUMULATIVE
  # rename(plot_vals = infer_perday_C) # DAILY
  
  print(
    ggplot(data=sub_data, aes(x=incub_count, y=plot_vals, color=factor(MC), group=factor(MC), ymin=u_ymin, ymax=u_ymax)) +
      geom_line(aes(group=MC)) +
      geom_point() +
      geom_errorbar(aes(ymin=plot_vals-se_reps, ymax=plot_vals+se_reps), width=0.3) +
      ggtitle(paste('Cumulative CO2-C by treatment: ', trt_vec[i])))
  # ggtitle(paste('Daily CO2-C by treatment: ', trt_vec[i])))
  # ===plot one graph, end here
  
  ggsave(paste0('by_trt-', trt_vec[i], '.pdf'), width=6, height=4, dpi=400)
  
  # used to find universal max for chart
  max_temp <- max(sub_data$diff_perday_C, na.rm=T)
  print(max_temp)
  min_temp <- min(sub_data$diff_perday_C, na.rm=T)
  print(min_temp)
}

# **(commented out)testing whether samplings differ from the reference --------------------------

# this should be made into a function... figure out which argument it needs

# for each MC, through all treatments
# pval_vec <- data.frame(row.names=mc_vec)

# trt_vec <- as.character(unique(diff_data$treatment))
# trt_vec <- sort(trt_vec)
#
# pval_mat <- as.data.frame(matrix(nrow=length(mc_vec), ncol=length(trt_vec)))
# names(pval_mat) <- trt_vec
#
# day_incub <- 40
# for (i in 1:length(mc_vec)){
#    ref <- diff_data %>% filter(MC==mc_vec[[i]], treatment=='R', incub_count==day_incub) %>% select(diff_co2_perday)
#    for (j in 1:length(trt_vec)){
#       group <- diff_data %>% filter(MC==mc_vec[[i]], treatment==trt_vec[[j]], incub_count==day_incub) %>% select(diff_co2_perday)
#
#       pval_mat[i,j] <- check_diff(ref, group)
#    }
# }
# pval_melt <- melt(pval_mat)
# pval_melt[pval_melt$value<=0.05,]
