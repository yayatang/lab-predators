# ======= header ============------------------------

# Clean + analyze IRGA data from datasheets
# August 2018

# ***to deal with switching the treatments:
# ***the line after a file is imported (or as soon as
# *** "sampleID" column is named) do the necessary switching

# Set working directory and clear workspace----------------------------------------

# rm(list=ls())

library(dplyr)
library(lubridate)
library(zoo)
library(here)
library(RColorBrewer)
library(ggplot2)
# setwd(here::here())

#fileloc <- here::here("PP-IRGA 1-3.csv")
# IMPORT META, DATA, INTERPOLATE STANDERDS AND ORGANISE------------------
# ===1: IMPORT META DATA ===
import_meta<-function(fileloc){
  
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
  meta$std_ppm <- as.numeric(meta$std_ppm)
  
  return(meta)
}

# === 2: IMPORT SAMPLE DATA ===
import_sample<- function(fileloc,meta_data){  #meta_data should be "meta" from import_meta function
  
  # read file + rename columns
  samp <- read.csv(fileloc, skip=2, header=TRUE, na.strings=c('.',' ','', 'NA'), stringsAsFactors=FALSE)
  samp <- samp[,1:13]
  colnames(samp) <- c('sampleID','tube_num', 'rep', 'rack', 'position', 'time_flush', 'time_msre', 'integral', 
                      'inject_num', 'std_time_int', 'std_integral', 'inj_redo', 'inj_2nd_int')
  samp <- samp[!is.na(names(samp))] #removes columns named NA
  samp <- samp[colSums(!is.na(samp)) > 0] # removes any empty columns
  
  samp$incub_count <- meta_data$incub_count
  samp$std_time_int <- paste(meta_data$day_flush, samp$std_time_int)
  
  return(samp)
}

# === 3: IMPORT + INTERPOLATE STANDARDS ===
interpolate_std<-function(samples){  #samples should be "samp" from import_sample function
  # standards are separated into their own list
  standards <- select(samples, std_time_int, std_integral) %>%
    mutate(to_remove = is.na(std_integral),  # marking which rows to remove (i.e. NAs)
           std_group = cumsum(is.na(std_integral))) %>% # groups of standards have the same number
    filter(to_remove==FALSE)
  standards <- select(standards, -to_remove)
  standards$std_time_int <- ymd_hms(standards$std_time_int)
  std_full <- make_std(standards)
  
  return(std_full)
}

# === 4: COMBINE ALL + FORMAT ===
combine_all<- function(samples, interpolate_data, meta_data){ ##samples should be "samp" from import_sample function 
  #meta_data should be "meta" from import_meta function #interpolate_data should be "std_full" from interpolate_std
  samp_trim <- samples %>%
    select(-std_time_int, -std_integral) %>% #remove standards from main data
    na.omit()
  
  # === 4: COMBINE ALL + FORMAT ===
  samp_trim$date_flush <- ymd_hms(paste(meta_data$day_flush, samp_trim$time_flush))
  samp_trim$date_flush <- round_date(samp_trim$date_flush, unit='minute')
  
  samp_trim$date_msre <- ymd_hms(paste(meta_data$day_msre, samp_trim$time_msre))
  samp_trim$date_msre <- round_date(samp_trim$date_msre, unit='minute')
  
  # clean up unnecessary vars before join
  samp_trim <- samp_trim %>%
    select(sampleID, integral, inject_num, incub_count, date_flush, date_msre)
  
  # merge interpolated standards with the samp_trimle values
  samp_trim <- left_join(samp_trim, interpolate_data, by = 'date_msre')
  
  # === calculate total time in hours the tubes were incubating ===
  samp_trim$total_time_incub <- as.numeric(difftime(samp_trim$date_msre, samp_trim$date_flush, units="hours"))
  # as.numeric keeps it from being a difftime object and dropping the unncessary info of units=hours
  samp_trim <- select(samp_trim, sampleID, incub_count, total_time_incub, integral, inject_num, std_vector)
  
  return(samp_trim)
}

# === 5: COMBINE FUNCTIONS FOR CODE USE ===
get_info<- function(fileloc, meta_data, samples, interpolate_data){
  meta<-import_meta(fileloc)
  samp<-import_sample(fileloc, meta)
  interpolate<-interpolate_std(samp)
  combine_full<-combine_all(samp,interpolate,meta)
}

# @@@ FUNCTION: interpolate standards------------------------------------
make_std <- function(standards){
  # === vectorize calibration values/standards ===
  # quick and dirty way to average the last three values, does not test how close the std vals are
  std_raw <- standards
  std_raw$std_time_int <- round_date(std_raw$std_time_int, unit='minute')
  
  # group standards by when they taken, and "rank" in order
  std_grouped <- std_raw %>%
    group_by(std_group) %>%
    arrange(desc(std_time_int)) %>% # put in descending order to select the last three measurements
    mutate(rank = cumsum(!is.na(std_group)))
  
  # remove standards that weren't the last three values
  std_vec <- std_grouped %>%
    filter(rank < 4) %>%
    group_by(std_group) %>%
    summarize(mean_int = mean(std_integral), 
              mean_time = std_time_int[rank==2]) %>%
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

# === Function for plotting CO2 perday Per treatment===
plot_trt_daily<-function(treatment,data_sheet){
  trt_ref<-data.frame("abb"=c("R","C","G","ME","MR","MA","WA","WE","WR","WW","WS","WN"),
                      "name"=c("Reference","Control","Whole_prey","Mantis_Excreta",
                               "Mantis_prey_remains","Mantis_All","Widow_All","Widow_Excreta",
                               "Widow_Prey_Remains_NO_silk","Widow_Prey_Remains_With_silk",
                               "Widow_Capture_silk","Widow_Nest_Silk"))
  data_trt<-data_sheet %>% filter(trt==treatment)          
  trt_string<-trt_ref$name[match(treatment,trt_ref$abb)]
  
  ggplot(data_trt, aes(incub_count,samp_co2_perday, color=sampleID))+
    geom_point()+
    geom_line()+
    #geom_errorbar(aes(ymin=infer_perday_C-se_reps, ymax=infer_perday_C+se_reps), width=0.3) +
    scale_color_brewer(palette = "Paired")+
    ggtitle(paste("daily C per treatment", trt_string))
  
  ggsave(paste0("results/3_daily_C_per_treatment_",trt_string,".pdf"), width=8, height=6, dpi=400)
}

# === Function for plotting CO2 cumulitve Per treatment===
plot_trt_cumul<-function(treatment,data_sheet){
  trt_ref<-data.frame("abb"=c("R","C","G","ME","MR","MA","WA","WE","WR","WW","WS","WN"),
                      "name"=c("Reference","Control","Whole_prey","Mantis_Excreta",
                               "Mantis_prey_remains","Mantis_All","Widow_All","Widow_Excreta",
                               "Widow_Prey_Remains_NO_silk","Widow_Prey_Remains_With_silk",
                               "Widow_Capture_silk","Widow_Nest_Silk"))
  data_trt<-data_sheet %>% filter(trt==treatment)          
  trt_string<-trt_ref$name[match(treatment,trt_ref$abb)]
  
  ggplot(data_trt, aes(incub_count,infer_cumul_C, color=sampleID))+
    #geom_point()+
    geom_line()+
    geom_errorbar(aes(ymin=infer_cumul_C-ctrl_se, ymax=infer_cumul_C+ctrl_se), width=0.5) +
    scale_color_brewer(palette = "Paired")+
    ggtitle(paste("cumul C per treatment", trt_string))
  
  ggsave(paste0("results/4_cumul_C_per_treatment_",trt_string,".pdf"), width=8, height=6, dpi=400)
}

### For plotting a cumulative graohs for treatment per tube------------------

# Import all tables and flatten all samplings into one data frame---------

# === read all files in the directory + flatten ===
file_list <- list.files(path=here::here("data/IRGA/raw"), pattern="*.csv", full.names = TRUE)
#file_list <- here::here("PP-IRGA 1-3.csv") # debugging

# get meta data + sample data for all files in directory
all_master <- lapply(file_list, get_info) 

# flatten list of data frames into one data frame
all_samp<-bind_rows(all_master)

# write to file: all standards interpolated, all samples properly labeled
write.csv(all_samp, "results/all_samp.csv")




# === make a master soil DF with soil mass, treatment mass, and moistening target ===
soil_ID <- read.csv(here::here("data/IRGA prep/00_setup_0-tubes-masses.csv"), header=T)
colnames(soil_ID) <- c('trt', 'rep', 'sampleID', 'tube_mass', 'tube_soil_actual', 
                       'pre_trt', 'tube_soil_trt')
soil_ID <- soil_ID %>%
  mutate(trt_mass = tube_soil_trt - pre_trt) %>%
  select(-tube_mass, -tube_soil_actual)

soil_raw <- read.csv(here::here("data/IRGA prep/00_setup_2-tubes-soil.csv"), header=T)
colnames(soil_raw) <- c('trt', 'rep', 'tube_mass', 'soil_target', 'tube_soil_target',
                        'tube_soil_actual')
soil_raw <-  soil_raw %>%
  mutate(soil_net = tube_soil_actual - tube_mass) %>%
  select(-soil_target, -tube_soil_target)

soil_master <- left_join(soil_raw, soil_ID, by=c('trt', 'rep'))

# === merge IRGA data with dry soil data ===
table_merged <- merge(all_samp, soil_master, by=c('sampleID'))
data_orig <- arrange(table_merged, sampleID) # ordering the data in the table

# known standard gas CO2 ppm value
known_std <- 1997

# this is a statement that checks that no injection values are missing
inj_idx <- (data_orig$inject_num != 0)

# data_orig has all the information needed for adjusting to references
# variables included: 
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
data_orig$samp_co2_tot <- data_orig$samp_co2_sub * co2c_const / data_orig$tube_soil_actual
data_orig$samp_co2_rate <- data_orig$samp_co2_tot / data_orig$total_time_incub
data_orig <- mutate(data_orig, samp_co2_perday = samp_co2_rate*24)

# Calculate average reference values [ref_data, ctrl_data]-----------------------

# this is just for knowing when to cut off the control tubes
# summarize reference replicates
ref_data <- data_orig %>%
  filter(trt=='R') %>%
  select(incub_count, rep, samp_co2_perday) %>%
  group_by(incub_count)%>%
  summarise(ref_avg = mean(samp_co2_perday, na.rm=T))

# this is for differences between tubes
ctrl_data_unsummarised <- data_orig %>%
  filter(trt=='C') %>%
  select(incub_count, rep, samp_co2_perday) %>%
  group_by(incub_count) %>%
  summarise_at(vars(samp_co2_perday),funs(mean(., na.rm = TRUE), se))
ctrl_data_unsummarised <- rename(ctrl_data_unsummarised, ctrl_avg=mean, ctrl_se=se)


diff_data_unsummarised <- merge(data_orig, ctrl_data_unsummarised, by=c('incub_count')) %>%
  mutate(diff_perday_C = samp_co2_perday - ctrl_avg)

diff_data_unsummarised <- diff_data_unsummarised %>%
  mutate(trt_ID=paste0(as.character(trt)))

irga_days <- data.frame(incub_count = unique(diff_data_unsummarised$incub_count)) %>% arrange(incub_count)
trt_vec <- sort(as.character(unique(diff_data_unsummarised$trt)))
tube_IDs <- unique(diff_data_unsummarised$sampleID)
tube_labels <- data.frame(sampleID = as.factor(tube_IDs),
                          trt = substr(tube_IDs, 1, nchar(tube_IDs)-3)) %>%
  arrange(trt)

max_days <- max(diff_data_unsummarised$incub_count)
days_all <- seq(1, max_days)

# reduce number of variables carried, no need for diff_summary_C anymore
basic_data_unsummarised <- select(diff_data_unsummarised, sampleID, incub_count, diff_perday_C, ctrl_se)
# basic_data_C is the last table with non-inferred data

# Create blank grid and merge with data [gapped_full_C] ------------------------------------

# matrix for all treatments across all days (to fill in with interpolated values)
grid_values <- expand.grid(tube_IDs, days_all)
colnames(grid_values) <- c('sampleID', 'incub_count')

# outer join blank table with CO2 data
gapped_data_unsummarised <- merge(grid_values, basic_data_unsummarised, all.x=TRUE, by=c('sampleID', 'incub_count'))

# include MC, and treatment columns
gapped_full_unsummarised <- merge(gapped_data_unsummarised, tube_labels, all.x=TRUE, by='sampleID')

# Interpolate daily CO2 + running cumulative values [cumul_summary_C]---------------------------

# each trt_ID (i.e. tube reps summary) will go through its own interpolation
filled_full_unsummarised <- gapped_full_unsummarised %>%
  group_by(sampleID) %>%
  arrange(incub_count) %>%
  mutate(infer_perday_C = na.approx(diff_perday_C))

# calculate cumulative CO2 respiration for each tube
cumul_summary_unsummarised <- filled_full_unsummarised %>%
  group_by(sampleID) %>%
  mutate(infer_cumul_C = order_by(incub_count, cumsum(infer_perday_C)))# %>%
# rename(mean_reps = diff_perday_C)

# these are the only days that get real bars, since these were true data days
errorbar_cumul_unsummarised <- merge(irga_days, gapped_full_unsummarised, all.x=TRUE)# %>% arrange(incub_count, treatment)
write.csv(cumul_summary_unsummarised, "results/4_cumul_final_values_by_tube.csv")


plot_trt_cumul<-function(treatment,data_sheet){
  trt_ref<-data.frame("abb"=c("R","C","G","ME","MR","MA","WA","WE","WR","WW","WS","WN"),
                      "name"=c("Reference","Control","Whole_prey","Mantis_Excreta",
                               "Mantis_prey_remains","Mantis_All","Widow_All","Widow_Excreta",
                               "Widow_Prey_Remains_NO_silk","Widow_Prey_Remains_With_silk",
                               "Widow_Capture_silk","Widow_Nest_Silk"))
  data_trt<-data_sheet %>% filter(trt==treatment)          
  trt_string<-trt_ref$name[match(treatment,trt_ref$abb)]
  
  ggplot(data_trt, aes(incub_count,infer_cumul_C, color=sampleID))+
    #geom_point()+
    geom_line()+
    geom_errorbar(aes(ymin=infer_cumul_C-ctrl_se, ymax=infer_cumul_C+ctrl_se), width=0.5) +
    scale_color_brewer(palette = "Paired")+
    ggtitle(paste("cumul C per treatment", trt_string))
  
  ggsave(paste0("results/4_cumul_C_by_treatment_",trt_string,".pdf"), width=8, height=6, dpi=400)
}


## Plot CO2 perday Per treatment----------
plot_trt_daily("MA",data_orig)
plot_trt_daily("R",data_orig)
plot_trt_daily("ME",data_orig)
plot_trt_daily("C",data_orig)
plot_trt_daily("MR",data_orig)
plot_trt_daily("G",data_orig)
plot_trt_daily("WA",data_orig)
plot_trt_daily("WE",data_orig)
plot_trt_daily("WR",data_orig)
plot_trt_daily("WW",data_orig)
plot_trt_daily("WS",data_orig)
plot_trt_daily("WN",data_orig)

## Plot CO2 cumultive Per treatment----------
plot_trt_cumul("MA", cumul_summary_unsummarised)
plot_trt_cumul("ME", cumul_summary_unsummarised)
plot_trt_cumul("MR", cumul_summary_unsummarised)
plot_trt_cumul("R", cumul_summary_unsummarised)
plot_trt_cumul("C", cumul_summary_unsummarised)
plot_trt_cumul("G", cumul_summary_unsummarised)
plot_trt_cumul("WE", cumul_summary_unsummarised)
plot_trt_cumul("WA", cumul_summary_unsummarised)
plot_trt_cumul("WR", cumul_summary_unsummarised)
plot_trt_cumul("WS", cumul_summary_unsummarised)
plot_trt_cumul("WN", cumul_summary_unsummarised)
plot_trt_cumul("WW", cumul_summary_unsummarised)


# this should be made into a function... figure out which argument it needs
# not yet ready in this iteration!!!

# for each MC, through all treatments
pval_vec <- data.frame(row.names=mc_vec)
trt_vec <- as.character(unique(diff_data$treatment))
trt_vec <- sort(trt_vec)

pval_mat <- as.data.frame(matrix(nrow=length(mc_vec), ncol=length(trt_vec)))
names(pval_mat) <- trt_vec

day_incub <- 40
for (i in 1:length(mc_vec)){
  ref <- diff_data %>% filter(MC==mc_vec[[i]], treatment=='R', incub_count==day_incub) %>% select(diff_co2_perday)
  for (j in 1:length(trt_vec)){
    group <- diff_data %>% filter(MC==mc_vec[[i]], treatment==trt_vec[[j]], incub_count==day_incub) %>% select(diff_co2_perday)
    
    pval_mat[i,j] <- check_diff(ref, group)
  }
}
pval_melt <- melt(pval_mat)
pval_melt[pval_melt$value<=0.05,]