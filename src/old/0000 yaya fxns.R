# yaya fxns, orig (not package)

# IMPORT META, DATA, INTERPOLATE STANDARDS AND ORGANISE------------------
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
  
  cat(substr(fileloc, 79, 93), ", meta")
  
  
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
  
  cat(substr(fileloc, 79, 93), ". sample")
  
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
  
  cat(substr(fileloc, 79, 93), ", standards")
  
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
  samp<-import_sample(fileloc, meta_data)
  interpolate<-interpolate_std(samples)
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

