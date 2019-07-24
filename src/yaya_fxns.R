## yaya.fxns

se <- function(vals_err_calc){
  # assume length = number of obs
  # standard error calculation excluding NAs
  val <- sd(vals_err_calc, na.rm=TRUE)/sqrt(sum(!is.na(vals_err_calc)))
}

check_midnight <- function(test_date){
  new_dates <- if_else(hour(test_date) < 6, test_date + days(1), test_date)
}

# function for swapping tube names
# this is easier after everything is in one table

switch_tubes <- function(wrong_tubes, switch_list){
  # # original function
  # 
  # for (i in 1:nrow(switch_list)){
  #   # get the name of the tube aka tubeID
  #   tube1 <- as.character(switch_list[i, 1])
  #   tube2 <- as.character(switch_list[i, 2])
  # 
  #   # get the necessary data for the tube, i.e. tubeID, trt, rep from tubeID
  #   tube1_meta <- tibble(sampleID = tube1, trt = substr(tube1, 1, 1), rep = as.numeric(substr(tube1, 3, 4)))
  #   tube2_meta <- tibble(sampleID = tube2, trt = substr(tube2, 1, 1), rep = as.numeric(substr(tube2, 3, 4)))
  # 
  #   # find corresponding tube num
  #   tube1_meta$tube_num <- all_tubes[which(all_tubes$sampleID==tube1 & all_tubes$phase==1),]$tube_num[1]
  #   tube2_meta$tube_num <- all_tubes[which(all_tubes$sampleID==tube2 & all_tubes$phase==1),]$tube_num[1]
  # 
  #   # phase 1, switch ALL tube info, phase 2 switch all BUT sampleID info
  #   all_tubes[which(all_tubes$tube_num==tube1_meta$tube_num & all_tubes$phase == 1),]$sampleID <- tube2_meta$sampleID
  #   all_tubes[which(all_tubes$tube_num==tube2_meta$tube_num & all_tubes$phase == 1),]$sampleID <- tube1_meta$sampleID
  #   all_tubes[which(all_tubes$tube_num==tube1_meta$tube_num),]$trt <- tube2_meta$trt
  #   all_tubes[which(all_tubes$tube_num==tube2_meta$tube_num),]$trt <- tube1_meta$trt
  #   all_tubes[which(all_tubes$tube_num==tube1_meta$tube_num),]$rep <- tube2_meta$rep
  #   all_tubes[which(all_tubes$tube_num==tube2_meta$tube_num),]$rep <- tube1_meta$rep
  # }
  
  # #TROUBLE SHOOTING
  # wrong_tubes <- all_samp3
  # i <- 2
  # 
  # new function
  for (i in 1:nrow(switch_list)){
    # get the name of the tube aka tubeID
    tube1 <- as.character(switch_list[i, 1])
    tube2 <- as.character(switch_list[i, 2])
    
    # tube_num_old1 <- wrong_tubes[wrong_tubes$sampleID==tube1 & wrong_tubes$phase==1,]$tube_num[1]
    # tube_num_old2 <- wrong_tubes[wrong_tubes$sampleID==tube2 & wrong_tubes$phase==1,]$tube_num[1]
    # 
    # tube1_new_meta <- wrong_tubes[which(wrong_tubes$tube_num == tube_num_old1), c('sampleID', 'trt','rep')]
    
    tube1_trt = substr(tube1, 1, 1)
    tube1_rep = as.numeric(substr(tube1, 3, 4))
    
    tube2_trt = substr(tube2, 1, 1)
    tube2_rep = as.numeric(substr(tube2, 3, 4))
    
    # find corresponding tube num
    tube1_num <- wrong_tubes[which(wrong_tubes$sampleID==tube1 & wrong_tubes$phase==1),]$tube_num[1]
    tube2_num <- wrong_tubes[which(wrong_tubes$sampleID==tube2 & wrong_tubes$phase==1),]$tube_num[1]
    
    # create new rows for DF
    # phase 1 v 2 sampleIDs DO NOT differ--merged by tube_num during import
    new_tube1 <- wrong_tubes[wrong_tubes$tube_num == tube1_num,]
    new_tube2 <- wrong_tubes[wrong_tubes$tube_num == tube2_num,]
    
    # remove old rows in DF
    wrong_tubes <- wrong_tubes[!(wrong_tubes$tube_num==tube1_num | wrong_tubes$tube_num==tube2_num),]
    # cat(nrow(wrong_tubes),"\n")
    
    # fix new rows
    new_tube1$sampleID <- tube2
    new_tube1$trt <- tube2_trt
    new_tube1$rep <- tube2_rep
    
    new_tube2$sampleID <- tube1
    new_tube2$trt <- tube1_trt
    new_tube2$rep <- tube1_rep
    
    # print(new_tube1, "\n", new_tube2, "\n")
    
    all_new_tubes <- rbind(new_tube1, new_tube2)
    wrong_tubes <- rbind(all_new_tubes, wrong_tubes)
  }
  new_df <- wrong_tubes
}

get_info <- function(fileloc){
  
  #==========================================
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
  
  # cat("meta imported")
  # ======================================================================================
  # === imports samples and standard data ===
  # reads file, removes empty and NA cols
  raw_sampstd <- read.csv(fileloc, skip = 2, header = TRUE, na.strings = c(".", " ", "", "NA"),
                          stringsAsFactors = FALSE)
  arr_sampstd_raw <- raw_sampstd[!is.na(names(raw_sampstd))]
  arr_sampstd_whole <- arr_sampstd_raw[colSums(!is.na(arr_sampstd_raw)) > 0]
  arr_sampstd_clean <- as_tibble(arr_sampstd_whole)
  
  cols_keep <- c(
    "sampleID", "tube.num", "rep", "rack", "position", "time.flushed",
    "time.sampled", "integral", "inject.num", "std.int.time", "std.integral")
  arr_sampstd <- arr_sampstd_clean[cols_keep]
  colnames(arr_sampstd) <- c(
    "sampleID", "tube_num", "rep", "rack", "position", "time_flush", "time_msre", "integral",
    "inject_num", "std_time_int", "std_integral")
  
  # === splits standards from sample data ===
  std_data <- select(arr_sampstd, std_time_int, std_integral)
  std_data$date_std <- ymd_hms(paste(meta$day_msre, std_data$std_time_int))
  std_data$date_std <- check_midnight(std_data$date_std)
  
  # ========FUTURE STANDARD GROUPING AND INTERPOLATION FXN
  # groups and summarizes true standard data
  # quick and dirty way to average the last three values
  # ***does not test how close the std vals are***
  std_raw <- std_data %>%
    mutate(to_remove = is.na(std_integral),
           std_group = cumsum(is.na(std_integral))) %>%
    filter(to_remove == FALSE) %>%
    select(-to_remove)
  
  # group standards by when they taken, and "rank" in order
  # put in descending order to select the last three measurements
  std_grouped <- std_raw %>%
    group_by(std_group) %>%
    arrange(desc(std_time_int)) %>% 
    mutate(rank = cumsum(!is.na(std_group)))
  
  # remove standards that weren't the last three values
  std_vec <- std_grouped %>%
    filter(rank < 4) %>%
    group_by(std_group) %>%
    summarize( mean_time = date_std[rank==2], 
               mean_integral = mean(std_integral)) %>%
    select(-std_group)
  
  std_vec$mean_time <- round_date(std_vec$mean_time, unit='minute')
  
  # ==========interpolating standards===========================
  # create standard vector by minute from start to finish, w interpolated vals
  std_min <- min(std_vec$mean_time)
  std_max <- max(std_vec$mean_time)
  filled <- seq.POSIXt(std_min, std_max, by='min')
  filled.df <- data.frame(mean_time = filled)
  
  std_full <- left_join(filled.df, std_vec, by='mean_time')
  std_full$std_vector <- na.approx(std_full$mean_int)
  std_full <- std_full %>%
    # rename(std_time=mean_time) %>%
    select(-mean_integral)
  # cat("standards imported")
  
  #=======================================================================
  # === makes clean imported sample data ===
  # add the day== 
  arr_samp <- arr_sampstd %>%
    mutate(date_flush = ymd_hms(paste(meta$day_flush, arr_sampstd$time_flush)),
           date_msre = ymd_hms(paste(meta$day_msre, arr_sampstd$time_msre))) %>%
    select(-std_time_int, -std_integral, -time_flush, -time_msre) %>%
    na.omit() 
  arr_samp$date_flush <- check_midnight(arr_samp$date_flush)
  arr_samp$date_msre <- check_midnight(arr_samp$date_msre)
  arr_samp$sampleID <- as.character(as.character(arr_samp$sampleID))
  arr_samp$integral <- as.numeric(arr_samp$integral)
  arr_samp$inject_num <- as.numeric(arr_samp$inject_num)
  
  # this checks no injection values are missing
  inj_idx <- (arr_samp$inject_num != 0)
  arr_samp$inject_num[!inj_idx] <- 1
  
  arr_samp$phase <- meta$phase
  arr_samp$incub_count <- meta$incub_count
  
  arr_samp$incub_hours <- arr_samp$date_msre - arr_samp$date_flush
  # maybe need something here to make sure time diff is in hours
  arr_samp$date_for_std <- round_date(arr_samp$date_msre, unit='minute')
  
  # ==============================\==============================================
  # cat(fileloc, "OK!")
  
  # merge samples with standards + soil data (get later)
  master_sampling <- left_join(arr_samp, std_full, by = c('date_for_std'='mean_time'))
  # master_sampling <- master_sampling %>%
  # select(sampleID, phase, incub_count, date_msre, std_vector, integral, inject_num, incub_time)
  # select(-tube_num, -position, -rack)
}

get_phase1_max <- function(phases_data) {
  phase1 <- filter(phases_data, phase == 1)
  max_p1 <- max(phase1$phase_count)
}

# check cleanness
cc <- function(df) {
  print(df %>% is.na() %>% colSums())
}

# cleanness density
cd <- function(df) {
  print(df %>% is.na() %>% colMeans())
}

# calculates average ghop water content
# NOT divided by feeding
calc_dry <- function(ghop_mass) {
  ghop_mass$water_mass <- ghop_mass$mass_tube_wet - ghop_mass$mass_tube_dry
  ghop_mass$percent_water <- ghop_mass$water_mass/ghop_mass$mass_wet
  water <- na.omit(ghop_mass$percent_water)
  water_avg <- mean(water)
  water_se <- se(water)
  cat("average ghop water content:", water_avg, "+-", water_se)
  # this is a dry mass
  ghop_mass$ghop_infer_origin <- ghop_mass$mass_wet - water_avg*ghop_mass$mass_wet
  
  # returns entire tibble with new inferred ghop dry mass
  ghop_mass
}