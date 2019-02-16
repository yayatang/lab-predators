# Compile tubes setup dasta
# August 2018

# set working directory + libraries
library(dplyr)
library(here)
setwd(here())

# clear workspace
rm(list = ls())# Create the tube data frame

# @FXN: rounding decimal points -------------------------------------------
# make sure sufficient decimal points are included
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

# import and clean data ---------------------------------------------------
tubes_target <- read.csv(here('data/IRGA prep', '00_setup_0-tubes-masses.csv'), header=TRUE)
moisture <- read.csv(here('data/IRGA prep','00_moisture_soil.csv'), heade =TRUE)
tubes_randomized <- read.csv(here('data/IRGA prep','00_setup_1-tubes-randomized.csv'), header=TRUE)

# edit data frames to clear out unncessary vars + round digits
moisture$soil.origin <- NULL
tubes_target <- round_df(tubes_target, 4)


# calculate parts added --------------------------------------------------

# vvvv NOT USED THIS TIME
# # calculate target fresh soil weight
# target.dry <- 5
# tubes_target$real.target.fsoil <- round(target.dry/(1-tubes_target$avg.gmc/100), digits=4)
# ^^^^

# calculate actual fresh + dry soil weight
tubes_target$actual.fsoil <- tubes_target$actual.tube - tubes_target$tube.mass
tubes_target$actual.dsoil <- tubes_target$actual.fsoil * (1-(moisture$gmc.fresh/100))

# calculate target water to add based on dry soil mass
whc_level <- 0.65 #65% water holding capacity
tubes_target$target.water <- tubes_target$actual.dsoil * (moisture$whc100.dry*whc_level/100)

# calculate actual treatment mass added
tubes_target$actual.trt <- tubes_target$actual.post.trt - tubes_target$actual.pre.trt

# calculate total target mass:
# tube + dry soil + treatment + added water
tubes_target$target.moistening <- tubes_target$tube.mass + tubes_target$actual.dsoil + tubes_target$actual.trt + tubes_target$target.water
tubes_target <- round_df(tubes_target, 4)

write.csv(tubes_target, file = here('data/IRGA prep', '0_tubes_target.csv'), row.names=FALSE)


# merge target moistening values with randomized tubes --------------------
tubes_setup <- merge(tubes_target, tubes_randomized, by="sampleID")
tubes_setup <- arrange(tubes_setup, rack, posit)

write.csv(tubes_setup, file=here('data/IRGA prep', '0_tubes_moistening.csv'), row.names=FALSE)
                       
          