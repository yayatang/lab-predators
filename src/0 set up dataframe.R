# Create the tube data frame
# Aug 2018

# set working directory + libraries
# setwd(choose.dir())
# library(reshape)
library(here)


# Set up the "tubes" data frame -------------------------------------------
tubes <- data.frame(tube_num = integer(144))
irga_count <- 144


# give each tube a number
tubes$tube_num <- cbind(seq(1:144))

# set of IRGA treatments + rep labels
# reps: 12 = replicates, 12 "treatments"
tubes$treatment <- rep(rep(c('R', 'C', 'G', 'ME', 'MR', 'MA', 'WE', 'WR', 'WW', 'WA', 'WS', 'WN'), each=12), length.out=irga_count)

# counting reps for original and bonus
tubes$rep <- rep(seq(1:12), length=144)

# randomize tubes-----------------------------------
# ***do this only once before exporting!!!!
# tubes <- tubes[sample(1:nrow(tubes)), ]

# labeling racks and tube position in rack ----------------------------
# *****this should happen AFTER randomization!!!
tubes$rack <- cbind(rep(seq(1:3), each=48))
tubes$posit <- cbind(rep(seq(1:48), length.out=nrow(tubes)))
tubes$sampleID <- paste(tubes$treatment, formatC(tubes$rep, width=2, flag="0"), sep=".")

# export to file
write.csv(tubes, file = here('data/raw', 'pp_tube_names.csv'))
