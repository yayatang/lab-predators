# new plotting script
# make a function, give it names, copy it to the appropriate vars

library(tidyverse)
library(plotly)
source(here::here('src/yaya_fxns.R'))

# tubes4plot <- readRDS("results/4_tubes_to_plot.rds")
trts4plot <- readRDS('results/4_trts_to_plot_adj.rds') #adjusted vals

trts <- unique(trts4plot$trt)
max_p1 <- get_phase1_max(trts4plot)

#==============================================================
# 1. comparing both predators products to each other
compare_pred <- trts4plot %>% 
  filter(trt == 'MA' | trt=='G' | trt=='WA')
colnames(compare_pred)

pred_vars <- c('exp_count', 'trt_cumul_gross')
pred_labs <- list(
  x <- 'Experimental days',
  y <- 'CO2 per dry ghop gram',
  title <- 'Cumulative gross CO2, ghop adjusted')

plot_by_trt(compare_pred, pred_vars, pred_labs)
