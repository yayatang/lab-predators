# new plotting script
# make a function, give it names, copy it to the appropriate vars

library(tidyverse)
library(plotly)
source(here::here('src/yaya_fxns.R'))

# data from the first plot used to get universal trt + max_p1 vars
trts_ghop_adj <- readRDS('results/4_trts_to_plot_adj.rds') %>% #adjusted vals
  ungroup()

trts <- unique(trts_ghop_adj$trt)
max_p1 <- get_phase1_max(trts_ghop_adj)

#==============================================================
# 1. comparing both predators products to each other
compare_pred <- trts_ghop_adj %>% 
  filter(trt == 'MA' | trt=='G' | trt=='WA')
colnames(compare_pred)

plot_pred_all <- ggplot(compare_pred, aes(x = exp_count, y = trt_cumul_gross, 
                                          color = trt, group = trt)) +
  geom_line(aes(group=trt)) +
  geom_vline(xintercept = max_p1) +
  geom_hline(yintercept = 0) +
  geom_line(size = 0.5) +
  geom_point(size = 0.7) +
  geom_errorbar(aes(ymin = trt_cumul_gross - trt_cumul_se,
    ymax = trt_cumul_gross + trt_cumul_se,
    width = 0.3)) +
  guides(fill = guide_legend(title=NULL)) +
  labs(title = 'Cumulative gross CO2, ghop adjusted') #, x = pred_labs[1], y = pred_labs[2])

ggplotly(plot_pred_all)

#==============================================================
# 2. compare cumulative predator poops, adjusted to poop mass
trts_poop_adj <- readRDS('results/4_trts_to_plot_adj_poop.rds')%>% #adjusted vals
  ungroup()

compare_poop <- trts_poop_adj %>% 
  filter(trt == 'WE' | trt=='ME')

plot_poop_all <- ggplot(compare_poop, aes(x = exp_count, y = trt_cumul_gross, 
                                          color = trt, group = trt)) +
  geom_line(aes(group=trt)) +
  geom_vline(xintercept = max_p1) +
  geom_hline(yintercept = 0) +
  geom_line(size = 0.5) +
  geom_point(size = 0.7) +
  geom_errorbar(aes(ymin = trt_cumul_gross - trt_cumul_se,
                    ymax = trt_cumul_gross + trt_cumul_se,
                    width = 0.3)) +
  guides(fill = guide_legend(title=NULL)) +
  labs(title = 'Cumulative gross CO2, product adjusted') #, x = pred_labs[1], y = pred_labs[2])

ggplotly(plot_poop_all)

#==============================================================
# 3. compare cumulative predator poops, adjusted to ghop mass
compare_poop_origin <- trts_ghop_adj %>% 
  filter(trt == 'WE' | trt == 'ME')

plot_poop_origin <- ggplot(compare_poop_origin, aes(x = exp_count, y = trt_cumul_gross, 
                                          color = trt, group = trt)) +
  geom_line(aes(group=trt)) +
  geom_vline(xintercept = max_p1) +
  geom_hline(yintercept = 0) +
  geom_line(size = 0.5) +
  geom_point(size = 0.7) +
  geom_errorbar(aes(ymin = trt_cumul_gross - trt_cumul_se,
                    ymax = trt_cumul_gross + trt_cumul_se,
                    width = 0.3)) +
  guides(fill = guide_legend(title=NULL)) +
  labs(title = 'Cumulative gross CO2, ghop adjusted') #, x = pred_labs[1], y = pred_labs[2])

ggplotly(plot_poop_origin)

#==============================================================
# 4. predator ALL vs predator products summed

# graph A: widow
compare_widow <- trts_ghop_adj %>% 
  filter(trt == 'WE' | trt == 'WW' | trt == 'WA')
# widow_summed <- compare_widow %>% 
#   filter(trt != 'WA') %>% 
#   group_by(exp_count) %>% 
#   summarize(trt_cumul_gross, sum)

plot_widow <- ggplot(compare_widow, aes(x = exp_count, y = trt_cumul_gross, 
                                                    color = trt, group = trt)) +
  geom_line(aes(group=trt)) +
  geom_vline(xintercept = max_p1) +
  geom_hline(yintercept = 0) +
  geom_line(size = 0.5) +
  geom_point(size = 0.7) +
  geom_errorbar(aes(ymin = trt_cumul_gross - trt_cumul_se,
                    ymax = trt_cumul_gross + trt_cumul_se,
                    width = 0.3)) +
  guides(fill = guide_legend(title=NULL)) +
  labs(title = 'Cumulative gross CO2, ghop adjusted') #, x = pred_labs[1], y = pred_labs[2])

ggplotly(plot_widow)
#=====================
# graph B: mantids
compare_mantid <- trts_ghop_adj %>% 
  filter(trt == 'ME' | trt == 'MR' | trt == 'MA')
# mantid_summed <- compare_mantid %>% 
#   filter(trt != 'WA') %>% 
#   group_by(exp_count) %>% 
#   summarize(trt_cumul_gross, sum)

plot_mantid <- ggplot(compare_mantid, aes(x = exp_count, y = trt_cumul_gross, 
                                        color = trt, group = trt)) +
  geom_line(aes(group=trt)) +
  geom_vline(xintercept = max_p1) +
  geom_hline(yintercept = 0) +
  geom_line(size = 0.5) +
  geom_point(size = 0.7) +
  geom_errorbar(aes(ymin = trt_cumul_gross - trt_cumul_se,
                    ymax = trt_cumul_gross + trt_cumul_se,
                    width = 0.3)) +
  guides(fill = guide_legend(title=NULL)) +
  labs(title = 'Cumulative gross CO2, ghop adjusted') #, x = pred_labs[1], y = pred_labs[2])

ggplotly(plot_mantid)


#==============================================================
# 5. comparing the effect of silk (widow wrapped vs widow remains)
# graph A: silk
compare_silk <- trts_ghop_adj %>% 
  filter(trt == 'WW' | trt == 'WR')
# silk_summed <- compare_silk %>% 
#   filter(trt != 'WA') %>% 
#   group_by(exp_count) %>% 
#   summarize(trt_cumul_gross, sum)

plot_silk <- ggplot(compare_silk, aes(x = exp_count, y = trt_cumul_gross, 
                                        color = trt, group = trt)) +
  geom_line(aes(group=trt)) +
  geom_vline(xintercept = max_p1) +
  geom_hline(yintercept = 0) +
  geom_line(size = 0.5) +
  geom_point(size = 0.7) +
  geom_errorbar(aes(ymin = trt_cumul_gross - trt_cumul_se,
                    ymax = trt_cumul_gross + trt_cumul_se,
                    width = 0.3)) +
  guides(fill = guide_legend(title=NULL)) +
  labs(title = 'Cumulative gross CO2, ghop adjusted') #, x = pred_labs[1], y = pred_labs[2])

ggplotly(plot_silk)
