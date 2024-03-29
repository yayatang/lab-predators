# new plotting script
# make a function, give it names, copy it to the appropriate vars

library(tidyverse)
library(plotly)
library(zoo)

source(here::here('src/yaya_fxns.R'))

# data from the first plot used to get universal trt + max_p1 vars
trts_ghop_adj <- readRDS('results/4_trts_to_plot_adj.rds') %>% #adjusted vals
  ungroup()
# not sure this is a valid file....
tubes_ghop_adj <- readRDS('results/4_tubes_to_plot_adj.rds') %>% 
  ungroup()

trts <- unique(trts_ghop_adj$trt)
max_p1 <- get_phase1_max(trts_ghop_adj)
max_p2 <- max(trts_ghop_adj$phase_count)

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
  geom_line(size = 0.7) +
  geom_errorbar(aes(ymin = trt_cumul_gross - trt_cumul_se,
                    ymax = trt_cumul_gross + trt_cumul_se,
                    width = 0.5)) +
  xlab('Experimental days lapsed') +
  ylab('Cumulative C-CO2') + 
  guides(fill = guide_legend(title=NULL)) +
  ggtitle('Comparing prey nutrient by fate, input adjusted') +
  theme_bw()

plot_pred_all
# p1 <- ggplotly(plot_pred_all)
# p1
# ggsave(paste0('results/5.1_compare_predators.png'), width=5, height=4, dpi=1000)
#==============================================================
# 2. compare cumulative predator poops, adjusted to poop/amendment mass
trts_amend_adj <- readRDS('results/4_trts_to_plot_adj_amend.rds')%>% #adjusted vals
  ungroup() %>% 
  unique()
# trts_amend_adj <- readRDS('results/4_trts_to_plot_adj_poop.rds')%>% #adjusted vals
# ungroup()

# including cumulative values per phase
# trts_amend_adj <- readRDS('results/4_tubes_by-phase.rds')

compare_poop <- trts_amend_adj %>% 
  filter(trt == 'WE' | trt=='ME') %>% 
  select(-tube_num, -origin_dry) %>% 
  unique()

compare_poop[compare_poop == "NaN"] <- NA
compare_poop$trt_cumul_gross <-  na.approx(compare_poop$trt_cumul_gross)

plot_poop_all <- ggplot(compare_poop, aes(x = exp_count, y = trt_cumul_gross,
                                          color = trt, group = trt)) +
  geom_line(aes(group=trt)) +
  geom_vline(xintercept = max_p1) +
  geom_hline(yintercept = 0) +
  geom_line(size = 0.7) +
  # geom_point(size = 1.1) +
  geom_errorbar(aes(ymin = trt_cumul_gross - trt_cumul_se,
                    ymax = trt_cumul_gross + trt_cumul_se,
                    width = 0.5)) +
  xlab('Experimental days lapsed') +
  ylab('Cumulative C-CO2') + 
  # scale_fill_manual(values = c("#d8b365", "#f5f5f5", "#5ab4ac")) +
  # scale_fill_manual(name='Treatment',
  #                   labels = c('Mantid waste', 'Spider waste')) +
  #                   # values = pred_colors) + 
  ggtitle('Cumulative gross CO2, waste biomass adjusted') +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))

plot_poop_all
# p2 <- ggplotly(plot_poop_all)
# p2
# ggsave(paste0('results/5.2_compare_predators_poop.png'), width=5, height=4, dpi=1000)

#==============================================================
# 3. compare cumulative predator poops, adjusted to ghop mass
# THIS COMPARISON DOESNT MAKE SENSE
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
  labs(title = 'Cumulative gross CO2, ghop adjusted')

p3 <- ggplotly(plot_poop_origin)
# ggsave(paste0('results/5.3_compare_poop.png'), width=5, height=4, dpi=1000)

#==============================================================
# 4. predator ALL vs predator products summed

# graph A: widow
compare_widow <- trts_amend_adj %>% 
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
  labs(title = 'Cumulative gross CO2, ghop adjusted')

p4a <- ggplotly(plot_widow)
p4a
# ggsave(paste0('results/5.4a_compare_widow.png'), width=5, height=4, dpi=1000)

compare_widow_end <- filter(compare_widow, phase_count == max_p2) %>% 
  distinct()

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
  labs(title = 'Cumulative gross CO2, ghop adjusted')

p4b <- ggplotly(plot_mantid)
p4b


# ggsave(paste0('results/5.4b_compare_mantid.png'), width=5, height=4, dpi=1000)


#==============================================================
# 5. comparing the effect of silk (widow wrapped vs widow remains)
compare_silk <- trts_amend_adj %>%  #
  filter(trt == 'WW' | trt == 'WR') %>% 
  select(-tube_num, -origin_dry) %>% 
  unique()

compare_silk[compare_silk == "NaN"] <- NA
compare_silk$trt_cumul_gross <-  na.approx(compare_silk$trt_cumul_gross)


# silk_summed <- compare_silk %>% 
#   filter(trt != 'WA') %>% 
#   group_by(exp_count) %>% 
#   summarize(trt_cumul_gross, sum)

plot_silk <- ggplot(compare_silk, aes(x = exp_count, y = trt_cumul_gross, 
                                      color = trt, group = trt)) +
  geom_line(aes(group=trt)) +
  geom_vline(xintercept = max_p1) +
  geom_hline(yintercept = 0) +
  geom_line(size = 0.7) +
  # geom_point(size = 1.1) +
  geom_errorbar(aes(ymin = trt_cumul_gross - trt_cumul_se,
                    ymax = trt_cumul_gross + trt_cumul_se,
                    width = 0.5)) +
  xlab('Experimental days lapsed') +
  ylab('Cumulative C-CO2') + 
  # scale_fill_manual(values = c("#d8b365", "#f5f5f5", "#5ab4ac")) +
  # scale_fill_manual(name='Treatment',
  #                   labels = c('Mantid waste', 'Spider waste')) +
  #                   # values = pred_colors) + 
  ggtitle('Cumulative gross CO2, ghop adjusted') +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))

# p5 <- ggplotly(plot_silk)
plot_silk
# ggsave(paste0('results/5.5_compare_silk.png'), width=5, height=4, dpi=1000)
# 
# #=========
# # save all plots
# htmlwidgets::saveWidget(p1, here::here('results/5.1_compare_pred_all.html'), selfcontained=TRUE)
# htmlwidgets::saveWidget(p2, here::here('results/5.2_compare_pred_poop.html'), selfcontained=TRUE)
# htmlwidgets::saveWidget(p3, here::here('results/5.3_compare_poop_origin.adj.html'), selfcontained=TRUE)
# htmlwidgets::saveWidget(p4a, here::here('results/5.4a_compare_widow.html'), selfcontained=TRUE)
# htmlwidgets::saveWidget(p4b, here::here('results/5.4b_compare_mantid.html'), selfcontained=TRUE)
# htmlwidgets::saveWidget(p5, here::here('results/5.5_compare_silk.html'), selfcontained=TRUE)