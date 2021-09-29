# cheating script to read files to make in excel and import back here
library(tidyverse)
library(lubridate)
library(plotly)
source(here::here('src/yaya_fxns.R'))

tubes_raw <- readRDS('results/4_tubes_to_plot_adj.rds') %>% 
  ungroup()
# ctrl_mean is the daily average mean for the control tubes
tubes_ghop <- tubes_raw %>% 
  select(tubeID, trt, rep, ghop_fate, exp_count, phase, phase_count, 
         ctrl_cumul, origin_dry, origin_dry,
         cumul_diff, cumul_gross_a)

max_p1 <- get_phase1_max(tubes_ghop)
max_p2 <- max(tubes_ghop$phase_count)

#### creating the artificial mantid treatment ####
mantid_summ_p1 <- filter(tubes_ghop, trt == 'ME' | trt == 'MR') %>% 
  # filter(exp_count == max_p1) %>% 
  group_by(ghop_fate, rep, exp_count) %>% 
  summarize(M_added_gross = sum(cumul_diff),
            M_origin_dry = mean(origin_dry)) %>% 
  rename(cumul_diff = M_added_gross,
         origin_dry = M_origin_dry) %>% 
  mutate(trt = 'M_add',
         tubeID = paste(trt, formatC(rep, width=2, flag="0"), sep = "."),
         phase = 1,
         phase_count= max_p1,
         exp_count = max_p1)

mantid_summ_p2 <- filter(tubes_ghop, trt == 'ME' | trt == 'MR') %>% 
  # filter(exp_count == max_p2+max_p1) %>% 
  group_by(ghop_fate, rep, exp_count) %>% 
  summarize(M_added_gross = sum(cumul_diff),
            M_origin_dry = mean(origin_dry)) %>% 
  rename(cumul_diff = M_added_gross,
         origin_dry = M_origin_dry) %>% 
  mutate(trt = 'M_add',
         tubeID = paste(trt, formatC(rep, width=2, flag="0"), sep = "."),
         phase = 2,
         phase_count= max_p2,
         exp_count = max_p1 + max_p2)

mantid_summ <- rbind(mantid_summ_p1, mantid_summ_p2)

#### spider all ####
spider_summ_p1 <- filter(tubes_ghop, trt == 'WE' | trt == 'WW') %>% 
  # filter(exp_count == max_p1) %>% 
  group_by(ghop_fate, rep, exp_count) %>% 
  summarize(W_added_gross = sum(cumul_diff),
            W_origin_dry = mean(origin_dry)) %>% 
  rename(cumul_diff = W_added_gross,
         origin_dry = W_origin_dry) %>% 
  mutate(trt = 'W_add',
         tubeID = paste(trt, formatC(rep, width=2, flag="0"), sep = "."),
         phase = 1,
         phase_count= max_p1,
         exp_count = max_p1)

spider_summ_p2 <- filter(tubes_ghop, trt == 'WE' | trt == 'WW') %>% 
  # filter(exp_count == max_p2+max_p1) %>% 
  group_by(ghop_fate, rep, exp_count) %>% 
  summarize(W_added_gross = sum(cumul_diff),
            W_origin_dry = mean(origin_dry)) %>% 
  rename(cumul_diff = W_added_gross,
         origin_dry = W_origin_dry) %>% 
  mutate(trt = 'W_add',
         tubeID = paste(trt, formatC(rep, width=2, flag="0"), sep = "."),
         phase = 2,
         phase_count= max_p2,
         exp_count = max_p1 + max_p2)

spider_summ <- rbind(spider_summ_p1, spider_summ_p2)

#### spider throw ####
spider_throw_p1 <- filter(tubes_ghop, trt == 'WS' | trt == 'WR') %>% 
  # filter(exp_count == max_p1) %>% 
  group_by(ghop_fate, rep, exp_count) %>% 
  summarize(W_added_gross = sum(cumul_diff),
            W_origin_dry = mean(origin_dry)) %>% 
  rename(cumul_diff = W_added_gross,
         origin_dry = W_origin_dry) %>% 
  mutate(trt = 'W_thro',
         tubeID = paste(trt, formatC(rep, width=2, flag="0"), sep = "."),
         phase = 1,
         phase_count= max_p1,
         exp_count = max_p1)

spider_throw_p2 <- filter(tubes_ghop, trt == 'WS' | trt == 'WR') %>% 
  # filter(exp_count == max_p2+max_p1) %>% 
  group_by(ghop_fate, rep, exp_count) %>% 
  summarize(W_thro_gross = sum(cumul_diff),
            W_origin_dry = mean(origin_dry)) %>% 
  rename(cumul_diff = W_thro_gross,
         origin_dry = W_origin_dry) %>% 
  mutate(trt = 'W_thro',
         tubeID = paste(trt, formatC(rep, width=2, flag="0"), sep = "."),
         phase = 2,
         phase_count= max_p2,
         exp_count = max_p1 + max_p2)

spider_throw <- rbind(spider_throw_p1, spider_throw_p2)


#######
# join computed predator data with with original tube data
pred_summ <- rbind(mantid_summ, spider_summ, spider_throw)

tubes_ghop_plus <- pred_summ %>% 
  full_join(tubes_ghop) %>% 
  mutate(cumul_diff_a = cumul_diff/origin_dry)

fct_relevel(tubes_ghop_plus$trt, 
            c('G','MA', 'M_add','MR', 'ME', 
              'WA', 'W_add', 'WW', 'WE','W_thro', 'WR', 'WS'))

# get only end of phase data
tubes_end_p1 <- tubes_ghop_plus %>% 
  filter(phase_count == max_p1 & phase == 1)
tubes_end_p2  <- tubes_ghop_plus %>% 
  filter(phase_count == max_p2 & phase == 2)

tubes_end <- list(tubes_end_p1, tubes_end_p2)

# formulas for regression
formula_lin <- y ~ x
formula_quad <- y ~ poly(x, 2)

######### GRAPHING @ phase ENDS ##########
graph_phase <- 2

pred_switch <- 'spider'
# pred_switch <- 'mantid'

phase_end <- tubes_end[[graph_phase]] %>%
  # mutate(trtID = paste0(trt, rep))# %>% 
  filter(ghop_fate == pred_switch | ghop_fate == 'carcass')
  # filter(ghop_fate == 'spider' | ghop_fate == 'carcass')
  # filter(ghop_fate == 'mantid' | ghop_fate == 'carcass')
  # filter(trt != 'WS')

phase_min <- min(phase_end$cumul_diff_a) - 0.1*abs(min(phase_end$cumul_diff_a))
phase_max <- max(phase_end$cumul_diff_a) + 0.1*abs(max(phase_end$cumul_diff_a))

gr_p1 <- ggplot(phase_end,
                aes(trt,
                    cumul_diff_a,
                    color = trt,
                    text = tubeID)) +
  geom_boxplot(aes(group = trt)) +
  # geom_dotplot(aes(y = cumul_diff_a, 
  # group = trt)) +
  # geom_point(size=1) +
  # geom_line()+
  # labs(x="amendment protein proportion",
  labs(x = 'treatment',
       y = "CO2-C mineralized, diff adj from control",
       # title = paste("amendment protein proportion vs phase",graph_phase,
       title = paste(pred_switch, 'treatments vs adj C-mineralization, phase',
                     graph_phase)) + 
  ylim(phase_min, phase_max) +
  theme_bw()

gr_p1

ggsave(paste(here::here('results/', today()),
             pred_switch, '_p', graph_phase,
             '_box_cumul_diff_origin_dry.png', sep=""),
       # '_scatter_cumul_diff_adj.png', sep=""),
       width=7, height=5, dpi=600)

# checkme <- tubes_end_p2 %>% filter(ghop_fate == 'mantid', trt != 'MA')
# View(checkme)

########### GRAPHING OVER TIME ########################

ggplot()