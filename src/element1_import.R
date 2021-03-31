# bar charts for ESA august 2019
# comparing waste + remains between predators

library(tidyverse)
library(readxl)
source(here::here('src/yaya_fxns.R'))

siel1 <- read_excel(here::here('data/SIEL data/YT1670-1_TotalCN_Rep, SIEL SO#1670 TRM.xls'), skip=2)
siel1 <- siel1[,colSums(is.na(siel1))<nrow(siel1)]

colnames(siel1) <- c('sampleID', 'mass_mg', 'percentN', 'percentC', 'CN_ratio')

siel2 <- read_excel(here::here('data/SIEL data/YT1670-2_TotalCN_Rep, SIEL SO#1670 TRM.xls'), skip=2)
siel2 <- siel2[,colSums(is.na(siel2))<nrow(siel2)]

colnames(siel2) <- c('sampleID', 'mass_mg', 'percentN', 'percentC', 'CN_ratio')
pp_raw <- bind_rows(siel1, siel2)

# samp_data <- read_xlsx(here::here('data/dry samples for CN analysis 2019.07.30.xlsx'), na='NA')
# samp_data <- samp_data[,2:6]
samp_data <- read_csv(here::here('data/CN_analysis_samples.csv'))
colnames(samp_data) <- c('sampleID', 'feeding', 'predatorID', 'predator_type', 'sample_type')

pp_mysamp <- pp_raw %>% 
  filter(sampleID != 'spinach') %>% 
  left_join(samp_data) %>% 
  filter(sample_type != 'silk')

pp_summ <- pp_mysamp %>% 
  group_by(predator_type, sample_type) %>% 
  summarize(mean_CN = mean(CN_ratio, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(pred_prod = paste(predator_type, sample_type)) %>% 
  select(pred_prod, mean_CN)
pp_summ[pp_summ == 'NA calib'] <- 'ghop carcass'
pp_summ[pp_summ == 'mantid feces'] <- 'mantid waste'
pp_summ[pp_summ == 'spider feces'] <- 'spider waste'


pred_prod <- c('Feces' = '#6E9B34','Prey remains' = '#AA5739')

pp <- ggplot(data = pp_summ, aes(pred_prod, mean_CN)) +
  geom_bar(stat = 'identity',  position = position_dodge()) +
  # geom_errorbar(aes(ymin = mean_CN - pred_se, ymax = avg_mass_mg + pred_se),
  # width = 0.15,
  # position = position_dodge(0.9)) +
  xlab('Predator product') +
  ylab('Average C:N') + 
  scale_fill_manual(name='Predator product',
                    labels = c('Feces', 'Prey remains'),
                    values = pred_prod) +
  ggtitle('C to N of predator products') +  # ggtitle('  ') + 
  theme_bw()
pp

#====================
# generate bar chart for comparing predator poop differences

pp_poop <- pp_mysamp %>% 
  # filter(sample_type=='remains' | sample_type == 'calib') %>%
  filter(sample_type=='feces') %>%
  group_by(predator_type, sample_type) %>% 
  summarize_at(vars(CN_ratio), list(~mean(., na.rm = TRUE), ~se(.))) %>%
  mutate(mean_CN = mean,
         se_CN = se) %>% 
  # summarize(mean_CN = mean(CN_ratio, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(pred_prod = paste(predator_type, sample_type)) %>% 
  select(pred_prod, mean_CN, se_CN)

pp_poop[pp_poop == 'NA calib'] <- 'ghop carcass'
pp_poop[pp_poop == 'mantid feces'] <- 'mantid waste'
pp_poop[pp_poop == 'spider feces'] <- 'spider waste'


pp_poop$pred_prod <- factor(c('mantid waste','spider waste'),
                            levels = c('spider waste', 'mantid waste'))
# pp_poop$pred_prod <- factor(c('ghop carcass', 'mantid remains','spider remains'),
#                             levels = c('ghop carcass', 'spider remains', 'mantid remains'))

# pp_summ$pred_prod <- factor(pp_summ$pred_prod, levels = c('ghop carcass',
#                                                           'spider remains',
#                                                           'mantid remains',
#                                                           'spider waste',
#                                                           'mantid waste'))
pp_poop

w4 <- unique(pp_poop$pred_prod)
poop_only <- ggplot(data = pp_poop, aes(pred_prod, mean_CN, fill=w4)) +
  geom_bar(stat = 'identity',  position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean_CN - se_CN, ymax = mean_CN + se_CN),
                width = 0.15,
                position = position_dodge(0.9)) +
  xlab('Predator product') +
  ylab('Average C:N') + 
  ggtitle('C to N of predator products')  + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none")


poop_only
# ggsave(paste0('results/e1_compare_waste.png'), width=5, height=4, dpi=1000)
