# bar charts for ESA august 2019
# comparing waste + remains between predators

library(tidyverse)
library(readxl)
library(viridis)
source(here::here('src/yaya_fxns.R'))
source("C:/Users/yaya/Dropbox/1 Ecologist/2 EXPERIMENTS/yaya_r_themes.R")


theme_pp <- theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
                  panel.background = element_rect(fill = "#f9f9f9",
                                                  colour = "#f9f9f9"),
                  panel.border = element_rect(color = "black", fill = NA))


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

pp_mysamp[is.na(pp_mysamp$predator_type),]$predator_type <- 'ghop'
pp_mysamp[pp_mysamp$sample_type ==  'feces',]$sample_type <- 'waste'
pp_mysamp[pp_mysamp$sample_type ==  'calib',]$sample_type <- 'carcass'

pp_mysamp <- pp_mysamp %>% 
  mutate(pred_prod = paste(predator_type, sample_type)) %>% 
  rename(ghop_fate = predator_type)

pp_summ <- pp_mysamp %>% 
  group_by(ghop_fate, sample_type) %>% 
  summarize(pred_prod = first(pred_prod),
            mean_CN_ratio = mean(CN_ratio),
            se_CN_ratio = se(CN_ratio)) %>% 
  ungroup() 

pred_prod <- c('Waste' = '#6E9B34','Prey remains' = '#AA5739')

prods2 <- viridis(4, option = 'C')

bar_x_labels_short <- c('Grasshopper', 
                        'Remains', 'Wastes',
                        'Remains', 'Wastes')


##### Figure for C:N of predator by-products
(pp <- pp_summ %>% 
    ggplot(aes(pred_prod, 
               mean_CN_ratio,
               fill = ghop_fate)) +
    geom_bar(stat = 'identity',  
             position = position_dodge()) +
    geom_errorbar(aes(ymin = mean_CN_ratio - se_CN_ratio, 
                      ymax = mean_CN_ratio + se_CN_ratio),
                  width = 0.15,
                  position = position_dodge(0.9)) +
    scale_x_discrete(labels= bar_x_labels_short) +
    labs(title = '\n C:N ratio of predator products',
         x = 'Necromass type',
         y = 'Average C:N',
         color = 'Treatment') +
    scale_fill_manual(name='Predator product',
                      labels = c('Carcass', 'Mantid', 'Spider'),
                      # values = prods2[1:5]) +
                      # values = prods2) +
                      values = viridis(4)[1:3]) + 
    # scale_fill_viridis(name = 'Animal necromass type', discrete = TRUE)+
    theme_yaya() + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))
)

my_ggsave(here::here('results/e1_CN_all_products.png'), 6, 5)

lm_cn_products <- lm(CN_ratio ~ ghop_fate + sample_type, pp_mysamp)
summary(lm_cn_products)

summary(aov(CN_ratio ~ pred_prod, pp_mysamp))
#====================
# generate bar chart for comparing predator poop differences

pp_poop <- pp_mysamp %>% 
  # filter(sample_type=='remains' | sample_type == 'calib') %>%
  filter(sample_type=='waste') %>%
  group_by(ghop_fate, sample_type) %>% 
  summarize_at(vars(CN_ratio), list(~mean(., na.rm = TRUE), ~se(.))) %>%
  mutate(mean_CN_ratio = mean,
         se_CN = se) %>% 
  # summarize(mean_CN_ratio = mean(CN_ratio, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(pred_prod = paste(ghop_fate, sample_type)) %>% 
  select(pred_prod, mean_CN_ratio, se_CN)

# pp_poop[pp_poop == 'NA calib'] <- 'ghop carcass'
# pp_poop[pp_poop == 'mantid feces'] <- 'mantid waste'
# pp_poop[pp_poop == 'spider feces'] <- 'spider waste'


pp_poop$pred_prod <- fct_relevel(c('mantid waste', 'spider waste'))
# levels = c('mantid waste', 'spider waste'))
# pp_poop$pred_prod <- factor(c('ghop carcass', 'mantid remains','spider remains'),
#                             levels = c('ghop carcass', 'spider remains', 'mantid remains'))

# pp_summ$pred_prod <- factor(pp_summ$pred_prod, levels = c('ghop carcass',
#                                                           'spider remains',
#                                                           'mantid remains',
#                                                           'spider waste',
#                                                           'mantid waste'))
pp_poop

w4 <- unique(pp_poop$pred_prod)
poop_only <- pp_poop %>% 
  ggplot(aes(pred_prod, 
             mean_CN_ratio, 
             fill = pred_prod)) +
  geom_bar(stat = 'identity',  position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean_CN_ratio - se_CN, ymax = mean_CN_ratio + se_CN),
                width = 0.15,
                position = position_dodge(0.9)) +
  xlab('Predator product') +
  ylab('Average C:N') + 
  ggtitle('\nC to N of predator products')  + 
  theme_yaya() + 
  theme(legend.position = 'none')



poop_only
ggsave(paste0('results/e1_compare_waste.png'), width=5, height=4, dpi=1000)
