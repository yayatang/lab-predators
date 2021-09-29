# for generating bar charts for poster in march 2019 (huji faculty)

library(readr)
library(dplyr)
library(here)
library(ggplot2)
library(viridis)
source(here::here('src/yaya_fxns.r'))
source("C:/Users/yaya/Dropbox/1 Ecologist/2 EXPERIMENTS/yaya_r_themes.R")


plot_bar <- read_csv(here::here('data/feedings/4_summ_by-product.csv')) %>% 
  rename(pred_se = std_err)
plot_bar[which(plot_bar$predator=='white widow'),]$predator <- 'spider'

prods1 <- c('feces' = '#CF8865', 'prey remains' = '#72AB97')
prods2 <- viridis(6, option = 'D')

##### Figure for comparing  predators waste size
(b <- ggplot(data = plot_bar, aes(predator, avg_mass_mg, fill=by.product)) + 
    geom_bar(stat = 'identity',  position = position_dodge()) +
    geom_errorbar(aes(ymin = avg_mass_mg - pred_se, ymax = avg_mass_mg + pred_se),
                  width = 0.15,
                  position = position_dodge(0.9)) +
    # xlab('Predator') +
    scale_x_discrete(labels= c('Mantid', 'Spider') )+ 
    ylab('Average dry mass (mg)') + 
    scale_fill_manual(name='By-product',
                      labels = c('Predator waste', 'Prey remains'),
                      values = c(prods2[5], prods2[6])) + 
    ggtitle('\nPredator by-product size') +
    theme_yaya()+ 
    theme(panel.border = element_blank(),
          axis.title.x = element_blank())
    #       panel.grid.major = element_blank(),
    #       panel.grid.minor = element_blank(), 
    #       axis.line = element_line(colour = "black"))
)

my_ggsave(here::here('results/bar_by.products.png'), 5, 5)

#=================

# end_raw <- read_csv(here::here('data/2_end_tubes.csv')) 
# 
# 
# end_tubes <- end_raw %>% 
#   group_by(trt) %>% 
#   summarize_at(vars(by_tube_diff_cumul), list(~mean(., na.rm = TRUE), ~se(.))) %>% 
#   mutate(mean_end = mean, 
#          se_end = se)
# 
# # kw = kruskal wallis
# (kw <- end_tubes %>% 
#     ggplot(end_tubes, 
#            aes(trt, 
#                mean_end, 
#                fill=trt)) + 
#     geom_bar(stat = 'identity',  position = position_dodge()) +
#     geom_errorbar(aes(ymin = mean_end - se_end, 
#                       ymax = mean_end + se_end),
#                   width = 0.15,
#                   position = position_dodge(0.9)) +
#     scale_fill_viridis(name = 'Grasshopper fate',
#                        labels = c('Carcass', 'Mantid, all products', 'Spider, all products'),
#                        discrete = T)+
#     labs(title = 'Cumulative C production by prey nutrient fate',
#          x = 'Treatment',
#          y = 'Cumulative C mineralization (g)',
#          color = 'Treatment') +
#     scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Control')) +
#     theme_npp()
# )
# 
# kw
# # ggsave(here::here('results/bar_by.products.png'), width=5, height = 4, dpi = 1000)
