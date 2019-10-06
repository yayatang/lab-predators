# for generating bar charts for poster

library(readr)
library(dplyr)
library(here)
library(ggplot2)

plot_bar <- read_csv(here::here('data/feedings/4_summ_by-product.csv')) %>% 
  rename(pred_se = std_err)
plot_bar[which(plot_bar$predator=='white widow'),]$predator <- 'spider'

prods <- c('feces' = '#CF8865', 'prey remains' = '#72AB97')

b <- ggplot(data = plot_bar, aes(predator, avg_mass_mg, fill=by.product)) + 
  geom_bar(stat = 'identity',  position = position_dodge()) +
  geom_errorbar(aes(ymin = avg_mass_mg - pred_se, ymax = avg_mass_mg + pred_se),
                width = 0.15,
                position = position_dodge(0.9)) +
  xlab('Predator') +
  ylab('Average dry mass') + 
  scale_fill_manual(name='By-product',
                    labels = c('Predator waste', 'Prey remains'),
                    values = prods) + 
  ggtitle('Foraging by-products after predator consumption') +
  # ggtitle('  ') + 
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))

ggsave(here::here('results/bar_by.products.png'), width=5, height = 4, dpi = 1000)

#=================

end_raw <- read_csv(here::here('data/2_end_tubes.csv')) 

end_tubes <- end_raw %>% 
  group_by(trt) %>% 
  summarize_at(vars(by_tube_diff_cumul), list(~mean(., na.rm = TRUE), ~se(.))) %>% 
  mutate(mean_end = mean, 
         se_end = se)

kw <- ggplot(data = end_tubes, aes(trt, mean_end, fill=trt)) + 
  geom_bar(stat = 'identity',  position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_end - se_end, ymax = mean_end + se_end),
                width = 0.15,
                position = position_dodge(0.9)) +
  xlab('Treatment') +
  ylab('Avg cumulative CO2 production') + 
  ggtitle('Cumulative CO2 production difference by prey nutrient fate') +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))

kw
ggsave(here::here('results/bar_by.products.png'), width=5, height = 4, dpi = 1000)
