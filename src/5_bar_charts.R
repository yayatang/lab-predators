# for generating bar charts for poster

library(readr)
library(dplyr)
library(here)
library(ggplot2)

plot_bar <- read_csv(here::here('data/feedings/4_summ_by-product.csv')) %>% 
  rename(pred_se = std_err)

b <- ggplot(data = plot_bar, aes(predator, avg_mass_mg, fill=by.product))

prods <- c('feces' = '#C57751', 'prey remains' = '#70AD47')

b + 
  geom_bar(stat = 'identity',  position = position_dodge()) +
  geom_errorbar(aes(ymin = avg_mass_mg - pred_se, ymax = avg_mass_mg + pred_se),
                width = 0.15,
                position = position_dodge(0.9)) +
  xlab('Predator') +
  ylab('Average dry mass') + 
  scale_fill_manual(name='By-product',
                    labels = c('Feces', 'Prey remains'),
                    values = prods) + 
  ggtitle('Foraging by-products after predator consumption') +
  # ggtitle('  ') + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))
  
ggsave(here::here('results/bar_chart.png'), width=5, height = 4, dpi = 1000)

