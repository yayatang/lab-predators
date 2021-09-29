# this script appears to process protein data from Shawn

library(readxl)
library(tidyverse)
library(ggpubr)
library(janitor)
source("C:/Users/yaya/Dropbox/1 Ecologist/2 EXPERIMENTS/yaya_r_themes.R")


protein_raw <- read_xlsx(here::here('data/protein analysis/Gideons protein summary.xlsx'))

colnames(protein_raw) <- c('vial_num', 'sampleID', 'mg_subsample', 
                           'lowry_protein_IGG', 'bradford_protein_IGG',
                           'lowry_protein_percent', 'bradford_protein_percent')
protein_raw[which(protein_raw$sampleID == 'F2 S20'),]$sampleID <- 'F2 S20 PR'


protein_data <- protein_raw %>% 
  clean_names() %>% 
  select(sample_id, bradford_protein_igg, bradford_protein_percent) %>% 
  separate(sample_id, c("feeding", "ghop_fate_id", "pred_prod")) %>% 
  mutate(ghop_fate = if_else(substr(ghop_fate_id, 1, 1)=='G', 'calib', 'spider')) %>% 
  group_by(ghop_fate) %>% 
  mutate(bradford_se = se(bradford_protein_percent),
         bradford_avg = mean(bradford_protein_percent)) %>% 
  ungroup()
protein_data[which(is.na(protein_data$pred_prod)),]$pred_prod <- 'carcass'
protein_data[which(protein_data$pred_prod == 'PR'),]$pred_prod <- 'remains'



(protein_calib <- protein_data %>% 
    ggplot(aes(pred_prod,
               bradford_protein_percent,
               fill = pred_prod)) + 
    geom_boxplot()
)
#--------------
# produce bar chart comparing grasshopper carcass protein to spider remains

# prod_colors <- c('calib' = '#277552', 'spider remains' = '#913059')
prod_colors <- viridis(4, option = "D")


protein_data %>% 
  group_by(ghop_fate) %>% 
  summarize(mean_bradford_protein_percent = mean(bradford_protein_percent),
            se_bradford_protein_percent = se(bradford_protein_percent)) %>% 
  ungroup() %>% 
  ggplot(aes(ghop_fate, 
             mean_bradford_protein_percent, 
             fill = ghop_fate)) + 
  geom_bar(stat = 'identity',  position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_bradford_protein_percent - se_bradford_protein_percent, 
                    ymax = mean_bradford_protein_percent + se_bradford_protein_percent),
                width = 0.15) +
  scale_fill_manual(values = c(viridis(4)[1], viridis(4)[3])) +
  # name='Input type',
  # labels = c('Grasshopper carcass', 'Spider prey remains')) + 
  scale_x_discrete(name = 'Grasshopper\nfate',
                   labels = c('Grasshopper\ncarcass', 'Spider\nprey remains')) +
  labs(title = '\n Protein content',
       x = 'Necromass type',
       y = '% protein (Bradford)',
       color = 'Treatment') +
  theme_yaya() + 
  theme(axis.title.x = element_blank(),
        legend.position = 'none')


my_ggsave(here::here('results/protein_barplots_bradford.png'), 4, 4)



#-------------- are these old DFs?
png(file=here::here('results/protein_boxplots_bradford.png'), width = 800, height = 700)
ggboxplot(protein_calib$bradford_protein_percent, protein_remains$bradford_protein_percent,
          main = "Protein content",
          at = c(1, 3),
          names = c('Grasshopper carcass', 'Spider prey remains'),
          xlab = "Sample type + protocol",
          ylab = "Percent protein",
          ylim = c(0, 55),
          las = 0
)
dev.off()

#--------------
protein_summary <- protein_data %>% 
  split(.$ghop_fate) %>% 
  map(summary)


# protein_data %>% 
#   group_by(ghop_fate) %>%
#   # mutate(count = n()) %>% 
#   summarise(
#     mean = mean(lowry_protein_percent, na.rm = TRUE),
#     sd = sd(lowry_protein_percent, na.rm = TRUE),
#     median = median(lowry_protein_percent, na.rm = TRUE),
#     IQR = IQR(lowry_protein_percent, na.rm = TRUE)
#     )

# kruskal.test(by_tube_total_cumul ~ trt, data = to_stat)
# pairwise.wilcox.test(to_stat$by_tube_total_cumul, to_stat$trt,
# p.adjust.method = "BH")