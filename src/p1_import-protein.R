# this script appears to process protein data from Shawn

library(readxl)
library(tidyverse)
library(ggpubr)

protein_raw <- read_xlsx(here::here('data/protein analysis/Gideons protein summary.xlsx'))

colnames(protein_raw) <- c('vial_num', 'sampleID', 'mg_subsample', 
                           'lowry_protein_IGG', 'bradford_protein_IGG',
                           'lowry_protein_percent', 'bradford_protein_percent')
protein_raw[which(protein_raw$sampleID == 'F2 S20'),]$sampleID <- 'F2 S20 PR'


protein_data  <-  protein_raw %>% 
  separate(sampleID, c("feeding", "ghop_fate_ID", "prey_remains")) %>% 
  mutate(ghop_fate = if_else(substr(ghop_fate_ID, 1, 1)=='G', 'calib', 'spider remains')) %>% 
  group_by(ghop_fate) %>% 
  mutate(bradford_se = se(bradford_protein_percent),
         bradford_avg = mean(bradford_protein_percent)) %>% 
  ungroup() %>% 
  select(-vial_num)

protein_calib <- protein_data %>% 
  filter(ghop_fate == 'calib')
hist(protein_calib$lowry_protein_percent)
hist(protein_calib$bradford_protein_percent)

protein_remains <- protein_data %>% 
  filter(ghop_fate == 'spider remains')
hist(protein_remains$lowry_protein_percent)
hist(protein_remains$bradford_protein_percent)

#--------------
# produce bar chart comparing grasshopper carcass protein to spider remains

prod_colors <- c('calib' = '#277552', 'spider remains' = '#913059')

protein_summary <- protein_data %>% 
  group_by(ghop_fate) %>% 
  summarize_at(vars(bradford_protein_percent, lowry_protein_percent), list(~mean(., na.rm = TRUE), ~se(.))) %>% 
  rename(bradford_mean = bradford_protein_percent_mean,
         lowry_mean = lowry_protein_percent_mean,
         bradford_se = bradford_protein_percent_se,
         lowry_se = lowry_protein_percent_se) %>% 
  ungroup()

bar_bradford <- ggplot(data = protein_summary, aes(ghop_fate, bradford_mean, 
                                           fill=ghop_fate)) + 
  geom_bar(stat = 'identity',  position = position_dodge()) +
  geom_errorbar(aes(ymin = bradford_mean - bradford_se, 
                    ymax = bradford_mean + bradford_se),
                width = 0.15,
                position = position_dodge(0.9)) +
  xlab('Soil input type') +
  ylab('Protein percentage') + 
  scale_fill_manual(values = prod_colors) +
                    # name='Input type',
                    # labels = c('Grasshopper carcass', 'Spider prey remains')) + 
  scale_x_discrete(labels = c('Grasshopper carcass', 'Spider prey remains')) +
  ggtitle('Protein content for soil inputs') +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust=0.5),
        legend.position = "none")

bar_bradford
ggsave(here::here('results/protein_barplots_bradford.png'), width=5, height = 4, dpi = 1000)


#--------------
png(file=here::here('results/protein_boxplots_bradford.png'), width = 800, height = 700)
geom_boxplot(protein_calib$bradford_protein_percent, protein_remains$bradford_protein_percent,
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