library(readxl)

library(tidyverse)


protein_raw <- read_xlsx(here::here('data/protein analysis/Gideons protein summary.xlsx'))

colnames(protein_raw) <- c('vial_num', 'sampleID', 'mg_subsample', 
                           'lowry_protein_IGG', 'bradford_protein_IGG',
                           'lowry_protein_percent', 'bradford_protein_percent')
protein_raw[which(protein_raw$sampleID == 'F2 S20'),]$sampleID <- 'F2 S20 PR'


protein_data  <-  protein_raw %>% 
  separate(sampleID, c("feeding", "ghop_fate_ID", "prey_remains")) %>% 
  mutate(ghop_fate = if_else(substr(ghop_fate_ID, 1, 1)=='G', 'calib', 'spider remains')) %>% 
  select(-vial_num)

protein_calib <- protein_data %>% 
  filter(ghop_fate == 'calib')
hist(protein_calib$lowry_protein_percent)
hist(protein_calib$bradford_protein_percent)

protein_remains <- protein_data %>% 
  filter(ghop_fate == 'spider remains')
hist(protein_remains$lowry_protein_percent)
hist(protein_remains$bradford_protein_percent)

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
