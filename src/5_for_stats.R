# 5_ get stats
library(tidyverse)

for_stats <- readRDS(here::here('results/4_tubes_by-phase.rds'))

end_tubes <- for_stats %>% 
  filter(exp_count==174,
         trt=='MA'|trt=='G'|trt=='WA')

kruskal.test(by_tube_total_cumul ~ trt, data = end_tubes) 

# summ_barplot <- summarise(end_tubes, by=trts)

barplot(end_tubes$by_tube_total_cumul, data = end_tubes)

write_csv(end_tubes, here::here('results/5_end_tubes.csv'))
