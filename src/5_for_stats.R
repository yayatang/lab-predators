# 5_ get stats
library(tidyverse)
library(dunn.test)

for_stats <- readRDS(here::here('results/4_tubes_by-phase.rds'))

end_tubes <- for_stats %>% 
  filter(exp_count==174,
         trt=='MA'|trt=='G'|trt=='WA')

kruskal.test(by_tube_total_cumul ~ trt, data = end_tubes) 
# dunnTest(by_tube_total_cumul ~ trt, data = end_tubes, method="bh")
dunn.test(by_tube_total_cumul ~ trt, data = end_tubes, method="bh")
# summ_barplot <- summarise(end_tubes, by=trts)


ggplot(end_tubes, 
       aes(x=trt, y=by_tube_total_cumul)) +
  geom_point(aes(color = trt))

# write_csv(end_tubes, here::here('results/5_end_tubes.csv'))
