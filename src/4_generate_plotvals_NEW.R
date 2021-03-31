# This is a new script for summarizing and visualizing the 
# IRGA data for poopy predators

#===== Setup: Libaries, import data ======
library(tidyverse)
library(ggpubr)
library(janitor)
library(viridis)
library(plotly)
source(here::here('src/yaya_fxns.R'))

#create my own theme for these plots
theme_pp <- theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
                  panel.background = element_rect(fill = "#f9f9f9",
                                                  colour = "#f9f9f9"),
                  panel.border = element_rect(color = "black", fill = NA))


## import big table with all relevant data
imported_data <- readRDS("results/3_data_to_graph.rds") %>% 
  ungroup() %>% 
  clean_names() %>%
  filter(trt != 'R',
         # trt != 'C',
         trt != 'WS', ## this it the treatment of only capture silk
         trt != 'WR',
         trt != 'WN') %>% ## this is for nest silk, and isn't relevant for this analysis 
  select(tube_id, trt, rep, ghop_fate, phase, phase_count, exp_count, infer_tube_total_daily, origin_dry, trt_added_mass) %>% 
  mutate(pred_prod = substr(trt, 2, 2)) %>% 
  group_by(trt, rep) %>% 
  arrange(exp_count)


# add/fix grouping variables to partially cluster bar charts
imported_data$trt <- fct_relevel(imported_data$trt, 'G', 'MA', 'MR', 'ME', 'WA', 'WW', 'WE', 'WR', 'WS', 'WN')

imported_data$group_compute <- as.numeric(nrow(imported_data))
imported_data[which(imported_data$pred_prod == '' | imported_data$pred_prod == 'A'),]$group_compute <- 1
imported_data[which(imported_data$group_compute != 1),]$group_compute <- 2
imported_data[which(imported_data$trt == 'C'),]$ghop_fate <- 'control'
imported_data$ghop_fate <- fct_relevel(imported_data$ghop_fate, 'carcass', 'mantid', 'spider', 'control')

imported_data[which(imported_data$pred_prod == 'W' | imported_data$pred_prod == 'R'),]$pred_prod <- 'remains'
imported_data[which(imported_data$pred_prod == 'A'),]$pred_prod <- 'all'
imported_data[which(imported_data$pred_prod == 'E'),]$pred_prod <- 'excreta'
imported_data[which(imported_data$pred_prod == ''),]$pred_prod <- 'none'
imported_data$pred_prod <- fct_relevel(as_factor(imported_data$pred_prod), 
                                       'none', 'all', 'remains', 'excreta')

########## Setup: Control data subset ########
## This section of code is to subset the control tubes data and get the  
# treatment average for each day.  This table will be added to the rest of the 
# IRGA data

# extract the control data, and summarize
ctrl_data <- imported_data %>% 
  filter(trt == 'C') %>% 
  select(exp_count, trt, rep, infer_tube_total_daily) %>% 
  group_by(exp_count) %>%
  arrange(exp_count) %>% 
  summarize(trt = first(trt),
            ctrl_daily_mean = mean(infer_tube_total_daily),
            ctrl_daily_se = se(infer_tube_total_daily)) %>% 
  mutate(ctrl_cumul = cumsum(ctrl_daily_mean))


#### BEGIN DATA VISUALIZATION #####


#---- Pre-checks: Plots comparing the system input masses ----

## check that the system inputs between the tubes aren't different
sys_input_compare <- imported_data %>% 
  select(trt, rep, ghop_fate, pred_prod, origin_dry, trt_added_mass) %>% 
  filter(trt != 'C') %>% 
  drop_na() %>% 
  unique() 

sys_input_compare %>%
  ggplot(aes(trt, origin_dry, fill = ghop_fate)) +
  geom_boxplot() + 
  geom_jitter(width = 0.1)+
  scale_fill_viridis(name = 'Grasshopper fate', discrete = TRUE) +
  labs(title = 'Mass of original grasshopper input to each predator system',
       x = 'Treatment',
       y = 'Dry mass') +
  scale_x_discrete(labels=c('Grasshopper carcass', 
                            'Mantid, All products', 'Mantid, Prey remains', 'Mantid, Excreta + egesta',
                            'Spider, All products', 'Spider, Prey remains', 'Spider, Excreta + egesta')) + 
  theme_pp

## ANOVA for comparing the system ghop inputs per treatment
sys_inputs_aov <- aov(origin_dry ~ trt, sys_input_compare)
summary(sys_inputs_aov)
## non significant so inputs are equivalent


#---- Pre-checks: treatment inputs comparison -----
### now check that the treatment input is different
sys_input_compare %>% 
  ggplot(aes(trt, trt_added_mass, fill = ghop_fate)) +
  geom_boxplot() + 
  geom_jitter(width = 0.1) +
  scale_fill_viridis(name = 'Grasshopper fate', discrete = TRUE) +
  labs(title = 'Mass of treatments added to microcosms',
       x = 'Treatment',
       y = 'Dry mass') +
  scale_x_discrete(labels=c('Grasshopper carcass',
                            'Mantid, All products', 'Mantid, Prey remains', 'Mantid, Excreta + egesta',
                            'Spider, All products', 'Spider, Prey remains', 'Spider, Excreta + egesta')) +
  theme_pp

# making the ANOVA model
tube_inputs_aov <- aov(trt_added_mass ~ trt, sys_input_compare) 
summary(tube_inputs_aov)

par(mfrow=c(2,2))
plot(tube_inputs_aov)

ggqqplot(resid(tube_inputs_aov))
shapiro.test(resid(tube_inputs_aov)) ## nonnormal!  this is the dry mass of inputs. viraj says not too far from normal, probably ok.


#==== Dynamics over the entire experiment ====

# update the dataset to include cumulative values 

cumul_data <- imported_data %>% 
  mutate(trt_added_mass = first(trt_added_mass),
         cumul_c_gross = cumsum(infer_tube_total_daily))

## This chunk makes a plotly object from the ggplot to explore raw daily dynamic data from each individual tube

g_check <- cumul_data %>% 
  group_by(trt, rep) %>% 
  # filter(trt == 'WR') %>% 
  # View()
  ggplot(aes(x = exp_count, y = infer_tube_total_daily, 
             group = interaction(trt, rep))) +
  geom_line(aes(color = trt)) + 
  scale_color_viridis(name = 'Treatment', discrete = TRUE) +
  labs(title = 'C mineralized daily by each tube',
       x = 'Incubation Day',
       y = 'C mineralization (mg)',
       color = 'Treatment') +
  theme(axis.text.x = element_text(vjust = 1, hjust=1),
        panel.background = element_rect(fill = "#f9f9f9",
                                        colour = "#f9f9f9"),
        panel.border = element_rect(color = "black", fill = NA))
# using custom theme bc it's temporal data, not summarized by treatment 
g_check

ggplotly(g_check) 

######## Pre-checks: Dynamics when scaled to tube input mass

# take a look at the input mass by treatment
sys_input_compare %>% 
  group_by(trt) %>% 
  filter(trt == 'MR' | trt == 'WE') %>% 
  # summarize(mean_inputs = mean(trt_added_mass)) %>% 
  View()

## These data are split between phase 1 and phase 2
# phase 1 is divided by the heterotrophic inputs mass
# phase 2 is divided by the amount of litter added to each tube 
# (CURRENTLY JUST 0.5g)

#------ scaled data for phase 1
g_check_p1 <- cumul_data %>% 
  group_by(trt, rep) %>% 
  filter(phase == 1) %>%  
  # trt != 'C') %>% 
  mutate(infer_daily_scaled = infer_tube_total_daily / trt_added_mass,
         cumul_c_daily_scaled = cumsum(infer_daily_scaled)) %>% 
  # View()
  ggplot(aes(x = exp_count, y = infer_daily_scaled, 
             group = interaction(trt, rep))) +
  geom_line(aes(color = trt)) + 
  
  scale_color_viridis(name = 'Treatment', discrete = TRUE) + #doesnt work
  labs(title = 'Phase 1 C mineralized daily by each tube, scaled to input mass',
       x = 'Incubation Day',
       y = 'C mineralization mg per g input',
       color = 'Treatment') +
  theme(axis.text.x = element_text(vjust = 1, hjust=1),
        panel.background = element_rect(fill = "#f9f9f9",
                                        colour = "#f9f9f9"),
        panel.border = element_rect(color = "black", fill = NA))
# using custom theme bc it's temporal data, not summarized by treatment 
g_check_p1

ggplotly(g_check_p1) 

#------ scaled data for phase 2
g_check_p2 <- cumul_data %>% 
  group_by(trt, rep) %>% 
  filter(phase == 2) %>%  
  # trt != 'C') %>% 
  mutate(infer_daily_scaled = infer_tube_total_daily / trt_added_mass,
         cumul_c_daily_scaled = cumsum(infer_daily_scaled)) %>% 
  # View()
  ggplot(aes(x = exp_count, y = infer_daily_scaled, 
             group = interaction(trt, rep))) +
  geom_line(aes(color = trt)) + 
  
  scale_color_viridis(name = 'Treatment', discrete = TRUE) +
  labs(title = 'Phase 2 C mineralized daily by each tube, scaled to litter mass',
       x = 'Incubation Day',
       y = 'C mineralization mg per g input',
       color = 'Treatment') +
  theme(axis.text.x = element_text(vjust = 1, hjust=1),
        panel.background = element_rect(fill = "#f9f9f9",
                                        colour = "#f9f9f9"),
        panel.border = element_rect(color = "black", fill = NA))
# using custom theme bc it's temporal data, not summarized by treatment 
g_check_p2

ggplotly(g_check_p2) 


#------ scaled data for phase 1, same as above by TREATMENT
g_check_p1_trt <- cumul_data %>% 
  group_by(trt, exp_count) %>% 
  filter(phase == 1) %>%  
  # trt != 'C') %>% 
  mutate(infer_daily_scaled = infer_tube_total_daily / trt_added_mass,
         cumul_c_daily_scaled = cumsum(infer_daily_scaled)) %>% 
  # View()
  summarize(trt_daily_scale = mean(infer_daily_scaled)) %>% 
  ggplot(aes(x = exp_count, y = trt_daily_scale, 
             group = trt)) +
  geom_line(aes(color = trt)) + 
  scale_color_viridis(name = 'Treatment', discrete = TRUE) + 
  labs(title = 'Phase 1 C mineralized daily by treatment, scaled to input mass',
       x = 'Incubation Day',
       y = 'C mineralization mg per g input',
       color = 'Treatment') +
  theme(axis.text.x = element_text(vjust = 1, hjust=1),
        panel.background = element_rect(fill = "#f9f9f9",
                                        colour = "#f9f9f9"),
        panel.border = element_rect(color = "black", fill = NA))
# using custom theme bc it's temporal data, not summarized by treatment 
g_check_p1_trt

ggplotly(g_check_p1_trt) 



### Pre-checks:  cumulative data with a boxplot ####

cumul_gg <- cumul_data %>% 
  filter(exp_count == max(exp_count)) %>% 
  ggplot(aes(x= trt, 
             y = cumul_c_gross, 
             fill = ghop_fate)) +
  geom_boxplot() + 
  geom_jitter(width = 0.1) + 
  scale_fill_viridis(name = 'Grasshopper fate', discrete = TRUE) +
  labs(title = 'End of experiment total cumulative C mineralized',
       x = 'Treatment',
       y = 'Dry mass') +
  scale_x_discrete(labels=c('Grasshopper', 
                            'Mantid, All products', 'Mantid, Prey remains', 'Mantid, Excreta + egesta',
                            'Spider, All products', 'Spider, Prey remains', 'Spider, Excreta + egesta',
                            'Litter only control')) + 
  theme_pp 

ggplotly(cumul_gg)


### ANOVA for all treatments, the simplest model
cumul_aov <- aov(cumul_c_gross ~ trt, filter(cumul_data, exp_count == max(exp_count)) )
par(mfrow=c(2,2))
plot(cumul_aov) ## check 76, 50, 24
summary(cumul_aov)

ggqqplot(resid(cumul_aov))
shapiro.test(resid(cumul_aov)) ## normal


### ANOVA for top level: ghop vs predators vs control
cumul_pred_aov <- aov(cumul_c_gross ~ trt, filter(cumul_data, exp_count == max(exp_count) & group_compute==1))
summary(cumul_pred_aov)

ggqqplot(resid(cumul_pred_aov))
shapiro.test(resid(cumul_pred_aov)) ## normal


## LM, leveling up the model to include the mass added to each microcosm
cumul_lm <- lm(cumul_c_gross ~ ghop_fate + trt_added_mass,  filter(cumul_data, exp_count == max(exp_count)))
summary(cumul_lm)
par(mfrow=c(2,2))
plot(cumul_lm)

## not running this model bc too many parameters
# cumul_fate_prod <- lm(cumul_c_gross ~ ghop_fate + trt_added_mass + pred_prod, 
#                       filter(cumul_data, exp_count == max(exp_count)))
# summary(cumul_fate_prod)


# Plot showing summarized C mineralization by treatment
# This is NOT subtracting the control.

## CUMULATIVE C mineralization by treatment
cumul_data %>% 
  group_by(trt) %>% 
  filter(exp_count == max(exp_count)) %>% 
  summarize(mean_cumul = mean(cumul_c_gross),
            se_cumul = se(cumul_c_gross),
            ghop_fate = first(ghop_fate),
            pred_prod = first(pred_prod),
            group_compute = first(group_compute)) %>%
  ggplot(aes(x = trt,
             y = mean_cumul,
             fill = ghop_fate)) +
  geom_col(position = 'dodge',
           color = 'black')+
  geom_errorbar(aes( ymin = mean_cumul - se_cumul, 
                     ymax = mean_cumul + se_cumul,
                     width = 0.15)) +
  geom_hline(yintercept = 21303.60) +
  scale_fill_viridis(name = '', discrete = TRUE) +
  labs(title = 'Gross C mineralized by each treatment',
       x = 'Treatment',
       y = 'C mineralization') +
  scale_x_discrete(labels=c('Grasshopper', 'All', 'Remains', 'Excreta, egesta',
                            'All', 'Remains', 'Excreta, egesta', 'Control')) + 
  theme_pp

## Look at treatments as a net effect on litter decomposition
## the x-axis lives on zero

# calculate the daily difference, and calculate the sum
diff_data <- imported_data %>% 
  filter(trt != 'R',
         trt != 'WS', ## this it the treatment of only capture silk
         trt != 'WR') %>% ## this is the treatment of remains without silk 
  select(trt, rep, ghop_fate, exp_count, infer_tube_total_daily,pred_prod, group_compute) %>% 
  left_join(select(ctrl_data, -trt), by = 'exp_count') %>% 
  group_by(trt, rep) %>% 
  arrange(exp_count) %>%
  mutate(daily_diff = infer_tube_total_daily - ctrl_daily_mean,
         cumul_diff = cumsum(daily_diff))

#######
## to check the plot of the cumul diff data
## looking at the difference between widow wrapped and widow excreta
# only the differences, either daily or cumulative
diff_data %>%
  filter(trt=='WE'| trt == 'WW') %>%
  ggplot(aes(x = exp_count,
             y = cumul_diff, #daily_diff
             color = trt,
             linetype = factor(rep),
             group = interaction(trt, rep))) +
  geom_line() +
  scale_fill_viridis(name = '', discrete = TRUE) +
  labs(title = 'Cumulative difference from controls over time',
       x = 'Days since experiment start',
       y = 'C mineralization') +
  geom_vline(xintercept = 54) +
  theme_pp

######

## data from the end of the experiment
diff_data %>% 
  filter(exp_count == max(exp_count)) %>% 
  group_by(trt) %>% 
  summarize(mean_cumul_diff = mean(cumul_diff),
            se_cumul_diff = se(cumul_diff),
            ghop_fate = first(ghop_fate),
            pred_prod = first(pred_prod),
            group_compute = first(group_compute)) %>%
  # View()
  ggplot(aes(x = trt, 
             y = mean_cumul_diff, 
             fill = ghop_fate)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_cumul_diff - se_cumul_diff, 
                    ymax = mean_cumul_diff + se_cumul_diff,
                    width = 0.15)) +
  scale_fill_viridis(name = '', discrete = TRUE) +
  geom_hline(yintercept = 0) +
  labs(title = 'C mineralized by each treatment, cumulative difference from controls @ end',
       x = 'Treatment',
       y = 'C mineralization') +
  scale_x_discrete(labels=c('Grasshopper', 'All', 'Remains', 'Excreta, egesta',
                            'All', 'Remains', 'Excreta, egesta', 'Control')) + 
  theme_pp

# checking the cumulative difference in phase 1
# day 53 is the last day of phase 1
diff_data %>% 
  group_by(trt, rep) %>% 
  mutate(ghop_fate = first(ghop_fate)) %>% 
  ungroup() %>% 
  filter(exp_count == 53) %>% 
  group_by(trt) %>% 
  summarize(mean_cumul_diff = mean(cumul_diff),
            se_cumul_diff = se(cumul_diff),
            ghop_fate = first(ghop_fate),
            pred_prod = first(pred_prod),
            group_compute = first(group_compute)) %>%
  # View()
  ggplot(aes(x = trt, 
             y = mean_cumul_diff, 
             fill = ghop_fate)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_cumul_diff - se_cumul_diff, 
                    ymax = mean_cumul_diff + se_cumul_diff,
                    width = 0.15)) +
  scale_fill_viridis(name = '', discrete = TRUE) +
  geom_hline(yintercept = 0) +
  labs(title = 'C mineralized by each treatment, \ncumulative difference from controls after phase 1',
       x = 'Treatment',
       y = 'C mineralization') +
  scale_x_discrete(labels=c('Grasshopper', 'All', 'Remains', 'Excreta, egesta',
                            'All', 'Remains', 'Excreta, egesta', 'Control')) + 
  theme_pp



### try to stack the summarize diff data
## which doesn't really make sense/seem necessary
## because of how the subproducts break down
diff_data %>% 
  # group_by(exp_count, trt, )
  filter(exp_count == max(exp_count)) %>% 
  group_by(trt) %>% 
  summarize(mean_cumul_diff = mean(cumul_diff),
            se_cumul_diff = se(cumul_diff),
            ghop_fate = first(ghop_fate),
            pred_prod = first(pred_prod),
            group_compute = first(group_compute)) %>% 
  # group3 = if_else(pred_prod %in% c('excreta', 'remains'), 4, 3)) %>%
  # View()
  ggplot(aes(x = ghop_fate, 
             y = mean_cumul_diff, 
             fill = pred_prod,
             group = group_compute)) +
  geom_col(position = "dodge") +
  # geom_errorbar(aes(ymin = mean_cumul_diff - se_cumul_diff, 
  #                   ymax = mean_cumul_diff + se_cumul_diff,
  #                   width = 0.15)) +
  scale_fill_viridis(name = 'Predator foraging\nby-products', discrete = TRUE) + # , option = 'magma'
  geom_hline(yintercept = 0) +
  labs(title = 'C mineralized by each treatment, cumulative difference from controls',
       x = 'Treatment',
       y = 'C mineralization') +
  scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Control')) + 
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
        panel.background = element_rect(fill = "#f9f9f9",
                                        colour = "#f9f9f9"),
        panel.border = element_rect(color = "black", fill = NA))
