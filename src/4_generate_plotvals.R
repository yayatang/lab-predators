# This is a new script for summarizing and visualizing the 
# IRGA data for poopy predators

#===== Setup: Libaries, import data ======
library(tidyverse)
library(ggpubr)
library(janitor)
library(viridis)
library(plotly)
library(gridExtra)
library(lme4)
# remotes::install_github("coolbutuseless/ggpattern")
source(here::here('src/yaya_fxns.R'))
source("C:/Users/yaya/Dropbox/1 Ecologist/2 EXPERIMENTS/yaya_r_themes.R")


#create my own theme for these plots
theme_pp <- theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
                  panel.background = element_rect(fill = "#f9f9f9",
                                                  colour = "#f9f9f9"),
                  panel.border = element_rect(color = "black", fill = NA))

theme_dyn <- theme(axis.text.x = element_text(vjust = 1, hjust=0.5),
                   panel.background = element_rect(fill = "#f9f9f9",
                                                   colour = "#f9f9f9"),
                   panel.border = element_rect(color = "black", fill = NA))

bar_x_labels <- c('Grasshopper carcass',
                  'Mantid, All products', 
                  'Mantid, Prey remains', 
                  # 'Mantid, Excreta + egesta',
                  'Mantid, Wastes', 
                  'Spider, All products', 
                  'Spider, Prey remains', 
                  # 'Spider, Excreta + egesta', 
                  'Spider, Wastes', 
                  'Control, litter only')

bar_x_labels_short <- c('Grasshopper', 
                        'All', 'Remains', 'Wastes', #'Excreta, egesta',
                        'All', 'Remains', 'Wastes') #Excreta, egesta', 
# 'Litter only')

vir_8 <- viridis(8)
vir_4 <- viridis(4)

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
## group_compute = 1 includes treatments that are ALL predator foraging by-products
## group_compute = 2 is everything else
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

#### ... ########
#### BEGIN DATA VISUALIZATION #####


#---- Figure: Plots comparing the system input masses ----

## check that the system inputs between the tubes aren't different
sys_input_compare <- imported_data %>% 
  select(trt, rep, ghop_fate, pred_prod, origin_dry, trt_added_mass) %>% 
  filter(trt != 'C') %>% 
  drop_na() %>% 
  unique() 

## BOXPLOT
sys_input_compare %>%
  ggplot(aes(trt, origin_dry, fill = ghop_fate)) +
  geom_boxplot() + 
  geom_jitter(width = 0.1)+
  scale_fill_manual(values = vir_4[1:3], name = 'Grasshopper fate') + 
  labs(title = 'Mass of original grasshopper input to each predator system',
       x = 'Treatment',
       y = 'Dry mass (g)') +
  scale_x_discrete(labels=bar_x_labels) + 
  theme_pp

## BAR PLOT
sys_input_compare %>%
  group_by(trt) %>% 
  summarize(mean_origin_dry = mean(origin_dry), 
            se_origin_dry = se(origin_dry),
            ghop_fate = first(ghop_fate)) %>% 
  ggplot(aes(trt, mean_origin_dry, fill = ghop_fate)) +
  geom_col(position = 'dodge',
           color = 'black')+
  geom_errorbar(aes( ymin = mean_origin_dry - se_origin_dry, 
                     ymax = mean_origin_dry + se_origin_dry,
                     width = 0.15)) +
  geom_jitter(data = sys_input_compare, aes(trt, origin_dry), width = 0.1)+
  # scale_fill_viridis(name = 'Grasshopper fate', discrete = TRUE) +
  scale_fill_manual(values = vir_4[1:3], name = 'Grasshopper fate') + 
  labs(title = '\nOriginal animal input size',
       x = 'Treatment',
       y = 'Dry mass (g)') +
  scale_x_discrete(labels=bar_x_labels_short) + 
  theme_yaya() + 
  theme(#plot.title = element_blank(),
    axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
    legend.position = 'none')


my_ggsave(here::here('results/4_mass_system_input.png'), 5.5, 5.5)


#---- Data analysis: stats for system inputs ----
## ANOVA for comparing the system ghop inputs per treatment
aov_sys_inputs <- aov(origin_dry ~ trt, sys_input_compare)
summary(aov_sys_inputs)
## non significant so inputs are equivalent

## mixed effect models: first, random effect
lm_sys_inputs <- glm(origin_dry ~ trt, data = sys_input_compare)
summary(lm_sys_inputs)
plot(lm_sys_inputs$residuals) ## ** not sure this passes 
par(mfrow = c(2,2))
plot(lm_sys_inputs)

dev.off()
plot(origin_dry ~ trt, sys_input_compare)

#---- Figure: treatment inputs comparison -----
### now check that the treatment input is different
sys_input_compare %>%
  group_by(trt) %>% 
  summarize(mean_trt_added_mass = mean(trt_added_mass), 
            se_trt_added_mass = se(trt_added_mass),
            ghop_fate = first(ghop_fate)) %>% 
  ggplot(aes(trt, mean_trt_added_mass, fill = ghop_fate)) +
  geom_col(position = 'dodge',
           color = 'black')+
  geom_errorbar(aes(ymin = mean_trt_added_mass - se_trt_added_mass, 
                    ymax = mean_trt_added_mass + se_trt_added_mass,
                    width = 0.15)) +
  geom_jitter(data = sys_input_compare, aes(trt, trt_added_mass), width = 0.1)+
  scale_fill_manual(values = vir_4[1:3], name = 'Grasshopper fate') + 
  labs(title = '\nAnimal necromass size added to microcosm',
       x = 'Treatment',
       y = 'Dry mass (g)') +
  scale_x_discrete(labels=bar_x_labels_short) + 
  theme_yaya() + 
  theme(#plot.title = element_blank(),
    axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))
# legend.position = 'none')


my_ggsave(here::here('results/4_mass_tube_input_jitter.png'), 7, 5.5)


# rough and dirty: making the ANOVA model
aov_tube_inputs <- aov(trt_added_mass ~ trt, sys_input_compare) 
summary(aov_tube_inputs)

par(mfrow=c(2,2))
plot(aov_tube_inputs)

ggqqplot(resid(aov_tube_inputs))
shapiro.test(resid(aov_tube_inputs)) ## this is the dry mass of inputs. viraj says not too far from normal, probably ok.

## Data analysis: lm for tube inputs comparison by treatment-----
lm_tube_inputs <- lm(trt_added_mass ~ trt, sys_input_compare)
summary(lm_tube_inputs)
plot(lm_tube_inputs)


## Data analysis: lm for tube inputs comparison by treatment-----
lm_product_inputs0 <- lm(trt_added_mass ~ ghop_fate + pred_prod, sys_input_compare)
summary(lm_product_inputs0)

lm_product_inputs <- lm(trt_added_mass ~ ghop_fate * pred_prod, sys_input_compare)
summary(lm_product_inputs)
plot(lm_product_inputs)
alias(lm_product_inputs)


anova(lm_product_inputs0, lm_product_inputs)

#==== Pre-check: Daily dynamics overall ====

# update the dataset to include cumulative values 

cumul_data <- imported_data %>% 
  mutate(ghop_fate= first(ghop_fate),
         origin_dry = first(origin_dry),
         trt_added_mass = first(trt_added_mass),
         cumul_c_gross = cumsum(infer_tube_total_daily),
         infer_daily_scaled = infer_tube_total_daily / trt_added_mass,
         cumul_c_daily_scaled = cumsum(infer_daily_scaled)) %>% 
  arrange(tube_id, exp_count)

## This chunk makes a plotly object from the ggplot to explore raw daily dynamic data from each individual tube

dyn_daily <- cumul_data %>% 
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
  theme_dyn

dyn_daily 
# ggplotly(dyn_daily) 

## This chunk makes a plotly object from the ggplot to explore dynamic CUMULATIVE C data from each individual tube

dyn_cumul <- cumul_data %>% 
  group_by(trt, rep) %>% 
  # filter(trt == 'WR') %>% 
  # View()
  ggplot(aes(x = exp_count, y = cumul_c_gross, 
             group = interaction(trt, rep))) +
  geom_line(aes(color = trt)) + 
  scale_color_viridis(name = 'Treatment', discrete = TRUE) +
  labs(title = 'C mineralized cumulatively by each tube',
       x = 'Incubation Day',
       y = 'C mineralization (mg)',
       color = 'Treatment') +
  theme_dyn

dyn_cumul
ggplotly(dyn_cumul) 

## This chunk shows the same cumulative data as above, but split between the two phases
## PHASE 1

## this is the code for the original, unsummarized cumulative data
# dyn_cumul_p1 <- cumul_data %>% 
#   group_by(trt, rep) %>% 
#   filter(phase == 1) %>% 
#   mutate(cumul_c_gross_phase = cumsum(infer_tube_total_daily)) %>% 
#   # View()
#   ggplot(aes(x = exp_count, y = cumul_c_gross_phase, 
#              group = interaction(trt, rep))) +
#   geom_line(aes(color = trt)) + 
#   scale_color_viridis(name = 'Treatment', discrete = TRUE) +
#   labs(title = 'C mineralized cumulatively by each tube, phase 1 only',
#        x = 'Incubation Day',
#        y = 'C mineralization (mg)',
#        color = 'Treatment') +
#   theme_dyn
# 
# dyn_cumul_p1
# 
# ggplotly(dyn_cumul_p1) 

## the dynamic, summarized plot of cumulative C
## PHASE 1
dyn_cumul_p1_trt <- cumul_data %>%
  group_by(trt, rep) %>%
  filter(phase == 1) %>%
  mutate(cumul_c_gross_phase = cumsum(infer_tube_total_daily)) %>%
  # View()
  ungroup() %>% 
  group_by(trt, exp_count) %>% 
  summarize(mean_cumul_c_p = mean(cumul_c_gross_phase),
            se_cumul_c_p = se(cumul_c_gross_phase),
            ghop_fate = first(ghop_fate)) %>% 
  ggplot(aes(x = exp_count, 
             y = mean_cumul_c_p, 
             group = trt)) +
  geom_line(aes(color = trt)) +
  scale_color_viridis(name = 'Treatment', discrete = TRUE) +
  labs(title = 'C mineralized cumulatively by each tube, phase 1 only',
       x = 'Incubation Day',
       y = 'C mineralization (mg)',
       color = 'Treatment') +
  theme_dyn

dyn_cumul_p1_trt
ggplotly(dyn_cumul_p1_trt)


## the dynamic, summarized plot of cumulative C
## PHASE 2
dyn_cumul_p2_trt <- cumul_data %>%
  group_by(trt, rep) %>%
  filter(phase == 2) %>%
  mutate(cumul_c_gross_phase = cumsum(infer_tube_total_daily)) %>%
  # View()
  ungroup() %>% 
  group_by(trt, exp_count) %>% 
  summarize(mean_cumul_c_p = mean(cumul_c_gross_phase),
            se_cumul_c_p = se(cumul_c_gross_phase),
            ghop_fate = first(ghop_fate)) %>% 
  ggplot(aes(x = exp_count, 
             y = mean_cumul_c_p, 
             group = trt)) +
  geom_line(aes(color = trt)) +
  scale_color_viridis(name = 'Treatment', discrete = TRUE) +
  labs(title = 'C mineralized cumulatively by each tube, phase 2 only',
       x = 'Incubation Day',
       y = 'C mineralization (mg)',
       color = 'Treatment') +
  theme_dyn

dyn_cumul_p2_trt
ggplotly(dyn_cumul_p2_trt)

grid.arrange(dyn_cumul_p1_trt, dyn_cumul_p2_trt, nrow=1)


## Data analysis: bar, end of phase plots by treatment
## same as above, in bar plot summarized form at the end of the experiment
end_cumul_p1_trt <- cumul_data %>% 
  group_by(trt, rep) %>% 
  filter(phase == 1) %>% 
  mutate(cumul_c_gross_phase = cumsum(infer_tube_total_daily)) %>% 
  ungroup() %>% 
  filter(exp_count == max(exp_count)) %>% 
  group_by(trt) %>% 
  summarize(mean_cumul_c_p = mean(cumul_c_gross_phase),
            se_cumul_c_p = se(cumul_c_gross_phase),
            ghop_fate = first(ghop_fate)) %>% 
  # View()
  ggplot(aes(x = trt, 
             y = mean_cumul_c_p, 
             fill = ghop_fate)) +
  geom_col(position = 'dodge',
           color = 'black')+
  geom_errorbar(aes( ymin = mean_cumul_c_p - se_cumul_c_p, 
                     ymax = mean_cumul_c_p + se_cumul_c_p,
                     width = 0.15)) +
  scale_fill_viridis(name = 'Treatment', discrete = TRUE) +
  scale_x_discrete(labels= bar_x_labels) +
  labs(title = 'C mineralized cumulatively by treatment, phase 1 only',
       x = 'Treatment',
       y = 'C mineralization (mg)',
       color = 'Treatment') +
  theme_pp

end_cumul_p1_trt


## This chunk shows the same cumulative data as above, but split between the two phases
## PHASE 2
end_cumul_p2_trt <- cumul_data %>% 
  group_by(trt, rep) %>% 
  filter(phase == 2) %>% 
  mutate(cumul_c_gross_phase = cumsum(infer_tube_total_daily)) %>% 
  ungroup() %>% 
  filter(exp_count == max(exp_count)) %>% 
  group_by(trt) %>% 
  summarize(mean_cumul_c_p = mean(cumul_c_gross_phase),
            se_cumul_c_p = se(cumul_c_gross_phase),
            ghop_fate = first(ghop_fate)) %>% 
  ggplot(aes(x = trt, y = mean_cumul_c_p, fill = ghop_fate)) +
  geom_col(position = 'dodge',
           color = 'black')+
  geom_errorbar(aes( ymin = mean_cumul_c_p - se_cumul_c_p, 
                     ymax = mean_cumul_c_p + se_cumul_c_p,
                     width = 0.15)) +
  scale_fill_viridis(name = 'Treatment', discrete = TRUE) +
  scale_x_discrete(labels = bar_x_labels) +
  labs(title = 'C mineralized cumulatively by treatment, phase 2 only',
       x = 'Treatment',
       y = 'C mineralization (mg)',
       color = 'Treatment') +
  theme_pp

end_cumul_p2_trt

end_diff_p2_trt <- cumul_data %>% 
  group_by(trt, rep) %>% 
  filter(phase == 2) %>% 
  mutate(cumul_c_gross_phase = cumsum(infer_tube_total_daily)) %>% 
  ungroup() %>% 
  filter(exp_count == max(exp_count)) %>% 
  group_by(trt) %>% 
  summarize(mean_cumul_c_p = mean(cumul_c_gross_phase),
            se_cumul_c_p = se(cumul_c_gross_phase),
            ghop_fate = first(ghop_fate),
            group_compute = first(group_compute),
            mean_diff_c_p = mean_cumul_c_p - 20664.72) ## need fixing to not be fixed/manually edited
# View()


# ## NEED FIXING To include new levels
# computed_predators <- end_diff_p2_trt %>% 
#   filter(group_compute == 2) %>% 
#   group_by(ghop_fate) %>% 
#   summarize(mean_diff_c_p = sum(mean_diff_c_p)/2, 
#             mean_cumul_c_p = sum(mean_cumul_c_p)/2,
#             ghop_fate = first(ghop_fate),
#             group_compute = 1) %>% 
#   mutate(trt = if_else(ghop_fate == 'mantid', 'MAdd', 'WAdd'),
#          trt = as_factor(trt))
# 
# end_diff_p2_trt  %>% 
#   
#   left_join(computed_predators) %>% 
#   View()
# ggplot(aes(x = trt, y = mean_diff_c_p, fill = ghop_fate)) +
#   geom_col(position = 'dodge',
#            color = 'black')+
#   geom_errorbar(aes( ymin = mean_diff_c_p - se_cumul_c_p, 
#                      ymax = mean_diff_c_p + se_cumul_c_p,
#                      width = 0.15)) +
#   scale_fill_viridis(name = 'Treatment', discrete = TRUE) +
#   scale_x_discrete(labels = bar_x_labels) +
#   labs(title = 'C mineralized cumulatively by treatment, phase 2 only',
#        x = 'Treatment',
#        y = 'C mineralization (mg)',
#        color = 'Treatment') +
#   theme_pp


#==== Pre-checks: Daily dynamics by tube when scaled to tube input mass, phase 1 + phase 2 ====

# take a look at the input mass by treatment
sys_input_compare %>% 
  group_by(trt) %>% 
  summarize(mean_inputs = mean(trt_added_mass))

## These data are split between phase 1 and phase 2
# phase 1 is divided by the heterotrophic inputs mass
# phase 2 is divided by the amount of litter added to each tube 
# (CURRENTLY JUST 0.5g)

#------ scaled data for phase 1, by TUBE
dyn_daily_p1_scl <- cumul_data %>% 
  group_by(trt, rep) %>% 
  filter(phase == 1) %>%  
  ggplot(aes(x = exp_count, 
             y = cumul_c_daily_scaled, 
             trt_added_mass = trt_added_mass, 
             group = interaction(trt, rep))) +
  geom_line(aes(color = trt)) + 
  scale_color_viridis(name = 'Treatment', discrete = TRUE) + #doesnt work
  labs(title = 'Phase 1 C mineralized cumulative by each tube, scaled to input mass',
       x = 'Incubation Day',
       y = expression(C~mineralization~(mg~g~dry~wt~inputs^-1)),
       # y = 'C mineralization mg per g input',
       color = 'Treatment') +
  theme_dyn

dyn_daily_p1_scl
# ggplotly(dyn_daily_p1_scl)


#------ scaled data for phase 2, by TUBE
dyn_daily_p2_scl <- cumul_data %>% 
  group_by(trt, rep) %>% 
  filter(phase == 2) %>%  
  ggplot(aes(x = exp_count, y = infer_daily_scaled, 
             group = interaction(trt, rep))) +
  geom_line(aes(color = trt)) + 
  scale_color_viridis(name = 'Treatment', discrete = TRUE) +
  labs(title = 'Phase 2 C mineralized daily by each tube, scaled to litter mass',
       x = 'Incubation Day',
       y = expression(C~mineralization~(mg~g~dry~wt~inputs^-1)),
       color = 'Treatment') +
  theme_dyn

dyn_daily_p2_scl
# ggplotly(dyn_daily_p2_scl) 


#==== Pre-checks: Daily dynamics by treatment when scaled to tube input mass, phase 1 + phase 2 ====

#------ scaled data for phase 1, same as above by TREATMENT
dyn_daily_p1_scl_trt <- cumul_data %>% 
  group_by(trt, exp_count) %>% 
  filter(phase == 1,
         trt != 'C') %>% 
  summarize(trt_cumul_scale = mean(cumul_c_daily_scaled)) %>% 
  ggplot(aes(x = exp_count, y = trt_cumul_scale, 
             group = trt)) +
  geom_line(aes(color = trt)) + 
  scale_color_manual(values = vir_8[1:7], name = 'Treatment') + 
  labs(title = 'Phase 1 C mineralized daily by treatment, scaled to input mass',
       x = 'Incubation Day',
       y = expression(C~mineralization~(mg~g~dry~wt~inputs^-1)),
       color = 'Treatment') +
  theme_dyn

dyn_daily_p1_scl_trt
# ggplotly(dyn_daily_p1_scl_trt) 

#------ scaled data for phase 2, by TREATMENT
dyn_daily_p2_scl_trt <- cumul_data %>% 
  group_by(trt, exp_count) %>% 
  filter(phase == 2,
         trt != 'C') %>% 
  summarize(trt_daily_scale = mean(infer_daily_scaled)) %>% 
  ggplot(aes(x = exp_count, y = trt_daily_scale, 
             group = trt)) +
  geom_line(aes(color = trt)) + 
  scale_color_manual(values = vir_8[1:7], name = 'Treatment') + 
  labs(title = 'Phase 2 C mineralized daily by each tube, scaled to input mass',
       x = 'Incubation Day',
       y = expression(C~mineralization~(mg~g~dry~wt~inputs^-1)),
       color = 'Treatment') +
  theme_dyn

dyn_daily_p2_scl_trt
# ggplotly(dyn_daily_p2_scl_trt) 

## for these last two sections, it may make more sense to look at the scaled respiration for phase 1, and then the total respiration for phase 2 because at that point the functional shifts should have already happened and every tube received 0.5g of litter. The comparison makes more sense withOUT scaling in phase 2.

##---- Figure: SCALED end of phase cumulative C ----
## the above data turned cumulative, and in bars
graph_title <- '\nPhase 1 mineralization, scaled to input mass'

(end_cumul_p1_scl_trt <- cumul_data %>% 
    group_by(trt, exp_count) %>% 
    filter(phase == 1, 
           trt != 'C') %>%  ## if C is filtered out, shorten the color scale
    summarize(mean_trt_cumul_scale = mean(cumul_c_daily_scaled),
              se_trt_cumul_scale = se(cumul_c_daily_scaled),
              ghop_fate = first(ghop_fate)) %>% 
    filter(exp_count == max(exp_count)) %>% 
    ggplot(aes(x = trt, y = mean_trt_cumul_scale, fill = ghop_fate)) +
    geom_col(position = 'dodge',
             color = 'black')+
    geom_errorbar(aes( ymin = mean_trt_cumul_scale - se_trt_cumul_scale, 
                       ymax = mean_trt_cumul_scale + se_trt_cumul_scale,
                       width = 0.15)) +
    scale_fill_manual(values = vir_4[1:3], name = 'Grasshopper fate',
                      labels = c('Carcass', 'Mantid', 'Spider')) + 
    scale_x_discrete(labels = bar_x_labels_short[1:7]) +
    labs(title = graph_title,
         x = 'Treatment',
         y = expression(C~mineralization~(mg~g~dry~wt~inputs^-1)),
         # y = 'C mineralization (mg g dry wt inputs ^-1)',
         color = 'Treatment') +
    theme_yaya()+
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
          axis.title = element_text(hjust=0))
)

my_ggsave(here::here('results/4_scaled_input mass.png'), 5, 5)


#==== Figure: End of exp cumulative data ====

# these are simple visualization box plots to look at the distribution of respiration across all treatments, unadjusted

# BOXPLOT

# cumul_end <- 
cumul_data %>% 
  filter(exp_count == max(exp_count)) %>% 
  ggplot(aes(x= trt, 
             y = cumul_c_gross, 
             fill = ghop_fate)) +
  geom_boxplot() + 
  geom_jitter(width = 0.1) + 
  scale_fill_viridis(name = 'Grasshopper fate', discrete = TRUE) +
  labs(title = 'End of experiment total cumulative C mineralized',
       x = 'Treatment',
       y = 'Dry mass (g)') +
  scale_x_discrete(labels = bar_x_labels) + 
  theme_pp 

# cumul_end
# ggplotly(cumul_end)

# BAR PLOT
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
  geom_hline(yintercept = 21465.50) + # previously was 21303.60 before april 7, 2021
  scale_fill_viridis(name = 'Grasshopper fate', discrete = TRUE,
                     labels = c('Carcass', 'Mantid', 'Spider', 'Control')) +
  labs(title = '', #Gross C mineralized by each treatment, over entire experiment',
       x = 'Treatment',
       y = 'C mineralization') +
  scale_x_discrete(labels= c(bar_x_labels_short, 'Litter-only')) + 
  # theme_pp
  theme_yaya() + 
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

my_ggsave(here::here('results/4_phase_end_mineralization.png'), 7, 5)



#----  Data analysis: linear models ----

## this is to test whether treatment had an effect on the final cumulative C
# the simplest model
cumul_aov <- aov(cumul_c_gross ~ trt, filter(cumul_data, exp_count == max(exp_count)) )
par(mfrow=c(2,2))
plot(cumul_aov) ## check 76, 50, 24 -- currently left in the dataset.
summary(cumul_aov)

ggqqplot(resid(cumul_aov))
shapiro.test(resid(cumul_aov)) ## normal

## posthoc
TukeyHSD(cumul_aov)

cumul_lm <- lm(cumul_c_gross ~ ghop_fate + pred_prod, filter(cumul_data, exp_count == max(exp_count)))
summary(cumul_lm)

cumul_lm_top <- lm(cumul_c_gross ~ trt, filter(cumul_data, exp_count == max(exp_count), trt == c('G','MA', 'WA')))
summary(cumul_lm_top)




### ANOVA for top level: ghop vs predators vs control
# this is to test whether there was a top level difference between ghop carcass, all mantid foraging by-products, and all spider foraging by-products
aov_cumul_pred <- aov(cumul_c_gross ~ trt, filter(cumul_data, exp_count == max(exp_count) & group_compute==1))
summary(aov_cumul_pred) ## significant

TukeyHSD(aov_cumul_pred)

ggqqplot(resid(aov_cumul_pred))
shapiro.test(resid(aov_cumul_pred)) ## normal


## LM, leveling up the model to consider ghop fate and predator products
## *** but does this question make sense? 
lm_cumul_end <- lm(cumul_c_gross ~ ghop_fate + pred_prod,
                   filter(cumul_data, exp_count == max(exp_count)))
summary(lm_cumul_end)
## assumptions check
par(mfrow=c(2,2))
plot(lm_cumul_end)

## LM,including interaction factor
lm_cumul_intrxn <- lm(cumul_c_gross ~ ghop_fate * pred_prod,
                      filter(cumul_data, exp_count == max(exp_count)))
summary(lm_cumul_intrxn)
par(mfrow=c(2,2))
plot(lm_cumul_intrxn)


#====== Figure: by phase cumulative data by treatment and phase =====

# Plots showing summarized C mineralization by treatment
# This is NOT subtracting the control.

# Phase 1
# summarized each tube's gross values at the END of the experiment
cumul_p1 <- cumul_data %>% 
  group_by(trt) %>% 
  filter(exp_count == 54) %>% 
  summarize(mean_cumul = mean(cumul_c_gross),
            se_cumul = se(cumul_c_gross),
            ghop_fate = first(ghop_fate),
            pred_prod = first(pred_prod),
            group_compute = first(group_compute),
            phase = phase)

(g_cumul_p1 <- cumul_p1 %>% 
    ggplot(aes(x = trt,
               y = mean_cumul,
               fill = ghop_fate)) +
    geom_col(position = 'dodge',
             color = 'black')+
    geom_errorbar(aes( ymin = mean_cumul - se_cumul, 
                       ymax = mean_cumul + se_cumul,
                       width = 0.15)) +
    # geom_hline(yintercept = 800.7792) + ### line for the Control treatments
    scale_fill_viridis(name = 'Grasshopper fate', discrete = TRUE,
                       labels = c('Carcass', 'Mantid', 'Spider', 'Control')) +
    labs(title = '\nGross C mineralized by each treatment, phase 1',
         x = 'Treatment',
         y = 'C mineralization') +
    scale_x_discrete(labels = bar_x_labels_short) + 
    theme_yaya() + 
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))
)

my_ggsave(here::here('results/4_phase_1_mineralization.png'), 7, 5)


### same data as above, but for phase 2 only
## summarized each tube's gross values at the END of the experiment

# this requires making a new cumulative C gross variable for phase 2 only
cumul_p2 <- cumul_data %>% 
  filter(phase == 2) %>% 
  group_by(trt, rep) %>% 
  arrange(tube_id, exp_count) %>% 
  mutate(cumul_c_phase2 = cumsum(infer_tube_total_daily)) %>% 
  ungroup() %>% 
  unique()


(g_cumul_p2 <- cumul_p2 %>% 
    group_by(trt) %>% 
    filter(exp_count == max(exp_count)) %>% 
    summarize(mean_cumul = mean(cumul_c_phase2),
              se_cumul = se(cumul_c_phase2),
              ghop_fate = first(ghop_fate),
              pred_prod = first(pred_prod),
              group_compute = first(group_compute),
              phase = phase) %>% 
    ggplot(aes(x = trt,
               y = mean_cumul,
               fill = ghop_fate)) +
    geom_col(position = 'dodge',
             color = 'black')+
    geom_errorbar(aes( ymin = mean_cumul - se_cumul, 
                       ymax = mean_cumul + se_cumul,
                       width = 0.15)) +
    # geom_hline(yintercept = 20664.72) + ### line for the Control treatments
    scale_fill_viridis(name = 'Grashopper fate', discrete = TRUE,
                       labels = c('Carcass', 'Mantid', 'Spider', 'Control')) +
    labs(title = '\nGross C mineralized by each treatment, phase 2 only',
         x = 'Treatment',
         y = 'C mineralization') +
    scale_x_discrete(labels = bar_x_labels_short) + 
    theme_yaya() + 
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))
)

my_ggsave(here::here('results/4_phase_2_mineralization.png'), 7, 5)

##### Data analysis: phase 2 end treatment differences #####
lm_pp_phase2 <- lm(cumul_c_phase2 ~ trt,  cumul_p2)
summary(lm_pp_phase2)
lm_pp_phase2_null <-lm(cumul_c_phase2 ~ 1,  cumul_p2)

anova(lm_pp_phase2, lm_pp_phase2_null)

# C respiration in each phase, arranged in one window
grid.arrange(g_cumul_p1, g_cumul_p2, nrow=1) ### THIS IS THE SAME AS AN EARLIER PLOT

## This is to set up the graph that stacks the cumulative C mineralized in phase 1 and phase 2 on top of one another
phase_ends <- bind_rows(cumul_p1, cumul_p2) %>% 
  mutate(phase = as_factor(phase)) %>% 
  unique() 

#==== Check: Graphs for the end of phase cumulative values
### graph to stack the phase 1 and phase 2 C mineralized
ends_1 <- phase_ends %>% 
  group_by(trt) %>% 
  ggplot(aes(x = trt, 
             y = mean_cumul, 
             fill = as_factor(phase))) +
  geom_col(position = "stack") +
  scale_fill_viridis(name = 'Phase', discrete = TRUE, direction = -1) +
  geom_hline(yintercept = 0) +
  labs(title = 'C mineralized in each phase, averaging each treatment gross cumulative',
       x = 'Treatment',
       y = 'C mineralization') +
  scale_x_discrete(labels= bar_x_labels) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
        panel.background = element_rect(fill = "#f9f9f9",
                                        colour = "#f9f9f9"),
        panel.border = element_rect(color = "black", fill = NA))


## these recalculated values need to be compared against the data from cumul_data directly
p_end_check1 <- cumul_data %>% 
  group_by(trt) %>% 
  filter(exp_count == 54) %>% 
  summarize(mean_cumul_p1 = mean(cumul_c_gross))
# this is the end of phase 1

p_end_check2 <- cumul_data %>% 
  group_by(trt) %>% 
  filter(exp_count == 176) %>% 
  summarize(mean_cumul_end = mean(cumul_c_gross))
# this is the end of phase 2  

## this separates the mean cumulative C data by subtracting values at the end of each phase, NOT recalculating 
p_end_check2_calc <- full_join(p_end_check1, p_end_check2) %>% 
  mutate(mean_cumul_p2 = mean_cumul_end - mean_cumul_p1) %>% 
  select(-mean_cumul_end) %>% 
  pivot_longer(!trt, names_to = 'phase', names_prefix = 'mean_cumul_p', values_to = 'mean_cumul_phase_end') %>% 
  arrange()

ends_2 <- p_end_check2_calc %>% 
  group_by(trt) %>% 
  ggplot(aes(x = trt, 
             y = mean_cumul_phase_end, 
             fill = as_factor(phase))) +
  geom_col(position = "stack") +
  scale_fill_viridis(name = 'Phase', discrete = TRUE, direction = -1) +
  geom_hline(yintercept = 0) +
  labs(title = 'C mineralized by phase, calculated at end of exp',
       x = 'Treatment',
       y = 'C mineralization') +
  scale_x_discrete(labels=c('Grasshopper', 'Mantid all', 'Mantid remains', 'Mantid excreta + egesta', 'Spider all', 'Spider remains', 'Spider excreta + egesta', 'Control')) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
        panel.background = element_rect(fill = "#f9f9f9",
                                        colour = "#f9f9f9"),
        panel.border = element_rect(color = "black", fill = NA))

ends_2

# grid.arrange(ends_1, ends_2, nrow=1)

combo_ends <- full_join(ends_1$data, ends_2$data)
View(combo_ends)

#----Figure: stacked cumulative chart, phase 2 -----
g_cumul_p2_stack <- cumul_p2 %>% 
  
  ggplot(aes(x = trt,
             y = mean_cumul,
             fill = ghop_fate)) +
  geom_col(position = 'dodge',
           color = 'black')+
  geom_errorbar(aes( ymin = mean_cumul - se_cumul, 
                     ymax = mean_cumul + se_cumul,
                     width = 0.15)) +
  # geom_hline(yintercept = 20664.72) + ### line for the Control treatments
  scale_fill_viridis(name = '', discrete = TRUE) +
  labs(title = 'Gross C mineralized by each treatment, phase 2 only',
       x = 'Treatment',
       y = 'C mineralization') +
  scale_x_discrete(labels=c('Grasshopper', 'All', 'Remains', 'Excreta, egesta',
                            'All', 'Remains', 'Excreta, egesta', 'Control')) + 
  theme_pp
g_cumul_p2

#### ... ########
#---- DIFFERENCE  data, from controls ----
#---- Pre-checks: dynamic C difference data, line plots ----

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

#=== line plot 
## to check the plot of the cumul diff data
## looking at the dynamic difference between widow wrapped and widow excreta
# only the differences, either daily or cumulative
diff_data %>%
  # filter(trt=='WE'| trt == 'WW') %>%
  ggplot(aes(x = exp_count,
             y = cumul_diff, #daily_diff
             color = trt,
             linetype = factor(rep),
             group = interaction(trt, rep))) +
  geom_line() +
  scale_color_viridis(name = '', discrete = TRUE) +
  labs(title = 'Dynamic cumulative difference from controls over time, by tube',
       x = 'Days since experiment start',
       y = 'C mineralization') +
  geom_vline(xintercept = 54) +
  theme_dyn

# same dynamic data as above, summarized
diff_data %>%
  # filter(trt=='WE'| trt == 'WW') %>%
  ungroup() %>% 
  group_by(trt, exp_count) %>% 
  summarize(mean_cumul_diff = mean(cumul_diff),
            se_cumul_diff = se(cumul_diff)) %>% 
  ggplot(aes(x = exp_count,
             y = mean_cumul_diff,
             color = trt)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_cumul_diff - se_cumul_diff,
                    ymax = mean_cumul_diff + se_cumul_diff,
                    width = 0.15)) +
  scale_color_viridis(name = 'Treatment', discrete = TRUE) +
  labs(title = 'Dynamic cumulative difference from controls over time, by treatment',
       x = 'Days since experiment start',
       y = 'C mineralization') +
  geom_vline(xintercept = 54) +
  theme_dyn

#---- Pre-checks:  end data for the cumulative difference from controls----
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
  scale_fill_viridis(name = 'Grasshopper fate', discrete = TRUE) +
  geom_hline(yintercept = 0) +
  labs(title = 'C mineralized by each treatment, cumulative difference from controls @ end',
       x = 'Treatment',
       y = 'C mineralization') +
  scale_x_discrete(labels=c('Grasshopper', 'All', 'Remains', 'Excreta, egesta',
                            'All', 'Remains', 'Excreta, egesta', 'Control')) + 
  theme_pp

#---- Pre-checks: cumulative difference in phase 1 ----
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


#---- Figure: stacked difference chart, end of exp ----

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
  ## group compute describes whether the 
  
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
  scale_fill_viridis(name = 'Predator foraging\nby-products', discrete = TRUE, option = 'magma') + 
  # scale_fill_brewer(palette="OrRd") +
  geom_hline(yintercept = 0) +
  labs(title = 'C mineralized by each treatment, cumulative difference from controls',
       x = 'Treatment',
       y = 'C mineralization') +
  scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Control')) + 
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5),
        panel.background = element_rect(fill = "#f9f9f9",
                                        colour = "#f9f9f9"),
        panel.border = element_rect(color = "black", fill = NA))



