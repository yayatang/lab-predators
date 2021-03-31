library(tidyverse)
library(plotly)
library(ggplot2)
source(here::here('src/yaya_fxns.R'))

tube_vals <- readRDS("results/4_tubes_to_plot.rds") %>% 
  ungroup()
trt_vals <- readRDS('results/4_trts_to_plot.rds')
trts <- unique(tube_vals$trt)
max_p1 <- get_phase1_max(tube_vals)

#================
# exploratory graph making function
scatter_fun <- function(x, y, dat) {
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")
  
  ggplot(dat, aes(x = .data[[x]], y = .data[[y]]), color=trt) +
    geom_point() +
    geom_line(size=0.5, aes(linetype = trt)) +
    scale_color_manual(values = cbbPalette) +
    # theme_bw() +
    labs(x = x,
         y = y)
}

#==============================================================
# 1. comparing both predators products to each other

filt_data <- tube_vals %>% 
  filter(trt == 'G' | trt == 'MA' | trt == 'WA')

#====
# DAT INTERLUDE
# data hard-coded into scatter_fun

final_cumul_only <- filt_data %>%
  # select(tube_num, exp_count, tubeID, trt, rep, ghop_fate, origin_dry, trt_added_mass, by_tube_diff_cumul, by_tube_total_cumul, by_trt_cumul_mean) %>%
  filter(exp_count==max(exp_count))
# dat <- final_cumul_only

dat_test <- final_cumul_only

response <- names(dat_test)[8:11]
expl <- names(dat_test)[c(2,7)]

response <- set_names(response)
expl <- set_names(expl)

scatter_fun('origin_dry', 'trt_added_mass', dat_test)

#====
response <- names(dat_test)[17:25]
# expl <- names(dat)[c(2,6, 10, 11)]
expl <- names(dat_test)[2]

response <- set_names(response)
expl <- set_names(expl)

#=====
# scatter_fun('exp_count', 'by_trt_cumul_mean')
# response_plots <- map(response, ~scatter_fun(.x, 'exp_count'))
# ecount_plots <- map(expl, ~scatter_fun(.x, 'exp_count'))
# 
# response_plots <- map(exp_count, ~scatter_fun(.x, 'exp_count'))
# 
# all_plots <- map(response, 
#                  ~map(expl, scatter_fun,))

#====
daily_plot <- function(graph_unit, graph_data, max_p1, trt_to_plot) {
  # unit 1 = tube
  # unit 2 = treatment
  
  if(graph_unit==1){
    graph_data <- filter(graph_data, trt==trt_to_plot)
    
    plot_by_tube <- ggplot(graph_data, aes(exp_count, infer_tube_diff_daily, color=tubeID)) +
      # facet_grid(~phase, scales="free") +
      geom_vline(xintercept=max_p1) +
      geom_hline(yintercept=0) +
      geom_line(size=0.5) +
      geom_point(size=0.7) +
      geom_errorbar(aes(ymin=infer_tube_diff_daily-tube_se, ymax=infer_tube_diff_daily+tube_se), width=0.3) +
      labs(x="Experimental days lapsed", y="Daily CO2-C") +
      ggtitle(paste('Daily diff CO2-C values by tube'))
    ggplotly(plot_by_tube)
  } else {
    plot_by_treatment <- ggplot(graph_data, aes(exp_count, by_trt_cumul_mean, color=trt)) +
      # facet_grid(~phase, scales="free") +
      geom_vline(xintercept=max_p1) +
      geom_hline(yintercept=0) +
      geom_line(aes(group=trt), size=0.5) +
      geom_point(size=0.7) +
      geom_errorbar(
        aes(ymin = by_trt_cumul_mean - by_trt_cumul_se, 
            ymax = by_trt_cumul_mean + by_trt_cumul_se), width=0.3) +
      labs(x="Experimental days lapsed", y="cumul CO2-C") +
      ggtitle(paste('Daily diff CO2-C values by treatment'))
    ggplotly(plot_by_treatment)
  }
}

#====
# graph_scale key
# 1 = by tube, for one treatment
# 2 = by treatment
graph_scale <- 2
trt_to_plot <- 'MA'

if(graph_scale==2){
  user_input <- readline("Which treatment?")
  trt_to_plot <- as.character(user_input)}

filtered_data <- tube_vals %>% 
  filter(trt != 'R')
daily_plot(graph_scale, filtered_data, max_p1, trt_to_plot)

# scatter_fun('exp_count', 'infer_tube_diff_daily', filtered_data)
# trt_tube_plots <- map(filtered_data, ~scatter_fun(.x, 'infer_tube_diff_daily', filtered_data))


#====

# uncomment to re-functionalize
# all_plots <- function(graph_data, max_p1) {

graph_data <- tube_vals %>% 
  filter(trt!='R',
         trt!='WN')

var_to_graph <- c('infer_tube_total_daily',
                  'by_tube_total_cumul',
                  'infer_tube_diff_daily',
                  'by_tube_diff_cumul',
                  'by_trt_daily_mean',
                  'by_trt_cumul_mean',
                  'by_trt_daily_gross',
                  'by_trt_cumul_gross')
se_to_graph <- c(rep(c('tube_se'),4),
                 rep(c('by_trt_daily_se','by_trt_cumul_se'), 2))
graph_group <- c(rep(c('tubeID'),4), rep(c('trt'),4))
y_titles <- rep(c('Daily CO2-C per gram ghop','Cumulative CO2-C per gram ghop'),4)
plot_titles <- c('Tube daily gross CO2', 
                 'Tube cumulative gross CO2',
                 'Tube daily net CO2',
                 'Tube cumulative net CO2',
                 'Treatment daily gross CO2, adjusted',
                 'Treatment cumulative gross CO2, adjusted',
                 'Treatment daily net CO2, adjusted',
                 'Treatment cumulative net CO2, adjusted')
dynamic_data <- tibble(var_to_graph, se_to_graph, graph_group, y_titles, plot_titles)

# graph_data
#   exp_count
#   var to graph
#   tubeID
#   se var
#   y axis title
#   graph overall title
#   
#   1 = daily total by tube
#   2 = cumul total by tube
#   3 = daily diff by tube
#   4 = cumul diff by tube
#   5 = daily diff by treatment mean
#   6 = cumul diff by treatment mean
# NEW!!!
#   7 = daily gross by treatment mean
#   8 = cumul gross by treatment mean
# //END NEW!!!
#     
#   infer_tube_total_daily
#   infer_tube_diff_daily
#   by_tube_diff_cumul
#   by_tube_total_cumul
#   by_trt_daily_mean (+ by_trt_daily_se)
#   by_trt_cumul_mean (+ by_trt_cumul_se)
#   by_trt_daily_gross (+ by_trt_daily_se)
#   by_trt_cumul_gross (+ by_trt_cumul_se)

# all_plots <- htmltools::tagList()

for (i in seq_along(length(dynamic_data))){
  
  # # i is the type of graph, according to the titles above
  # i <- 1
  selected_data <- graph_data %>% 
    select(tubeID, exp_count, trt, rep, !!dynamic_data$var_to_graph[[i]], 
           !!dynamic_data$se_to_graph[[i]], !!dynamic_data$graph_group[[i]], ghop_fate)
  renamed_data <- selected_data %>% 
    rename(graph_yvar = !!dynamic_data$var_to_graph[[i]],
           graph_se = !!dynamic_data$se_to_graph[[i]])
  
  if (i >= 5) {
    plot_data <- renamed_data %>% 
      ungroup() %>% 
      filter(rep==1) %>% 
      rename(Treatment = trt) %>% 
      select(-tubeID, -rep)
    
  } else {
    plot_data <- renamed_data %>%
      rename(graph_unit = tubeID) %>% 
      select(-trt)}
  
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")
  
  any_plot <- ggplot(plot_data, aes(exp_count, graph_yvar, color=trt)) +
    # facet_grid(~phase, scales="free") +
    geom_vline(xintercept=max_p1, color="grey", size = 0.3) +
    geom_hline(yintercept=0) +
    geom_line(size=0.5) + #, aes(linetype = animal_group)) +
    # geom_point(size=1) +
    # geom_errorbar(aes(ymin = graph_yvar - graph_se,
    #     ymax = graph_yvar + graph_se),
    # width=0.3) +
    labs(x="Experimental days lapsed", y=dynamic_data$y_titles[[i]]) +
    ggtitle(paste(dynamic_data$plot_titles[[i]])) +
    scale_color_manual(values = cbbPalette) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.border = element_blank(),
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(), 
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  
  # any_plot$labels$fill <- "Soil input treatment" 
  
  any_plot
  ggsave(paste0('results/2019.08.04_',i,'_by_', dynamic_data$graph_group[i], '_adjusted.png'), width=5, height=4, dpi=1000)
  
  ggplotly(any_plot)
  htmlwidgets::saveWidget(any_plot, paste0(here::here('results/'),
                                           today(), 
                                           'plotly_',
                                           dynamic_data$graph_group[i]))
  
  
  #   all_plots[[i]] <- as_widget(ggplotly(any_plot))
  #   all_plots[[i]] <- any_plot
}

# invoke(all_plots, ggplotly)