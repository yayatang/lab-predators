library(tidyverse)
library(plotly)
source(here::here('src/yaya_fxns.R'))

imported_data <- readRDS("results/3_data_to_graph.rds") %>% 
  filter(trt != 'R')
trt_key <- unique(imported_data$trt)
max_p1 <- get_phase1_max(imported_data)

#un comment to make daily plots by tube
# for one plot with conditional to make it tube vs trt
daily_plot <- function(graph_unit, graph_data, max_p1, trt_to_plot) {
  # unit 1 = tube
  # unit 2 = treatment
  
  if(graph_unit==1){
    #should be replaced with "map" function to plotly all treatments
    
    graph_data <- filter(graph_data, trt==trt_to_plot)
    
    plot_by_tube <- ggplot(graph_data, aes(exp_count, infer_tube_diff_daily, color=tubeID)) +
      # facet_grid(~phase, scales="free") +
      geom_vline(xintercept=max_p1) +
      geom_hline(yintercept=0) +
      geom_line(size=0.5) +
      geom_point(size=0.7) +
      geom_errorbar(aes(ymin=infer_tube_diff_daily-ctrl_se, ymax=infer_tube_diff_daily+ctrl_se), width=0.3) +
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
      ggtitle(paste('Cumulative diff CO2-C values by treatment'))
    ggplotly(plot_by_treatment)
  }
}
# user_input <- readline("Which treatment?")
# trt_to_plot <- as.character(user_input)

trt_to_plot <- 'WS'
send_unit <- 1
daily_plot(send_unit, imported_data, max_p1, trt_to_plot)
# 
# # trying to automate with purrr
# diff_plots <- imported_data %>% 
#   split(.$trt) %>% 
#   map(~daily_plot(send_unit, ., max_p1, trt_to_plot))
