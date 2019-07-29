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


# uncomment to re-functionalize vvv
# all_plots <- function(graph_data, max_p1) {

graph_data <- imported_data %>% 
  filter(trt!='R',
         trt!='WN',
         trt!='WS',
         trt!='WW')

var_to_graph <- c('infer_tube_total_daily',
                  'by_tube_total_cumul',
                  'infer_tube_diff_daily',
                  'by_tube_diff_cumul',
                  'by_trt_daily_mean',
                  'by_trt_cumul_mean')
se_to_graph <- c(rep(c('ctrl_se'),4),
                 'by_trt_daily_se',
                 'by_trt_cumul_se')
graph_group <- c(rep(c('tubeID'),4), rep(c('trt'),2))
y_titles <- rep(c('Daily CO2-C','Cumulative CO2-C'),3)
plot_titles <- c('Daily CO2 Total by Tube',
                 'Cumulative CO2 Total by tube',
                 'Daily CO2 difference from control, by tube',
                 'Cumulative CO2 by tube',
                 'Daily CO2 difference by treatment, adjusted',
                 'Cumulative CO2 by treatment, biomass adjusted')
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
# NEW!!!!!
#   7 = daily total by treatment mean
#   8 = cumul total(gross) by treatment mean
#   
#   infer_tube_total_daily
#   infer_tube_diff_daily
#   by_tube_diff_cumul
#   by_tube_total_cumul
#   by_trt_daily_mean (+ by_trt_daily_se)
#   by_trt_cumul_mean (+ by_trt_cumul_se)

# all_plots <- htmltools::tagList()

# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")

for (i in seq_along(dynamic_data)){
  # # i is the type of graph, according to the titles above
  # i <- 1
  selected_data <- graph_data %>% 
    select(tube_num, tubeID, exp_count, trt, rep, !!dynamic_data$var_to_graph[[i]], 
           !!dynamic_data$se_to_graph[[i]], !!dynamic_data$graph_group[[i]], ghop_fate)
  renamed_data <- selected_data %>% 
    rename(graph_yvar = !!dynamic_data$var_to_graph[[i]],
           graph_se = !!dynamic_data$se_to_graph[[i]])

  if (i >= 5) {
    plot_data <- renamed_data %>% 
      ungroup() %>% 
      filter(rep==1) %>% 
      rename(graph_unit = trt) %>% 
      select(-tubeID, -rep)
    
  } else {
    plot_data <- renamed_data %>%
      rename(graph_unit = tubeID) %>% 
      select(-trt)}
  
  
  any_plot <- ggplot(plot_data, aes(exp_count, graph_yvar, color=graph_unit)) +
    # facet_grid(~phase, scales="free") +
    geom_vline(xintercept=max_p1, color="grey", size = 0.3) +
    geom_hline(yintercept=0) +
    geom_line(size=0.5, aes(linetype = ghop_fate)) +
    # geom_point(size=1) +
    # geom_errorbar(aes(ymin = graph_yvar - graph_se,
    #     ymax = graph_yvar + graph_se),
    # width=0.3) +
    labs(x="Experimental days lapsed", y=dynamic_data$y_titles[[i]]) +
    ggtitle(paste(dynamic_data$plot_titles[[i]])) +
    # scale_color_manual(values = cbbPalette) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.border = element_blank(),
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(), 
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  
  # any_plot$labels$fill <- "Soil input treatment"
  
  any_plot
  ggsave(paste0('results/2019.07_',i,'_by_', dynamic_data$graph_group[i], '.png'), width=5, height=4, dpi=1000)
  
  #==== make and save the interactive ggplot aka ggplotly
  any_plotly <- ggplotly(any_plot)
  any_plotly
  save_path <- paste0(here::here(),'/results/2019.07_plotly_',i,'_by_', dynamic_data$graph_group[i],'.html')
  htmlwidgets::saveWidget(as_widget(any_plotly), save_path)
}


# all_plots[[i]] <- as_widget(ggplotly(any_plot))
# all_plots[[i]] <- any_plot
# }
# }

# exec(all_plots, ggplotly)