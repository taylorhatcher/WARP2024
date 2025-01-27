# Taylor M. Hatcher 
# This script visualizes the constant TPC data from Joel Kingsolver in 1999 and recent constant TPC data 
#
# Load required libraries 
library(ggplot2)
library(data.table)
library(dplyr)
library(patchwork)
library(reshape2)
library(viridis)
library(tidyverse)
library(lubridate)

# set working directory to github repository
setwd("~/Desktop/Repos/WARP2024/Data")

tpcvis <- read.csv("PastPresentFilteredConstantTpc2024.csv")

# Filter out current data set
tpcvis_current <- tpcvis %>%
  filter(time.per == "current")

# Plot rgrlog growth rate for 2024 constant tpc data set
current_plot_log <- ggplot(tpcvis_current, aes( x = temp, y = rgrlog, color = mom)) +
  geom_point() +
  geom_line(aes(group = mom), alpha = 0.7) +
  facet_grid(durbin ~ instar) +
  theme_bw() +
  xlab("Temperature (C)") +
  ylab("RGR (mg/mg/h") +
  ggtitle("Present Data (2024)") +
  ylim(0, 0.20) +
  scale_color_viridis(discrete = TRUE)
  print(current_plot)
  
# Plot rgrarith growth rate for 2024 constant tpc data set
current_plot_arith <- ggplot(tpcvis_current, aes( x = temp, y = rgrarith, color = mom)) +
  geom_point() +
  geom_line(aes(group = mom), alpha = 0.7) +
  facet_grid(durbin ~ instar) +
  theme_bw() +
  xlab("Temperature (C)") +
  ylab("Arith. Growth Rate") +
  ggtitle("TPC Constant Present Data (2024)") +
  ylim(0, 12.00) +
  scale_color_viridis(discrete = TRUE) 
  print(current_plot_arith)

# Combined plots comparing log scale vs arith scale for current data
  combined_current_scale_comparison_plot <- current_plot_log + current_plot_arith + plot_layout(ncol = 1)
  print(combined_current_scale_comparison_plot)  
  
  
  tpc.plot <- ggplot(tpc[tpc$dur %in% c(6,24),], aes(x=temp, y=rgr, color=time.per)) +
    geom_point(alpha=0.5) +
    #geom_line(aes(group = mom, color = time.per), alpha = 0.7) + # lineage lines
    facet_grid(dur ~ instar) +
    theme_bw() +
    xlab("Temperature (C)") +
    ylab("RGR (mg/mg/h)") +
    ggtitle("2024 vs. 1999 Constant TPC") +
    ylim(-0.02, 11)+
    scale_color_manual(values = c("current" = "#EE6A50", "past" = "#7AC5CD"))
  print(tpc.plot)
  setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
  pdf("2024ConstantTPCComparison")
  combined_plot
  dev.off()
  
# Filter out past/historic data set
  tpcvis_past <- tpcvis %>%
    filter(time.per == "past")
  
# Plot rgrlog growth rate for 1999 constant tpc data set
    historic_plot_log <- ggplot(tpcvis_past, aes( x = temp, y = rgrlog, color = mom)) +
    geom_point() +
    geom_line(aes(group = mom), alpha = 0.7) +
    facet_grid(durbin ~ instar) +
    theme_bw() +
    xlab("Temperature (°C)") +
    ylab("RGR (mg/mg/h") +
    ggtitle("Log Scale Historic Data (1999)") +
    ylim(0, 0.20) +
    scale_color_viridis(discrete = TRUE)
  print(historic_plot_log)

# Plot rgrarith growth rate for 1999 constant tpc data set 
  historic_plot_arith <- ggplot(tpcvis_past, aes( x = temp, y = rgrarith, color = mom)) +
    geom_point() +
    geom_line(aes(group = mom), alpha = 0.7) +
    facet_grid(durbin ~ instar) +
    theme_bw() +
    xlab("Temperature (°C)") +
    ylab("Arith RGR (mg/mg/h)") +
    ggtitle("Arith Scale Historic Data (1999)") +
    ylim(0, 12.00) +
    scale_color_viridis(discrete = TRUE)
    print(historic_plot_arith)
    
# Combine both scale plots for historic 1999 data set    
combined_historic_scale_comparison_plot <- historic_plot_log + historic_plot_arith + plot_layout(ncol = 1)
    print(combined_historic_scale_comparison_plot)  

# Plot both historic and current log scales  
  tpc.logplot <- ggplot(tpcvis, aes(x=temp, y=rgrlog, color=time.per)) +
      geom_point(alpha=0.5) +
      #geom_line(aes(group = mom, color = time.per), alpha = 0.7) + # lineage lines
      facet_grid(durbin ~ instar) +
      theme_bw() +
      xlab("Temperature (C)") +
      ylab("RGR (mg/mg/h)") +
      ggtitle("2024 vs. 1999 Constant TPC") +
      ylim(-0.02, 0.12)+
      scale_color_manual(values = c("current" = "#EE6A50", "past" = "#7AC5CD"))
    print(tpc.logplot)
    #setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
    pdf("2024ConstantTPCComparison")
    combined_plot
    dev.off()

# Plot both historic and current arithmetic scales
    tpc.arithplot <- ggplot(tpcvis, aes(x=temp, y=rgrarith, color=time.per)) +
      geom_point(alpha=0.5) +
      geom_line(aes(group = mom, color = time.per), alpha = 0.7) + # lineage lines
      facet_grid(durbin ~ instar) +
      theme_bw() +
      xlab("Temperature (C)") +
      ylab("RGR (mg/mg/h)") +
      ggtitle("2024 vs. 1999 Constant TPC") +
      ylim(-0.02, 11.0)+
      scale_color_manual(values = c("current" = "#EE6A50", "past" = "#7AC5CD"))
    print(tpc.arithplot)
    #setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
    pdf("2024ConstantTPCComparison")
    combined_plot
    dev.off()
  
sum(is.na(tpcvis$mom))
sum(is.na(tpcvis$temp))
 sum(is.na(tpcvis$rgrlog))
sum(is.na(tpcvis$rgrarith))
 sum(is.na(tpcvis$durbin))
sum(is.na(tpcvis$instar))  
   