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
  
# Filter out past/historic data set
  tpcvis_past <- tpcvis %>%
    filter(time.per == "past")
  
# Plot rgrlog growth rate for 1999 constant tpc data set
    historic_plot_log <- ggplot(tpcvis_past, aes( x = temp, y = rgrlog, color = mom)) +
    geom_point() +
    geom_line(aes(group = mom), alpha = 0.7) +
    facet_grid(durbin ~ instar) +
    theme_bw() +
    xlab("Temperature (C)") +
    ylab("RGR (mg/mg/h") +
    ggtitle("Historic Data (1999)") +
    ylim(0, 0.20) +
    scale_color_viridis(discrete = TRUE)
  print(current_plot)
  
  
   
sum(is.na(tpcvis$mom))
sum(is.na(tpcvis$temp))
 sum(is.na(tpcvis$rgrlog))
sum(is.na(tpcvis$rgrarith))
 sum(is.na(tpcvis$durbin))
sum(is.na(tpcvis$instar))  
   