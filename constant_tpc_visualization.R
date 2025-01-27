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

current_plot <- ggplot(tpcvis, aes( x = temp, y = rgrlog, color = mom)) +
  filter(time.per == "current")+
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
  
 sum(is.na(tpcvis$mom))
  sum(is.na(tpcvis$temp))
sum(is.na(tpcvis$rgrlog))
sum(is.na(tpcvis$rgrarith))
  sum(is.na(tpcvis$durbin))
   sum(is.na(tpcvis$instar))  
   