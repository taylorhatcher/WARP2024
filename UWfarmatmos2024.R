#Plotting the UW farm atmospheric data from the 2024 field season
#Taylor M. Hatcher
#
#
# Load required libraries
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)

# Set working directory to github repository for the project
setwd("~/Desktop/Repos/WARP2024")

atmosphericdata <- read.csv("CUHatmosphericdata2024.csv", skip = 2)
