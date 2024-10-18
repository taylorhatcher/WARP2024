# Plotting operative model temperatures from 2024 field season
# Taylor M. Hatcher
#
library(ggplot2)
library(dplyr)
library(lubridate)
library(cowplot)
library(tidyr)

# Read data from Hobo1 CSV file
hobo1data2024 <- read.csv('Hobo_1_2024field.csv',
                          sep = ',',
                          header = FALSE,
                          skip = 3,
                          colClasses= c("character", "character", "numeric", "numeric", "character"),
                          na.strings = c("Logged", "Series:T-Type")
                          ) 
# Read data from Hobo2 CSV file 
hobo2data2024 <- read.csv('Hobo_2_2024field.csv',
                          sep = ',',
                          header = FALSE,
                          skip = 3,
                          colClasses= c("character", "character"))
# Read data from Hobo3 CSV file
hobo3data2024 <- read.csv('Hobo_3_2024field.csv',
                          sep = ',',
                          header = FALSE,
                          skip = 3,
                          colClasses= c("character", "character"))
