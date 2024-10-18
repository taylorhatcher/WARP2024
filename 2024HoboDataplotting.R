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
                          na.strings = c("Logged", "Series:T-Type")) %>%
  mutate(dt = mdy_hms(V1)) 

# Read data from Hobo2 CSV file 
hobo2data2024 <- read.csv('Hobo_2_2024field.csv',
                          sep = ',',
                          header = FALSE,
                          skip = 3,
                          colClasses= c("character", "character")) %>%
  mutate(dt = mdy_hms(V1))

# Read data from Hobo3 CSV file
hobo3data2024 <- read.csv('Hobo_3_2024field.csv',
                          sep = ',',
                          header = FALSE,
                          skip = 3,
                          colClasses= c("character", "character")) %>%
  mutate(dt = mdy_hms(V1))
# Combine first 2 data frames from hobo loggers
combined_hobo1_hobo2 <- merge(hobo1data2024, hobo2data2024, by = 'dt' , all = TRUE)

# Combine 1st merge with last data set
combined_loggers <- merge(combined_hobo1_hobo2, hobo3data2024, by = 'dt' , all = TRUE)

# Omit rows with any NA values
cleaned_loggers <- drop_na(combined_loggers, dt)

# Remove unnecessary columns from dataset
cleaned_loggers <- combined_loggers[ , !(names(combined_loggers) %in% c("V6.x", "V7.x", "V8.x", "V9.x", "V10.x", "V11.x", "V12.x", "V6.y", "V7.y", "V8.y", "V9.y", "V10.y", "V11.y", "V12.y", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13"))]

