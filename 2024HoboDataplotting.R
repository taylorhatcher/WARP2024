# Plotting operative model temperatures from 2024 field season
# Taylor M. Hatcher
#

# Load required libraries
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)

setwd("~/Desktop/Repos/WARP2024/Data")

# Load data using fread with fill = TRUE to handle mismatched columns, I am using fread becasue I was having issues with loading my data with read.csv for some reason - researched it and came to the conclusion that fread could handle this large data set faster than csv() or read.csv()
hobo1data2024 <-
  fread(
    'Hobo_1_2024field.csv',
    skip = 4,
    fill = TRUE,
    na.strings = c("Logged", "Series: T-Type")
  )
hobo2data2024 <-
  fread(
    'Hobo_2_2024field.csv',
    skip = 4,
    fill = TRUE,
    na.strings = c("Logged", "Series: T-Type")
  )
hobo3data2024 <-
  fread(
    'Hobo_3_2024field.csv',
    skip = 4,
    fill = TRUE,
    na.strings = c("Logged", "Series: T-Type")
  )

# select only for columns with values-- thank you Julia
hobo1data2024 <- hobo1data2024 %>%
  select("V1", "V2", "V3", "V4", "V5")
hobo2data2024 <- hobo2data2024 %>%
  select("V1", "V2", "V3", "V4", "V5")
hobo3data2024 <- hobo3data2024 %>%
  select("V1", "V2", "V3", "V4", "V5")

# Rename the datetime column in each dataset
setnames(hobo1data2024, old = "V1", new = "datetime")
setnames(hobo2data2024, old = "V1", new = "datetime")
setnames(hobo3data2024, old = "V1", new = "datetime")

# Convert the datetime column to a workable date-time object
hobo1data2024[, datetime := mdy_hms(datetime)]
hobo2data2024[, datetime := mdy_hms(datetime)]
hobo3data2024[, datetime := mdy_hms(datetime)]

# Convert relevant temperature columns to numeric
hobo1data2024[, (2:5) := lapply(.SD, as.numeric), .SDcols = 2:5]
hobo2data2024[, (2:5) := lapply(.SD, as.numeric), .SDcols = 2:5]
hobo3data2024[, (2:5) := lapply(.SD, as.numeric), .SDcols = 2:5]

# Merge datasets by datetime for all hobo loggers - can only merge two at a time so I merged first two and then the last with the merged 
combined_loggers <-
  merge(hobo1data2024, hobo2data2024, by = "datetime", all = TRUE)
combined_loggers <-
  merge(combined_loggers,
        hobo3data2024,
        by = "datetime",
        all = TRUE)


# Drop rows with NA in datetime or temperature columns -- lots of NAs
combined_loggers <- na.omit(combined_loggers, cols = "datetime")

# Reshape the data to long format for plotting
long_data <- melt(
  combined_loggers,
  id.vars = "datetime",
  measure.vars = c(
    "V2.x",
    "V3.x",
    "V4.x",
    "V5.x",
    "V2.y",
    "V3.y",
    "V4.y",
    "V5.y",
    "V2",
    "V3",
    "V4",
    "V5"
  ),
  
# copy and paste that into select
  variable.name = "Logger",
  value.name = "Temperature"
)

# Update Logger column as a factor with meaningful names
long_data <- long_data %>%
  mutate(Logger = factor(
    Logger,
    labels = c(
      "HOBO Logger 1 - Temp 1",
      "HOBO Logger 1 - Temp 2",
      "HOBO Logger 1 - Temp 3",
      "HOBO Logger 1 - Temp 4",
      "HOBO Logger 2 - Temp 1",
      "HOBO Logger 2 - Temp 2",
      "HOBO Logger 2 - Temp 3",
      "HOBO Logger 2 - Temp 4",
      "HOBO Logger 3 - Temp 1",
      "HOBO Logger 3 - Temp 2",
      "HOBO Logger 3 - Temp 3",
      "HOBO Logger 3 - Temp 4 - Shaded Air Temperature"
    )
  ))

# Check for missing data in long_data
table(is.na(long_data$Temperature))

# Define the start and end dates for filtering
start_date <- as.POSIXct("2024-06-22", format = "%Y-%m-%d")
end_date <-
  as.POSIXct("2024-08-15 23:59:59", format = "%Y-%m-%d %H:%M:%S")

# Convert datetime to just date for daily means
filtered_data_day <- long_data%>%
  mutate(DATE = as.Date(datetime))

# Divide logger information into time intervals so it is easier to visualize, split into experiment times when caterpillars were in the field 
filtered_data_day <- long_data %>%
  mutate(DATE = as.Date(datetime)) %>%
  mutate(Interval = case_when(
    DATE >= as.Date("2024-06-21") & DATE <= as.Date("2024-07-03") ~ "2024-06-21 to 2024-07-03",
    DATE >= as.Date("2024-07-03") & DATE <= as.Date("2024-07-28") ~ "2024-07-03 to 2024-07-28",
    DATE >= as.Date("2024-07-28") & DATE <= as.Date("2024-08-18") ~ "2024-07-28 to 2024-08-18",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Interval))

# Calculate daily mean temperatures for all loggers combined
dailylogger_means <- filtered_data_day %>%
  group_by(DATE, Interval) %>%
  summarise(MeanTemperature = mean(Temperature, na.rm = TRUE), .groups = 'drop')

# Plot daily mean temperature distributions for each time interval indicated above
ggplot(dailylogger_means, aes(x = MeanTemperature, color = Interval)) +
  geom_density(size = 1, alpha = 1) +
  scale_fill_viridis_d() +
  labs(
    title = "2024 Daily Mean Temperature Distributions",
    x = "Mean Temperature (°C)",
    y = "Density",
    fill = "Date Interval"
  ) +
  theme_classic(base_size = 18)

# Load in historic met data frame
kinghistmetdata <- read.csv("FormattedHistoricMetDataFieldSln.csv")

# calculate daily means for met data from 1999
dailyhistoriclogger_means <- kinghistmetdata %>%
  group_by(Variable, DATE) %>%
  summarise(MeanTemperature = mean(TAIR, na.rm = TRUE), .groups = 'drop')

# Plot daily mean historic temperature distributions 
ggplot(dailyhistoriclogger_means, aes(x = MeanTemperature, color = Variable, group = Variable)) +
  geom_density(alpha = 0.7) +
  scale_fill_viridis_d() +
  labs(
    title = "1999 Daily Mean Temperature Distributions",
    x = "Mean Temperature (°C)",
    y = "Density",
    color = "Logger"
) +
theme_classic(base_size = 20)


# Manually assign 'Interval' to historic data 
dailyhistoriclogger_means <- dailyhistoriclogger_means %>%
  mutate(Interval = "1999")  

# Assign 'Interval' to the 2024 data 
dailylogger_means <- dailylogger_means %>%
  mutate(Interval = factor(Interval)) 

# Combine both 2024 and 1999 data
combined_means <- bind_rows(
  dailylogger_means %>% mutate(Source = "2024"),
  dailyhistoriclogger_means %>% mutate(Source = "1999")
)

# Plotting
ggplot() +
  # Plot density lines for 2024 data
  geom_density(data = filter(combined_means, Source == "2024"),
               aes(x = MeanTemperature, color = Interval, group = Interval),
               size = 1) +
  
  # Plot density lines for 1999 data
  geom_density(data = filter(combined_means, Source == "1999"),
               aes(x = MeanTemperature, color = Interval, group = Interval),
               size = 1, alpha = 0.7) +
  
  # Color scales for the intervals
  scale_color_viridis_d(option = "D") +
  
  labs(
    title = "1999 vs 2024 Operative Temp Logger Daily Mean Distribution",
    x = "Mean Temperature (°C)",
    y = "Density",
    color = "Date Interval"
  ) +
  theme_classic(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))











    # Filter data to include only relevant dates
#filtered_data <- long_data %>%
#  filter(datetime >= start_date & datetime <= end_date)

# Convert datetime to hourly granularity
#filtered_data <- filtered_data %>%
#  mutate(DateTime = floor_date(datetime, "hour")) %>% # floor_date filters to the hour and ignores the seconds which aren't useful 
#  mutate(Temperature = as.numeric(Temperature))

# Calculate hourly means for each logger # code gets angry here for some reason
#hourly_means <- filtered_data %>%
 # group_by(DateTime, Logger) %>%
  #summarise(MeanTemperature = mean(Temperature, na.rm = TRUE),
#            .groups = 'drop')

#long_data_filtered <- long_data %>%
 # filter(!is.na(Temperature))

# Calculate Min and Max temperatures for each logger
# temp_extremes <- long_data_filtered %>%
#   group_by(Logger) %>%
#   summarise(
#     T_min = ifelse(all(is.na(Temperature)), NA, min(Temperature, na.rm = TRUE)),
#     T_max = ifelse(all(is.na(Temperature)), NA, max(Temperature, na.rm = TRUE)),
#     .groups = 'drop'
#   )

# # Reshape the temp_extremes data into long format for plotting
# temp_extremes_long <- temp_extremes %>%
#   pivot_longer(
#     cols = c(T_min, T_max),
#     names_to = "Temperature_Type",
#     values_to = "Temperature"
#   )
# 
# # Check how many missing values per logger - there are a lot of nas because loggers were not in sync
# missing_data_per_logger <- long_data %>%
#   group_by(Logger) %>%
#   summarise(MissingCount = sum(is.na(Temperature)))
# print(missing_data_per_logger)
# 
# # plotting the Temperature Distributions- Make sure that all loggers are represented
# tempdisgraph <-
#   ggplot(long_data_filtered, aes(x = Temperature, color = Logger)) +
#   geom_density(size = 1) +
#   scale_color_viridis_d() +
#   labs(
#     title = "2024 Temperature Distribution by Logger",
#     x = "Temperature (°C)",
#     y = "Density",
#     color = "Logger"
#   )
# theme_classic(base_size = 18) +
#   theme(legend.position = c(0.8, 0.8))
# print(tempdisgraph)
# 
# # Min and Max Temperature Plot (Bar Plot)
# minmaxplot <-
#   ggplot(temp_extremes_long,
#          aes(x = Logger, y = Temperature, fill = Temperature_Type)) +
#   geom_bar(stat = "identity", position = position_dodge()) +
#   scale_fill_viridis_d() +
#   labs(title = " 2024 Minimum and Maximum Temperatures by Logger",
#        x = "Logger",
#        y = "Temperature (°C)",
#        fill = "Temperature Type") +
#   theme_classic(base_size = 16) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# print(minmaxplot)
# 
# # Density Plot for Full Dataset (Temperature Distributions)
# tempdensplot <-
#   ggplot(filtered_data, aes(x = Temperature, color = Logger)) +
#   geom_density(size = 1) +
#   scale_color_viridis_d() +
#   labs(
#     title = "Temperature Distributions by Logger",
#     x = "Temperature (°C)",
#     y = "Density",
#     color = "Logger"
#   ) +
#   theme_classic(base_size = 18) +
#   theme(legend.position = c(0.8, 0.8))
# print(tempdensplot)
# 
# #Plot the hourly mean temperature data
# hourlymeanplot <-
#   ggplot(hourly_means,
#          aes(x = DateTime, y = MeanTemperature, color = Logger)) +
#   geom_line(size = 1) +
#   labs(
#     title = "Hourly Mean Temperature from Multiple Loggers (June 22 - August 15, 2024)",
#     x = "Date and Time",
#     y = "Mean Temperature (°C)",
#     color = "Logger"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# print(hourlymeanplot)
