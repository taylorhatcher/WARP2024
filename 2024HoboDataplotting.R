# Plotting operative model temperatures from 2024 field season
# Taylor M. Hatcher
#
# This is a test

# Load required libraries
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)

# Load data using fread with fill = TRUE to handle mismatched columns
hobo1data2024 <- fread('Hobo_1_2024field.csv', skip = 4, fill = TRUE, na.strings = c("Logged", "Series: T-Type"))
hobo2data2024 <- fread('Hobo_2_2024field.csv', skip = 4, fill = TRUE, na.strings = c("Logged", "Series: T-Type"))
hobo3data2024 <- fread('Hobo_3_2024field.csv', skip = 4, fill = TRUE, na.strings = c("Logged", "Series: T-Type"))

hobo1data2024 <- hobo1data2024 %>%
  select(datetime, V2, V3, V4, V5)
# Exclude nas----n Julia's suggestion!!
#dataframe <- dataframe%>% #
 # select() 
# Rename the datetime column in each dataset
setnames(hobo1data2024, old = "V1", new = "datetime")
setnames(hobo2data2024, old = "V1", new = "datetime")
setnames(hobo3data2024, old = "V1", new = "datetime")

# Convert the datetime column to a proper date-time object
hobo1data2024[, datetime := mdy_hms(datetime)]
hobo2data2024[, datetime := mdy_hms(datetime)]
hobo3data2024[, datetime := mdy_hms(datetime)]

# Convert relevant temperature columns to numeric
hobo1data2024[, (2:5) := lapply(.SD, as.numeric), .SDcols = 2:5]
hobo2data2024[, (2:5) := lapply(.SD, as.numeric), .SDcols = 2:5]
hobo3data2024[, (2:5) := lapply(.SD, as.numeric), .SDcols = 2:5]

# Merge datasets by datetime
combined_loggers <- merge(hobo1data2024, hobo2data2024, by = "datetime", all = TRUE)
combined_loggers <- merge(combined_loggers, hobo3data2024, by = "datetime", all = TRUE)


# Drop rows with NA in datetime or temperature columns
combined_loggers <- na.omit(combined_loggers, cols = "datetime")

# Reshape the data to long format for plotting
long_data <- melt(combined_loggers,
                  id.vars = "datetime",
                  measure.vars = c("V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12"),# copy and paste that into select
                  variable.name = "Logger",
                  value.name = "Temperature")

# Update Logger column as a factor with meaningful names
long_data <- long_data %>%
  mutate(Logger = factor(Logger, labels = c("Logger 1 - Temp 1", "Logger 1 - Temp 2", "Logger 1 - Temp 3",
                                            "Logger 1 - Temp 4", "Logger 2 - Temp 1", "Logger 2 - Temp 2",
                                            "Logger 2 - Temp 3", "Logger 2 - Temp 4",
                                            "Logger 3 - Temp 1", "Logger 3 - Temp 2", "Logger 3 - Temp 3")))
# Check for any missing data in long_data
table(is.na(long_data$Temperature))

# Define the start and end dates for filtering
start_date <- as.POSIXct("2024-06-22", format="%Y-%m-%d")
end_date <- as.POSIXct("2024-08-15 23:59:59", format="%Y-%m-%d %H:%M:%S")

# Filter data to include only relevant dates
filtered_data <- long_data %>%
  filter(datetime >= start_date & datetime <= end_date)

# Convert datetime to hourly granularity
filtered_data <- filtered_data %>%
  mutate(DateTime = floor_date(datetime, "hour")) %>%
  mutate(Temperature = as.numeric(Temperature))

# Calculate hourly means for each logger
hourly_means <- filtered_data %>%
  group_by(DateTime, Logger) %>%
  summarise(MeanTemperature = mean(Temperature, na.rm = TRUE), .groups = 'drop')

long_data_filtered <- long_data %>%
  filter(!is.na(Temperature))

# Calculate Min and Max temperatures for each logger
temp_extremes <- long_data_filtered %>%
  group_by(Logger) %>%
  summarise(T_min = ifelse(all(is.na(Temperature)), NA, min(Temperature, na.rm = TRUE)),
            T_max = ifelse(all(is.na(Temperature)), NA, max(Temperature, na.rm = TRUE)),
            .groups = 'drop')

# Reshape the temp_extremes data into long format for plotting
temp_extremes_long <- temp_extremes %>%
  pivot_longer(cols = c(T_min, T_max), names_to = "Temperature_Type", values_to = "Temperature")

# Check how many missing values per logger
missing_data_per_logger <- long_data %>%
  group_by(Logger)%>%
  summarise(MissingCount = sum(is.na(Temperature)))
print(missing_data_per_logger)

# plotting the Temperature Distributions- Make sure that all loggers are represented
ggplot(long_data_filtered, aes(x = Temperature, color = Logger)) +
  geom_density(size = 1) +
  scale_color_viridis_d() +
  labs(
    title = "Temperature Distribution by Logger",
    x = "Temperature (°C)",
    y = "Density",
    color = "Logger"
  )
theme_classic(base_size = 18) +
  theme(legend.position = c(0.8, 0.8))

# Min and Max Temperature Plot (Bar Plot)
ggplot(temp_extremes_long, aes(x = Logger, y = Temperature, fill = Temperature_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis_d() +
  labs(
    title = "Minimum and Maximum Temperatures by Logger",
    x = "Logger",
    y = "Temperature (°C)",
    fill = "Temperature Type"
  ) +
  theme_classic(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Density Plot for Full Dataset (Temperature Distributions)
ggplot(filtered_data, aes(x = Temperature, color = Logger)) +
  geom_density(size = 1) +
  scale_color_viridis_d() +
  labs(
    title = "Temperature Distributions by Logger",
    x = "Temperature (°C)",
    y = "Density",
    color = "Logger"
  ) +
  theme_classic(base_size = 18) +
  theme(legend.position = c(0.8, 0.8))

# Plot the hourly mean temperature data
ggplot(hourly_means, aes(x = DateTime, y = MeanTemperature, color = Logger)) +
  geom_line(size = 1) +
  labs(title = "Hourly Mean Temperature from Multiple Loggers (June 22 - August 15, 2024)",
       x = "Date and Time", y = "Mean Temperature (°C)", color = "Logger") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

