# Plotting operative model temperatures from 2024 field season
# Taylor M. Hatcher
#
#this is a test

library(data.table)
library(ggplot2)
library(lubridate)

# Load data using fread with fill = TRUE to handle mismatched columns
hobo1data2024 <- fread('Hobo_1_2024field.csv', skip = 3, fill = TRUE, na.strings = c("Logged", "Series: T-Type"))
hobo2data2024 <- fread('Hobo_2_2024field.csv', skip = 3, fill = TRUE, na.strings = c("Logged", "Series: T-Type"))
hobo3data2024 <- fread('Hobo_3_2024field.csv', skip = 3, fill = TRUE, na.strings = c("Logged", "Series: T-Type"))

# Check the column names to make sure there aren't any unexpected duplicates
print(names(hobo1data2024))
print(names(hobo2data2024))
print(names(hobo3data2024))

# Rename the datetime column in each dataset
setnames(hobo1data2024, old = "V1", new = "datetime")
setnames(hobo2data2024, old = "V1", new = "datetime")
setnames(hobo3data2024, old = "V1", new = "datetime")

# Convert the datetime column to a proper date-time object
hobo1data2024[, datetime := mdy_hms(datetime)]
hobo2data2024[, datetime := mdy_hms(datetime)]
hobo3data2024[, datetime := mdy_hms(datetime)]

# Convert relevant temperature columns to numeric (adjust the column range as per your data structure)
hobo1data2024[, (2:5) := lapply(.SD, as.numeric), .SDcols = 2:5]
hobo2data2024[, (2:5) := lapply(.SD, as.numeric), .SDcols = 2:5]
hobo3data2024[, (2:5) := lapply(.SD, as.numeric), .SDcols = 2:5]

# Merge datasets by datetime
combined_loggers <- merge(hobo1data2024, hobo2data2024, by = "datetime", all = TRUE)
combined_loggers <- merge(combined_loggers, hobo3data2024, by = "datetime", all = TRUE)

# Drop rows with NA in datetime or temperature columns
combined_loggers <- na.omit(combined_loggers, cols = "datetime")

# Reshape the data to long format for plotting, specifying temperature columns explicitly
long_data <- melt(combined_loggers, 
                  id.vars = "datetime", 
                  measure.vars = c("V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12"), 
                  variable.name = "Logger", 
                  value.name = "Temperature")

# Since the logger names are V2, V3, etc., map these to meaningful logger names
long_data[, Logger := factor(Logger, labels = c("Logger 1 - Temp 1", "Logger 1 - Temp 2", "Logger 1 - Temp 3", 
                                                "Logger 1 - Temp 4", "Logger 2 - Temp 1", "Logger 2 - Temp 2", 
                                                "Logger 2 - Temp 3", "Logger 2 - Temp 4", 
                                                "Logger 3 - Temp 1", "Logger 3 - Temp 2", "Logger 3 - Temp 3"))]

# Define the start and end dates for filtering - filtering to only encompass when caterpillars were out in the field during experiments
start_date <- as.POSIXct("2024-06-22", format="%Y-%m-%d")
end_date <- as.POSIXct("2024-08-15 23:59:59", format="%Y-%m-%d %H:%M:%S")

# Check the structure of filtered_data to understand what's inside
str(filtered_data)

# Convert datetime to date and hour for grouping, ensuring it's in POSIXct format
filtered_data <- filtered_data %>%
  mutate(DateTime = floor_date(datetime, "hour"))

# Ensure Temperature is numeric
filtered_data <- filtered_data %>%
  mutate(Temperature = as.numeric(Temperature))

# Check for NAs in Temperature
sum(is.na(filtered_data$Temperature))  # Count NAs in Temperature column

# Calculate hourly means for each logger ####### look at this section of code --- what is happening?
hourly_means <- filtered_data %>%
  group_by(DateTime, Logger) %>%
  summarise(MeanTemperature = mean(Temperature, na.rm = TRUE), .groups = 'drop')

# Check if the hourly_means dataframe has valid data
print(hourly_means)

# Plot the hourly mean temperature data
ggplot(hourly_means, aes(x = DateTime, y = MeanTemperature, color = Logger)) +
  geom_line(size = 1) +
  labs(title = "Hourly Mean Temperature from Multiple Loggers (June 22 - August 15, 2024)",
       x = "Date and Time", 
       y = "Mean Temperature (°C)", 
       color = "Logger") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a heatmap for hourly means
ggplot(hourly_means, aes(x = DateTime, y = Logger, fill = MeanTemperature)) +
  geom_tile() +
  labs(title = "Hourly Mean Heatmap of Logger Measurements (June 22 - August 15, 2024)",
       x = "Date and Time", 
       y = "Logger") +
  theme_minimal() +
  scale_fill_viridis_c()

