# Plotting operative model temperatures from 2024 field season
# Taylor M. Hatcher
#
# Plotting operative model temperatures from 2024 field season
# Taylor M. Hatcher

library(ggplot2)
library(dplyr)
library(lubridate)
library(cowplot)
library(tidyr)

# Load in logger data for each logger and process datetime
# Use 'character' for temp columns initially to handle possible non-numeric entries
hobo1data2024 <- read.csv('Hobo_1_2024field.csv', sep = ',', header = FALSE, skip = 3,
                          colClasses = c("character", "character", "character", "character", "character"),
                          na.strings = c("Logged", "Series: T-Type")) %>%
  mutate(datetime = mdy_hms(V1)) %>%
  mutate(across(starts_with("V"), as.numeric))  # Convert temp columns to numeric

hobo2data2024 <- read.csv('Hobo_2_2024field.csv', sep = ",", header = FALSE, skip = 3,
                          colClasses= c("character", "character", "character", "character", "character"),
                          na.strings = c("Logged", "Series: T-Type")) %>%
  mutate(datetime = mdy_hms(V1)) %>%
  mutate(across(starts_with("V"), as.numeric))  # Convert temp columns to numeric

hobo3data2024 <- read.csv('Hobo_3_2024field.csv', sep = ",", header = FALSE, skip = 3,
                          colClasses= c("character", "character", "character", "character", "character"),
                          na.strings = c("Logged", "Series: T-Type")) %>%
  mutate(datetime = mdy_hms(V1)) %>%
  mutate(across(starts_with("V"), as.numeric))  # Convert temp columns to numeric

# Rename relevant columns that we want to graph
colnames(hobo1data2024) <- c("datetime", "temp1", "temp2", "temp3", "temp4")
colnames(hobo2data2024) <- c("datetime", "temp5", "temp6", "temp7", "temp8")
colnames(hobo3data2024) <- c("datetime", "temp9", "temp10", "temp11", "temp12")

# Merge all datasets by datetime
combined_loggers <- full_join(hobo1data2024, hobo2data2024, by = "datetime") %>%
  full_join(., hobo3data2024, by = "datetime")

# Drop rows with NA in datetime or temperature columns
cleaned_loggers <- combined_loggers %>% drop_na(datetime)

# Reshape the data to long format for plotting
long_data <- cleaned_loggers %>%
  pivot_longer(cols = starts_with("temp"), names_to = "Logger", values_to = "Temperature")

# Plot the temperature data over time
ggplot(long_data, aes(x = datetime, y = Temperature, color = Logger)) +
  geom_line() +
  labs(title = "Temperature Data from Multiple Loggers",
       x = "Date and Time", y = "Temperature (°C)", color = "Logger") +
  theme_minimal()

# Create a heatmap for a different visualization
ggplot(long_data, aes(x = datetime, y = Logger, fill = Temperature)) +
  geom_tile() +
  labs(title = "Heatmap of Logger Measurements Over Time",
       x = "Date and Time", y = "Logger") +
  theme_minimal() +
  scale_fill_viridis_c()

