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

# Combine Date and time columns and format
atmosphericdata$datetime <- as.POSIXct(paste(atmosphericdata$Date, atmosphericdata$Time), format="%Y-%m-%d %H:%M")


# Plot solar radiation
SolarPlot <- ggplot(atmosphericdata, aes(x = datetime, y = Solar)) +
  geom_line(data=atmosphericdata, stat="identity", position="identity") +
  labs(title = "2024 Atmospheric Science UW farm WStation Solar Radiation", y="Solar Radiation (W/m2)", x = "Time")
print(SolarPlot)

# Plot wind speed

WindSpeedPlot <- ggplot(atmosphericdata, aes(x = datetime, y = Speed)) +
  geom_line(data=atmosphericdata, stat = "identity", position="identity") +
  labs(title = "2024 Atmospheric Science UW farm WStation Wind speed")
print(WindSpeed)

atmosphericdata <- atmosphericdata %>%
  mutate(Temperature = ifelse(is.na(as.numeric(as.character(Temperature))), NA, as.numeric(as.character(Temperature))))
atmosphericdata <- atmosphericdata %>%
  filter(!is.na(Temperature))

# Convert Temperature to Celsius from Fahrenheit
atmosphericdata$Temperature <- (atmosphericdata$Temperature - 32) * 5 / 9
# Calculate daily mean temperature
hourly_means <- atmosphericdata %>%
  mutate(date = as.Date(datetime),
         hour = format(datetime, "%H")) %>%
  group_by(date, hour)%>%
  summarise(MeanTemperature = mean(Temperature, na.rm = FALSE)) %>%
  ungroup()
print(hourly_means)

# Plot daily hourly mean temperatures

hourlymeanplot <- ggplot(hourly_means, aes(x = date, y = MeanTemperature)) +
  geom_line(size = 1) +
  labs(title = "UW Farm AtmosWData Hourly Mean Temperature", 
       x = "Date and Time", y = "Mean Temperature (°C)") +
  theme_minimal()
print(hourlymeanplot)

# Plot temperature data

TemperaturePlot <- ggplot(atmosphericdata, aes(x = datetime, y = Temperature)) +
  geom_point(data=atmosphericdata, stat="identity", position="identity", aes(color = as.factor(Temperature))) +
  labs(title = "2024 Atmospheric Science UW farm Wstation Temperature")
print(TemperaturePlot)


