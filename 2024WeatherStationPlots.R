# # # Plotting 2024 Weather Station Data
# Reading in data
WeatherData <- read.csv("(Edited) Weather Station Field Data.csv")

# Renaming columns
WeatherData <- WeatherData %>% rename(DateTime = 'Date.Time')
WeatherData <- WeatherData %>% rename(Solar = 'Ch2')
WeatherData <- WeatherData %>% rename(GroundTemp = 'Ch4')
WeatherData <- WeatherData %>% rename(AirTemp = 'Ch5')
WeatherData <- WeatherData %>% rename(WindSpeed = 'ChX')
WeatherData <- WeatherData %>% rename(AmbientTemp = 'Amb')

# Deleting information rows and preparing to plot
WData <- WeatherData

WData$DateTime <- as_datetime(WData$DateTime, format = "%m/%d/%Y %H:%M")

# Solar plot
SolarPlot <- ggplot(WData, aes(x = DateTime, y = Solar)) +
  geom_line(data=WData, stat="identity", position="identity") +
  labs(title="2024 Field Weather Station Solar Radiation", y="Solar Radiation (W/m2)", x="Time")

print(SolarPlot)

# Wind plot
WindPlot <- ggplot(WData, aes(x = DateTime, y = WindSpeed*0.44704)) +
  geom_line(data=WData, stat="identity", position="identity") +
  labs(title="2024 Field Weather Station Wind Speed", y="Wind Speed (m/s)", x="Time")

print(WindPlot)

# # Temp Plot
WTData= WData[,c("DateTime","GroundTemp","AirTemp","AmbientTemp")]

#to long format
WTData <- melt(setDT(WTData), id.vars = c("DateTime"), variable.name = "temp")

# Plot
TempPlot <- ggplot(WTData, aes(x = DateTime, y = value, group = as.factor(WTData$temp))) +
  geom_line(data=WTData, stat="identity", position="identity", aes(color = as.factor(WTData$temp))) +
  labs(title="2024 Field Weather Station Temperatures", y="Temperature (C)", x="Time", color="Sensor") +
  ylim(0, 50) +
  scale_color_brewer(palette = "Dark2")

print(TempPlot)


# # Temp Extremes Plot
# Calculate Min and Max temperatures for each logger
temp_extremes <- WTData %>%
  group_by(temp) %>%
  summarise(T_min = ifelse(all(is.na(value)), NA, min(value, na.rm = TRUE)),
            T_max = ifelse(all(is.na(value)), NA, max(value, na.rm = TRUE)),
            .groups = 'drop')

# Reshape the temp_extremes data into long format for plotting
temp_extremes_long <- temp_extremes %>%
  pivot_longer(cols = c(T_min, T_max), names_to = "Temperature_Type", values_to = "Temperature")

minmaxplot <- ggplot(temp_extremes_long, aes(x = temp, y = Temperature, fill = Temperature_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis_d() +
  labs(
    title = "Minimum and Maximum Temperatures by Sensor",
    x = "Sensor",
    y = "Temperature (°C)",
    fill = "Temperature Type"
  ) +
  theme_classic(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(minmaxplot)
