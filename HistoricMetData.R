## Plotting Temperature Distributions from the Field Seln Expeiments from 1999
# Taylor M. Hatcher
# 
#
#
#
# install required packages
install.packages("readxl")
# Load required libraries
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(readxl)
library(viridis)


# set working directory to github repository
setwd("~/Desktop/Repos/WARP2024")

#Load data and exclude the first rows but keep header names
Colnames <- as.character(read_xlsx("UWCUH.MetData.Seln2.Aug1999.jdatecorrected.Taylorsversion.xlsx", n_max = 1, col_names = FALSE))
histmicroclimdata <- read_xlsx(path = "UWCUH.MetData.Seln2.Aug1999.jdatecorrected.Taylorsversion.xlsx", skip = 7, col_names = Colnames)

# Select for columns needed for analysis - excluding TM13 because logger seems to have been malfunctioning during recording time
histmicroclimdata <- histmicroclimdata %>%
  select("YEAR", "SITE","DATE", "JDATE", "LONGTIME", "TIME", "SOLAR", "TAIR","TM1","TM2","TM3","TM4","TM5","TM6","TM7","TM8","TM9","TM10","TM11","TM12","TM14","TM15","TM16","TM17","TM18","TM19","TM20")

# Convert LONGTIME to a proper datetime format 
histmicroclimdata$LONGTIME <- as.POSIXct(histmicroclimdata$LONGTIME, format = "%Y-%m-%d %H:%M:%S")

# Extract only the time in HH:MM format
histmicroclimdata$LONGTIME <- format(histmicroclimdata$LONGTIME, "%H:%M")

# Combine DATE and LONGTIME into a single datetime column
histmicroclimdata <- histmicroclimdata %>%
  mutate(
    DATETIME = paste(DATE, LONGTIME),        # Combine DATE and LONGTIME
    DATETIME = ymd_hm(DATETIME)             # Parse combined column as datetime
  )

# Reshape data into long format for TM1 to TM20
histmicroclimdata_long <- histmicroclimdata %>%
  pivot_longer(
    cols = starts_with("TM"), # Select all columns that start with "TM"
    names_to = "Variable", 
    values_to = "Value"
  )

# Plot all TM columns as separate lines
TemperaturePlot <- ggplot(histmicroclimdata_long, aes(x = DATETIME, y = Value, color = Variable)) +
  geom_point() +
  scale_color_viridis_d()+
  labs(
    title = "Historic 1999 Field Seln Microclimate Temperatures",
    x = "Datetime",
    y = "Temperature (°C)",
    color = "Sensor"
  ) +
  theme_minimal()

#print plot
  print(TemperaturePlot)

# Calculate temp extremes for data frame
  temp_extremes <- histmicroclimdata_long %>%
    group_by(Variable) %>%
    summarise(T_min = min(Value, na.rm = TRUE),
              T_max = max(Value, na.rm = TRUE),
              .groups = 'drop')
  
# Reshape the temp_extremes data into long format for plotting
  temp_extremes_long <- temp_extremes %>%
    pivot_longer(cols = c(T_min, T_max), names_to = "Temperature_Type", values_to = "Temperature")

# Min and Max distribution plot for each temperature logger
histminmaxplot <- ggplot(temp_extremes_long, aes(x = Variable, y = Temperature, fill = Temperature_Type)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_viridis_d() +
    labs(
      title = "1999 Field Seln Minimum and Maximum Temperatures by Logger",
      x = "Logger",
      y = "Temperature (°C)",
      fill = "Temperature Type"
    ) +
    theme_classic(base_size = 16) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(histminmaxplot)
  
  # Combine DATE and LONGTIME into a single datetime column
  histhourly_means <- histmicroclimdata_long %>%
    mutate(HourlyTime = floor_date(DATETIME, "hour")) %>%
    group_by(HourlyTime, Variable) %>%
    summarise(MeanTemperature = mean(Value, na.rm = TRUE),
              .groups = 'drop')
  # Calculate hourly means for each temperature logger
  histhourly_means <- histmicroclimdata_long %>%
    mutate(HourlyTime = floor_date(DATETIME, "hour")) %>% # Round datetime to the nearest hour
    group_by(HourlyTime, Variable) %>%                   # Group by rounded hour and logger
    summarise(MeanTemperature = mean(Value, na.rm = TRUE), .groups = 'drop') # Calculate mean
  
  # Plot hourly means for each logger
  hourly_plot <- ggplot(histhourly_means, aes(x = HourlyTime, y = MeanTemperature, color = Variable)) +
    geom_line() +                                         # Use line plot to show trends
    scale_color_viridis_d() +                            # Use a colorblind-friendly palette
    labs(
      title = "1999 Field Seln Hourly Mean Temperatures by Logger",
      x = "Datetime",
      y = "Mean Temperature (°C)",
      color = "Sensor"
    ) +
    theme_minimal() +                                    # Clean plot style
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
    )
  
  # Print the plot
  print(hourly_plot)
