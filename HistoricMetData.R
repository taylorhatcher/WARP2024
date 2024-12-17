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


# set working directory to github repository
setwd("~/Desktop/Repos/WARP2024")

#Load data and exclude the first rows but keep header names
Colnames <- as.character(read_xlsx("UWCUH.MetData.Seln2.Aug1999.jdatecorrected.Taylorsversion.xlsx", n_max = 1, col_names = FALSE))
histmicroclimdata <- read_xlsx(path = "UWCUH.MetData.Seln2.Aug1999.jdatecorrected.Taylorsversion.xlsx", skip = 7, col_names = Colnames)

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

TemperaturePlot <- ggplot(histmicroclimdata, aes(x = DATETIME, y = TM1)) +
  geom_point(data=histmicroclimdata, stat="identity", position="identity", aes(color = as.factor(TM1))) +
  labs(title = "2024 Atmospheric Science UW farm Wstation Temperature")
print(TemperaturePlot)
