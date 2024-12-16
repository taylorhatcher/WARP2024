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

#Load data
histmicroclimdata <- read_xlsx(path = "UWCUH.MetData.Seln2.Aug1999.jdatecorrected.Taylorsversion.xlsx")

# Convert LONGTIME to a proper datetime format if needed
histmicroclimdata$LONGTIME <- as.POSIXct(histmicroclimdata$LONGTIME, format = "%Y-%m-%d %H:%M:%S")

# Extract only the time in HH:MM format
histmicroclimdata$LONGTIME <- format(histmicroclimdata$LONGTIME, "%H:%M")

# Check the results
head(histmicroclimdata$LONGTIME)
