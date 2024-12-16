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
histmicroclimdata <- read_xlsx(path = "UWCUH.MetData.Seln2.Aug1999.xlsx")
