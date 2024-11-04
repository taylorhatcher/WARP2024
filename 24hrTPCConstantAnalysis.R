## Taylor M. Hatcher
## Analysis of 6 hr and 24hr constant TPCs for P. rapae. Each caterpillar only experienced one temperature for 24 hours. 
library(ggplot2)
library(data.table)
library(dplyr)
library(patchwork)
library(reshape2)
library(viridis)
library(tidyverse)

# read in TPC data 
tpccurrent = read.csv("2024PrapaeConstantTPCCombineddata.csv")
# Convert fw to numeric and drop nas
tpccurrent$fw <- as.numeric(tpccurrent$fw)

tpccurrentrgr= (log(tpccurrent$fw) - log(tpccurrent$M0))/tpccurrent$duration

tpccurrent$mom= tpccurrent$Female
tpccurrent$ID= tpccurrent$Individual
tpccurrent$UniID= paste(tpccurrent$temp, tpccurrent$mom, tpccurrent$ID, sep=".")
#Calculate mass gained in each time treatment
tpccurrent$mgain= tpccurrent$fw - tpccurrent$M0

#Plot
plot(tpccurrentrgr)




tpccurrent <- tpccurrent %>% drop_na(fw)
tpccurrent$rgr= (log(tpccurrent$fw) - log(tpccurrent$M0))/tpccurrent$duration

