# Taylor M. Hatcher
# 24 Hr Constant TPC Analysis for P.rapae

library(ggplot2)
library(data.table)
library(dplyr)
library(patchwork)
library(reshape2)
library(viridis)
library(tidyverse)

# set working directory to github repository
setwd("~/Desktop/Repos/WARP2024")

# read in past data 
tpc1 = read.csv("PrapaeW.1999.ConstantTempTPCs.4thinstar.jul2021.xlsx - data.csv")
tpc1$instar=4

tpc2 = read.csv("PrapaeW.1999.ConstantTempTPCs.5thinstar.jul2021.xlsx - data.csv")
names(tpc2)=names(tpc1)
tpc2$instar=5

#tpc3 = read.csv("Prapae.WAonly.csv")
#tpc3$instar=4
# this section of code is me trying to add in the data set for historic TRN for leaf but they aren't named similarly so I can't rbind them

# combine past data sets using rbind
tpc.p= rbind(tpc1, tpc2)

# calculate relative growth rate for past data set
tpc.p$rgr= (log(tpc.p$fw) - log(tpc.p$Mo))/tpc.p$time ### ?!?!?! is this the correct way to calculate rgr?
tpc.p$time.per= "past"

# ensure column names match current data column names
tpc.ps= tpc.p[,c("UniID","mom","ID","temp","instar","time","duration","mgain","rgr","time.per")]

# #plot densities for what time they did weighings in the past
# ggplot(tpc.ps[tpc.ps$time>5 & tpc.ps$time<6.5,], aes(x=time, color=factor(instar), group=factor(instar)))+
# geom_density()
# table(tpc.ps$time, tpc.ps$instar)

# load in recent 2024 TPC data
tpc.c = read.csv("2024PrapaeConstantTPCCombineddata.csv")

# Convert fw to numeric and drop nas
tpc.c$fw <- as.numeric(tpc.c$fw)

# calculate relative growth rate for current data set
tpc.c$rgr= (log(tpc.c$fw) - log(tpc.c$M0))/tpc.c$duration #!!!!!!!!check duration if it is correct
tpc.c$time.per= "current"

# ensure naming matches old data sets
tpc.c$mom= tpc.c$Female
tpc.c$ID= tpc.c$Individual
tpc.c$UniID= paste(tpc.c$temp, tpc.c$mom, tpc.c$ID, sep=".")

#Calculate mass gained in each time treatment
tpc.c$mgain= tpc.c$fw - tpc.c$M0

#combine historic and current
tpc.cs= tpc.c[,c("UniID","mom","ID","temp","instar","time","duration","mgain","rgr","time.per")]

# combine data sets 
tpc= rbind(tpc.cs, tpc.ps)


###Plot

##Set up durations of 6hr and 24hr

tpc$dur=NA
tpc$time= as.numeric(tpc$time)
tpc[tpc$time>5 & tpc$time<6.5 & tpc$time.per=="past","dur"]=6
tpc[tpc$time>20 & tpc$time<26 & tpc$time.per=="past","dur"]=24
tpc[tpc$time.per=="current","dur"]= tpc[tpc$time.per=="current","duration"]

###separate historic and present data

# Filter data for historic data set 
tpc_past <- tpc %>% filter(time.per == "past")

# Create the plot for past data
past_plot <- ggplot(tpc_past[tpc_past$dur %in% c(6, 24),], aes(x = temp, y = rgr, color = mom)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(group = mom), alpha = 0.7) +
  facet_grid(dur ~ instar) +
  theme_bw() +
  xlab("Temperature (C)") +
  ylab("RGR (mg/mg/h") +
  ggtitle("Historic Data (1999)") +
  scale_color_viridis(discrete = TRUE, option = "C")
print(past_plot)

# Filter data for current data set
tpc_current <- tpc %>% filter(time.per == "current")

# Create the plot for current data
current_plot <- ggplot(tpc_current[tpc_current$duration %in% c(6,24),], aes( x = temp, y = rgr, color = mom)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(group = mom), alpha = 0.7) +
  facet_grid(dur ~ instar) +
  theme_bw() +
  xlab("Temperature (C)") +
  ylab("RGR (mg/mg/h") +
  ggtitle("Present Data (2024)") +
  ylim(-0.02, 0.14) +
  scale_color_viridis(discrete = TRUE)
print(current_plot)

#combine both individual plots for past and current data
combined_plot <- past_plot + current_plot + plot_layout(ncol = 1)
print(combined_plot)   
tpc.plot <- ggplot(tpc[tpc$dur %in% c(6,24),], aes(x=temp, y=rgr, color=time.per)) +
  geom_point(alpha=0.5) +
  geom_line(aes(group = mom, color = time.per), alpha = 0.7) + # lineage lines
  facet_grid(dur ~ instar) +
  theme_bw() +
  xlab("Temperature (C)") +
  ylab("RGR (mg/mg/h)") +
  ggtitle("2024 vs. 1999 Constant TPC") +
  ylim(-0.02, 0.14)+
  scale_color_manual(values = c("current" = "#EE6A50", "past" = "#7AC5CD"))
print(tpc.plot)

# Aggregate mean values with corrected standard error calculation
tpc.agg <- tpc %>%
  group_by(temp, time.per, dur, instar) %>%
  dplyr::summarise(
    mean = mean(rgr, na.rm = TRUE),
    se = sd(rgr, na.rm = TRUE) / sqrt(n())
  )
#plotting family means with error bars
tpc.plot <- ggplot(tpc.agg[tpc.agg$dur %in% c(6, 24), ],aes(x = temp, y = mean, color = time.per, group = time.per)) +
  geom_point(alpha = 0.7, size = 2) + # Points for mean values
  geom_errorbar(aes(ymin = mean - se, ymax = mean +se), width = 0.3, alpha = 0.6) + # Error bars
  facet_grid(dur ~ instar) + #Facets for duration and instar
  theme_bw() +
  xlab("Temperature(°C)") +
  ylab("RGR (mg/mg/hr)") +
  ggtitle("Past vs. Present Constant TPC")
ylim(-0.10, 0.14)

tpc$time.per <- factor(tpc$time.per, levels = c("past", "current"))
print(tpc.plot)


#plot family mean values 
tpc.agg.f <- tpc %>%
  group_by(mom) %>%
  dplyr::summarise(rgr, na.rm=TRUE) 


