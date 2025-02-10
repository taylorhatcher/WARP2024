# Taylor M. Hatcher
# 24 Hr Constant TPC Analysis for P.rapae
# This analysis compares historical constant feeding relative growth rates at 6hr and 24hr at a constant temperature

# Load required libraries 
library(ggplot2)
library(data.table)
library(dplyr)
library(patchwork)
library(reshape2)
library(viridis)
library(tidyverse)
library(lubridate)

# set working directory to github repository
setwd("~/Desktop/Repos/WARP2024/Data")

#### read in past raw data 
tpc1 = read.csv("PrapaeW.1999.ConstantTempTPCs.4thinstar.jul2021.xlsx - data.csv")
tpc1$instar=4 # identify instar

# filter out and include only caterpillars who were active
  tpc1 <- tpc1 %>% filter(active == "yes")

tpc2 <- read.csv("PrapaeW.1999.ConstantTempTPCs.5thinstar.jul2021.xlsx - data.csv")
names(tpc2) <- names(tpc1)
tpc2$instar <- 5 # Identify instar

# filter out and include only caterpillars who were active
tpc2 <- tpc2 %>% filter(active == "yes")

# Combine past datasets using rbind
tpc.p <- rbind(tpc1, tpc2)

# calculate relative growth rate using logarithmic scale for 1999 past data set
tpc.p$rgrlog= (log10(tpc.p$fw) - log10(tpc.p$Mo))/tpc.p$time 
tpc.p$time.per= "past" # labels this data as past data set 

# calculate relative growth rate using arithmetic scale 
tpc.p$rgrarith = (tpc.p$fw - tpc.p$Mo) / tpc.p$time 
tpc.p$time.per = "past"

# ensure column names match current data column names
tpc.ps= tpc.p[,c("UniID","mom","ID","temp", "active","instar","time","duration","mgain","rgrlog","rgrarith","time.per")]


#### load in recent 2024  Constant TPC data
tpc.c = read.csv("2024PrapaeConstantTPCCombineddata.csv", skip = 1)


# Paste date in t.in and t.out column to make duration calculation easier
tpc.c$t.in <- paste(tpc.c$Date, tpc.c$t.in, sep = " ")
tpc.c$t.out <- paste(tpc.c$Date, tpc.c$t.out, sep = " ")

# Paste mom and individual together to create UniID
tpc.c <- tpc.c %>%
  mutate(UniID = paste(Female, Individual, sep = " "))

# Convert to POSIXct with correct formatting
tpc.c <- tpc.c %>%
  mutate(
    t.in = as.POSIXct(t.in, format = "%d-%b-%y %H:%M", tz = "UTC"),
    t.out = as.POSIXct(t.out, format = "%d-%b-%y %H:%M", tz = "UTC")
  )%>%
  group_by(UniID)%>% 
  mutate(
    first_t_out = first(t.out),
    duration = as.numeric(difftime(t.out, first_t_out, units = "hours")))

# take care of dead values and put nas if there are no numeric values in the cell
tpc.c$fw <- as.numeric(tpc.c$fw)

# calculate logarithmic scale for relative growth rate for current 2024 data set
tpc.c$rgrlog= (log10(tpc.c$fw) - log10(tpc.c$M0))/tpc.c$duration
tpc.c$time.per= "current"

# calculate arithmetic scale growth rate
tpc.c$rgrarith = (tpc.c$fw - tpc.c$M0) / tpc.c$duration
tpc.c$time.per = "current"



# Make sure that new data follows naming of old data sets
tpc.c$mom= tpc.c$Female
tpc.c$ID= tpc.c$Individual
tpc.c$UniID= paste(tpc.c$temp, tpc.c$mom, tpc.c$ID, sep=".")

# Filter caterpillars to include the ones who were active
tpc.cs <- tpc.cs %>% filter(active == "y")

# Ensure data types match for both datasets before combining
tpc.ps$mom <- as.character(tpc.ps$mom)
tpc.cs$mom <- as.character(tpc.cs$mom)

tpc.ps$time <- as.character(tpc.ps$time)
tpc.cs$time <- as.character(tpc.cs$time)

#Calculate mass gained in each time treatment
tpc.c$mgain= tpc.c$fw - tpc.c$M0

#combine historic and current
tpc.cs= tpc.c[,c("UniID","mom","ID","temp","active","instar","time","duration","mgain","rgrlog","rgrarith","time.per")]


# Combine past and current datasets while preserving time.per
tpc <- rbind(tpc.cs, tpc.ps) # code not working here - fix

##Set up durations of 6hr and 24hr
tpc.p$duration <- tpc.p$time
tpc$dur=NA
tpc$time= as.numeric(tpc$time)
tpc[tpc$time>5 & tpc$time<6.5 & tpc$time.per=="past","dur"]=6
tpc[tpc$time>20 & tpc$time<26 & tpc$time.per=="past","dur"]=24
tpc[tpc$time.per=="current","dur"]= tpc[tpc$time.per=="current","duration"]
tpc[tpc$time.per =="past", "dur"]= tpc[tpc$time.per=="past", "duration"]


## Set up durations of 6hr and 24hr
tpc <- tpc %>%
  mutate(
    durbin = case_when(
      time > 5 & time < 7 ~ 6,
      time > 23 & time < 26 ~ 24,
      duration >5 & duration < 7 ~ 6,
      duration > 23 & duration < 25 ~ 24,
      TRUE ~ NA_real_  # Default case with NA as a numeric value
    )
  )

# Ensure consistent durations for past datasets
#tpc <- tpc %>%
# mutate(dur = if_else(time.per == "past", duration, durbin))

# Save data frame to new Csv
write.csv(tpc, "PastPresentFilteredConstantTpc2024.csv")

# Create the plot for past data
past_plot <- ggplot(tpc_past[tpc_past$dur %in% c(6, 24),], aes(x = temp, y = rgr, color = mom)) +
  geom_point() +
  #geom_line(aes(group = mom), alpha = 0.7) +
  facet_grid(dur ~ instar) +
  theme_bw() +
  xlab("Temperature (C)") +
  ylab("RGR (mg/mg/h") +
  ggtitle("Historic Data (1999)") +
  scale_color_viridis(discrete = TRUE, option = "C")
print(past_plot)
setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
pdf("24hrTPCConstantHistoricData")

# Filter data for current data set
tpc_current <- tpc %>% filter(time.per == "current")
tpc_current <- tpc %>% filter(active == "y")


# Filter out nas introduced by calculating growth rate and filter out values where the caterpillars were not active
tpc_current_filtered <- tpc_current %>%
  filter(!(is.na(rgr) & durbin == 0)) 

# Exclude rows where rgr <= 0 or rgr is NA
tpc_current_filtered <- tpc_current_filtered %>%
  filter(rgr > 0 & !is.na(rgr))

#check how many have NAs
sum(is.na(tpc_current_filtered$mom))
sum(is.na(tpc_current_filtered$temp))
sum(is.na(tpc_current_filtered$rgr))
sum(is.na(tpc_current_filtered$durbin))
sum(is.na(tpc_current_filtered$instar))

# Create the plot for current data
current_plot <- ggplot(tpc_current_filtered, aes( x = temp, y = rgr, color = mom)) +
  geom_point() +
  # geom_line(aes(group = mom), alpha = 0.7) +
  facet_grid(durbin ~ instar) +
  theme_bw() +
  xlab("Temperature (C)") +
  ylab("RGR (mg/mg/h") +
  ggtitle("Present Data (2024)") +
  ylim(0, 0.17) +
  scale_color_viridis(discrete = TRUE)
print(current_plot)
setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
pdf("2024_24hrTPCConstantCurrentData")
current_plot
dev.off()


#combine both individual plots for past and current data
combined_plot <- past_plot + current_plot + plot_layout(ncol = 1)
print(combined_plot)   
tpc.plot <- ggplot(tpc[tpc$dur %in% c(6,24),], aes(x=temp, y=rgr, color=time.per)) +
  geom_point(alpha=0.5) +
  #geom_line(aes(group = mom, color = time.per), alpha = 0.7) + # lineage lines
  facet_grid(dur ~ instar) +
  theme_bw() +
  xlab("Temperature (C)") +
  ylab("RGR (mg/mg/h)") +
  ggtitle("2024 vs. 1999 Constant TPC") +
  ylim(-0.02, 11)+
  scale_color_manual(values = c("current" = "#EE6A50", "past" = "#7AC5CD"))
print(tpc.plot)
setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
pdf("2024ConstantTPCComparison")
combined_plot
dev.off()

# Aggregate mean values with standard error calculation
tpc.agg <- tpc %>%
  group_by(temp, time.per, dur, instar) %>% # need to actually calculate the precise durations and not just a flat 6 hr and 24 hr durations for the present data set
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
  ggtitle("Past vs. Present Constant TPC") +
  ylim(-0.10, 0.14) +
  scale_color_manual(values = c("current" = "#EE6A50", "past" = "#7AC5CD"))
#tpc$time.per <- factor(tpc$time.per, levels = c("past", "current")) 
print(tpc.plot)
setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
pdf("2024ConstantTPCFamilyMeans")
tpc.plot
dev.off()

#plot family mean values 
tpc.agg.f <- tpc %>%
  group_by(mom) %>%
  dplyr::summarise(rgr, na.rm=TRUE)

sum(is.na(tpc_current$rgr))
sum(is.na(tpc_past$rgr))
>>>>>>> Stashed changes

# ###separate historic and present data
# 
# # Filter data for historic data set 
# tpc_past <- tpc %>% filter(time.per == "past")
# tpc_past <- tpc %>% filter(active == "y")
# 
# # Create the plot for past data
# past_plot <- ggplot(tpc_past[tpc_past$dur %in% c(6, 24),], aes(x = temp, y = rgr, color = mom)) +
#   geom_point() +
#   #geom_line(aes(group = mom), alpha = 0.7) +
#   facet_grid(dur ~ instar) +
#   theme_bw() +
#   xlab("Temperature (C)") +
#   ylab("RGR (mg/mg/h") +
#   ggtitle("Historic Data (1999)") +
#   scale_color_viridis(discrete = TRUE, option = "C")
# print(past_plot)
# setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
# pdf("24hrTPCConstantHistoricData")
# 
# # Filter data for current data set
# tpc_current <- tpc %>% filter(time.per == "current")
# tpc_current <- tpc %>% filter(active == "y")
# 
# 
# # Filter out nas introduced by calculating growth rate and filter out values where the caterpillars were not active
# tpc_current_filtered <- tpc_current %>%
#   filter(!(is.na(rgr) & durbin == 0)) 
# 
# # Exclude rows where rgr <= 0 or rgr is NA
# tpc_current_filtered <- tpc_current_filtered %>%
#   filter(rgr > 0 & !is.na(rgr))
# 
# #check how many have NAs
# sum(is.na(tpc_current_filtered$mom))
# sum(is.na(tpc_current_filtered$temp))
# sum(is.na(tpc_current_filtered$rgr))
# sum(is.na(tpc_current_filtered$durbin))
# sum(is.na(tpc_current_filtered$instar))
# 
# # Create the plot for current data
# current_plot <- ggplot(tpc_current_filtered, aes( x = temp, y = rgr, color = mom)) +
#  geom_point() +
#  # geom_line(aes(group = mom), alpha = 0.7) +
#   facet_grid(durbin ~ instar) +
#   theme_bw() +
#   xlab("Temperature (C)") +
#   ylab("RGR (mg/mg/h") +
#   ggtitle("Present Data (2024)") +
#   ylim(0, 0.17) +
#   scale_color_viridis(discrete = TRUE)
# print(current_plot)
# setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
# pdf("2024_24hrTPCConstantCurrentData")
# current_plot
# dev.off()
# 
# 
# #combine both individual plots for past and current data
# combined_plot <- past_plot + current_plot + plot_layout(ncol = 1)
# print(combined_plot)   
# tpc.plot <- ggplot(tpc[tpc$dur %in% c(6,24),], aes(x=temp, y=rgr, color=time.per)) +
#   geom_point(alpha=0.5) +
#   #geom_line(aes(group = mom, color = time.per), alpha = 0.7) + # lineage lines
#   facet_grid(dur ~ instar) +
#   theme_bw() +
#   xlab("Temperature (C)") +
#   ylab("RGR (mg/mg/h)") +
#   ggtitle("2024 vs. 1999 Constant TPC") +
#   ylim(-0.02, 11)+
#   scale_color_manual(values = c("current" = "#EE6A50", "past" = "#7AC5CD"))
# print(tpc.plot)
# setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
# pdf("2024ConstantTPCComparison")
# combined_plot
# dev.off()
# 
# # Aggregate mean values with standard error calculation
# tpc.agg <- tpc %>%
#   group_by(temp, time.per, dur, instar) %>% # need to actually calculate the precise durations and not just a flat 6 hr and 24 hr durations for the present data set
#   dplyr::summarise(
#     mean = mean(rgr, na.rm = TRUE),
#     se = sd(rgr, na.rm = TRUE) / sqrt(n())
#   )
# #plotting family means with error bars
# tpc.plot <- ggplot(tpc.agg[tpc.agg$dur %in% c(6, 24), ],aes(x = temp, y = mean, color = time.per, group = time.per)) +
#   geom_point(alpha = 0.7, size = 2) + # Points for mean values
#   geom_errorbar(aes(ymin = mean - se, ymax = mean +se), width = 0.3, alpha = 0.6) + # Error bars
#   facet_grid(dur ~ instar) + #Facets for duration and instar
#   theme_bw() +
#   xlab("Temperature(°C)") +
#   ylab("RGR (mg/mg/hr)") +
#   ggtitle("Past vs. Present Constant TPC") +
#   ylim(-0.10, 0.14) +
#   scale_color_manual(values = c("current" = "#EE6A50", "past" = "#7AC5CD"))
#   #tpc$time.per <- factor(tpc$time.per, levels = c("past", "current")) 
#   print(tpc.plot)
#   setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
#   pdf("2024ConstantTPCFamilyMeans")
#   tpc.plot
#   dev.off()
# 
# #plot family mean values 
# tpc.agg.f <- tpc %>%
#   group_by(mom) %>%
#   dplyr::summarise(rgr, na.rm=TRUE)
# 
# sum(is.na(tpc_current$rgr))
# sum(is.na(tpc_past$rgr))
# 
