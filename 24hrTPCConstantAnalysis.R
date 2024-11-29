## Taylor M. Hatcher
## Analysis of 6 hr and 24hr constant TPCs for P. rapae. Each caterpillar only experienced one temperature for 24 hours. 
library(ggplot2)
library(data.table)
library(dplyr)
library(patchwork)
library(reshape2)
library(viridis)
library(tidyverse)

setwd("~/Desktop/Repos/WARP2024")

#past data 
tpc1 = read.csv("PrapaeW.1999.ConstantTempTPCs.4thinstar.jul2021.xlsx - data.csv")
tpc1$instar=4

#past data that includes a 6 hour time weighing: "Prapae.NC_WA.2000.TRNS.leaf" for 4th instar caterpillars
#past data that includes some 9 hour weighings, some 6.5 hour weighings, and 24.5 hour weighings
tpc2 = read.csv("PrapaeW.1999.ConstantTempTPCs.5thinstar.jul2021.xlsx - data.csv")
names(tpc2)=names(tpc1)
tpc2$instar=5

tpc.p= rbind(tpc1, tpc2)

tpc.p$rgr= (log(tpc.p$fw) - log(tpc.p$Mo))/tpc.p$time
tpc.p$time.per= "past"

tpc.ps= tpc.p[,c("UniID","mom","ID","temp","instar","time","duration","mgain","rgr","time.per")]
#plot
ggplot(tpc.ps[tpc.ps$time>5 & tpc.ps$time<11,], aes(x=time, color=factor(instar), group=factor(instar)))+
  geom_density()
table(tpc.ps$time, tpc.ps$instar)

#recent 2024 TPC data
tpc.c = read.csv("2024PrapaeConstantTPCCombineddata.csv")


# Convert fw to numeric and drop nas
tpc.c$fw <- as.numeric(tpc.c$fw)

tpc.c$rgr= (log(tpc.c$fw) - log(tpc.c$M0))/tpc.c$duration
tpc.c$time.per= "current"

tpc.c$mom= tpc.c$Female
tpc.c$ID= tpc.c$Individual
tpc.c$UniID= paste(tpc.c$temp, tpc.c$mom, tpc.c$ID, sep=".")
#Calculate mass gained in each time treatment
tpc.c$mgain= tpc.c$fw - tpc.c$M0

#combine historic and current
tpc.cs= tpc.c[,c("UniID","mom","ID","temp","instar","time","duration","mgain","rgr","time.per")]


tpc= rbind(tpc.cs, tpc.ps)


######Plot

##Set up durations of 6hr and 24hr

tpc$dur=NA
tpc$time= as.numeric(tpc$time)
# tpc[tpc$time>5 & tpc$time<11 & tpc$time.per=="past","dur"]=6
tpc[tpc$time>5 & tpc$time<6.6 & tpc$time.per=="past","dur"]=6
tpc[tpc$time>20 & tpc$time<26 & tpc$time.per=="past","dur"]=24
tpc[tpc$time.per=="current","dur"]= tpc[tpc$time.per=="current","duration"]

#tpc.plot = ggplot(tpc[tpc$dur %in% c(6,24),], aes(x=temp, y=rgr, color=time.per)) +
# geom_point(alpha=0.5) +
#  facet_grid(dur ~ instar) +
#  theme_bw() +
# xlab("Temperature (C)") +
#  ylab("RGR (mg/mg/h)") +
#  ggtitle("2024") +
# ylim(-0.10, 0.14)

#Plot family mean values for constant tpc
#tpc.agg.f <- tpc %>% # new code added 11-22-24
  #group_by(mom, temp, duration, instar)%>% # new code added 11-22-24
 # dplyr::summarise(mean = mean(rgr, na.rm = TRUE), # new code added 11-22-24E)
 #                  se = sd(rgr, na.rm = TRUE) /sqrt(n())
  )# new code added 11-22-24
# Aggregating by temperature, duration, and instar
# Aggregate mean values with corrected standard error calculation
tpc.agg <- tpc %>%
  group_by(temp, time.per, dur, instar) %>%
  dplyr::summarise(
    mean = mean(rgr, na.rm = TRUE),
    se = sd(rgr, na.rm = TRUE) / sqrt(n())
  )
#This piece of code is ensuring that 'time.per' is a factor with specified levels
tpc$time.per <- factor(tpc$time.per, levels = c("past", "current"))

# Plot individual means and family means
tpc.plot <- ggplot() +
  # Layer for individual-level aggregated data
  geom_point(
    data = tpc.agg[tpc.agg$dur %in% c(6, 24), ],
    aes(x = temp, y = mean, color = time.per, group = time.per),
    alpha = 0.7, size = 2
  )+
  geom_errorbar(
    data = tpc.agg[tpc.agg$dur %in% c(6, 24), ],
    aes(x = temp, ymin = mean - se, ymax = mean + se, color = time.per, group = time.per),
    width = 0.3, alpha = 0.6) +
  # ) +
  # # Line connecting family-level data
  # geom_line(
  #   data = tpc.agg.f,
  #   aes(x = temp, y = mean, group = mom),
  #   color = "black", linetype = "solid", size = 1
  # ) +
   # Layer for family-level aggregated data
   geom_point(
     data = tpc.agg.f,
     aes(x = temp, y = mean)) +
 #    color = "blue", size = 1, stroke = 1.2) +
  # geom_errorbar(
   #  data = tpc.agg.f,
   #  aes(x = temp, ymin = mean - se, ymax = mean + se, group = mom),
  # width = 0.3, color = "red", alpha = 0.7
  # ) +
  facet_grid(dur ~ instar) + #facet by duration and instar
  theme_bw() +
  xlab("Temperature (°C") +
  ylab("RGR (mg/mg/hr)") +
  ggtitle("Past vs. Present Constant TPC with Family Means") +
  ylim(-0.10, 0.14)


#plotting family means with error bars
#   tpc.plot <- ggplot(tpc.agg[tpc.agg$dur %in% c(6, 24), ],aes(x = temp, y = mean, color = time.per, group = time.per)) +
#   geom_point(alpha = 0.7, size = 2) + # Points for mean values
#  geom_errorbar(aes(ymin = mean - se, ymax = mean +se), width = 0.3, alpha = 0.6) + # Error bars
#  facet_grid(dur ~ instar) + #Facets for duration and instar
#  theme_bw() +
#  xlab("Temperature(°C)") +
#  ylab("RGR (mg/mg/hr)") +
#  ggtitle("Past vs. Present Constant TPC")
# ylim(-0.10, 0.14)




# Fit a linear model and calculate predictions for each group
#tpc.fit <- tpc.valid %>%
# group_by(time.per, dur, instar) %>%
# do({
# model <- lm(mean ~ temp, data = .)
#  data.frame(
#   temp = seq(min(.$temp, na.rm = TRUE), max(.$temp, na.rm = TRUE), length.out = 100),  # Generate temperature sequence
#   mean = predict(model, newdata = data.frame(temp = seq(min(.$temp, na.rm = TRUE), max(.$temp, na.rm = TRUE), length.out = 100))),
#   time.per = unique(.$time.per),
#   dur = unique(.$dur),
#  instar = unique(.$instar)
# )
#   })



# Print the plot
print(tpc.plot)

setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
pdf("2024TRNfeeding_constant.pdf",height = 8, width = 8)
# Display the final plot
print(tpc.plot)
tpc.agg.fh$year=1999
colnames(tpc.agg.fh)[1]="FEMALE"
tpc.agg.fh$FEMALE= as.character(tpc.agg.fh$FEMALE)
tpc.agg.f$FEMALE= as.character(tpc.agg.f$FEMALE)
tpc.agg.f.all= rbind(tpc.agg.f, tpc.agg.fh)
tpc.agg.f.all$yrfemale= paste(tpc.agg.f.all$year, tpc.agg.f.all$FEMALE)

tpc.all.plot= ggplot(tpc.agg.f.all, aes(x=temp,y=mean, col=factor(year)))+
  geom_line(aes(group=yrfemale)) +scale_color_viridis_d()

#add means
tpc.all.plot= tpc.all.plot + 
  geom_errorbar(data=tpc.agg.all, aes(x=temp, y=mean, ymin=mean-se, ymax=mean+se, col=factor(year)), width=0)+
  geom_point(data=tpc.agg.all, aes(x=temp, y = mean, col=factor(year)), size=3)+
  theme_bw()+xlab("Temperature (C)")+ylab("RGR (g/g/h)")+
  ggtitle("1999 & 2024") +ylim(-0.02,0.06) 
