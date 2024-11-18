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
tpc[tpc$time>5 & tpc$time<11 & tpc$time.per=="past","dur"]=6
tpc[tpc$time>20 & tpc$time<26 & tpc$time.per=="past","dur"]=24
tpc[tpc$time.per=="current","dur"]= tpc[tpc$time.per=="current","duration"]

tpc.plot = ggplot(tpc[tpc$dur %in% c(6,24),], aes(x=temp, y=rgr, color=time.per)) +
  geom_point(alpha=0.5) +
  facet_grid(dur ~ instar) +
  theme_bw() +
  xlab("Temperature (C)") +
  ylab("RGR (mg/mg/h)") +
  ggtitle("2024") +
  ylim(-0.02, 0.14)
# Aggregate mean values with corrected standard error calculation
    tpc.agg <- tpc %>%
      group_by(temp, time.per, dur, instar) %>%
      dplyr::summarise(
        mean = mean(rgr, na.rm = TRUE),
        se = sd(rgr, na.rm = TRUE) / sqrt(n())
      )
    tpc$time.per <- factor(tpc$time.per, levels = c("past", "current"))
    
    #plot with error bars
    tpc.plot <- ggplot(tpc.agg[tpc.agg$dur %in% c(6, 24), ],aes(x = temp, y = mean, color = time.per, group = time.per)) +
      geom_point(alpha = 0.7, size = 2) + # Points for mean values
      geom_errorbar(aes(ymin = mean - se, ymax = mean +se), width = 0.3, alpha = 0.6) + # Error bars
      facet_grid(dur ~ instar) + #Facets for duration and instar
      theme_bw() +
      xlab("Temperature(°C)") +
      ylab("RGR (mg/mg/hr)") +
      ggtitle("Past vs. Present Constant TPC")
    ylim(-0.02, 0.14)
    
    
    
    # Fit a linear model and calculate predictions for each group
    tpc.fit <- tpc.valid %>%
      group_by(time.per, dur, instar) %>%
      do({
        model <- lm(mean ~ temp, data = .)
        data.frame(
          temp = seq(min(.$temp, na.rm = TRUE), max(.$temp, na.rm = TRUE), length.out = 100),  # Generate temperature sequence
          mean = predict(model, newdata = data.frame(temp = seq(min(.$temp, na.rm = TRUE), max(.$temp, na.rm = TRUE), length.out = 100))),
          time.per = unique(.$time.per),
          dur = unique(.$dur),
          instar = unique(.$instar)
        )
      })
    
    
    
    # Print the plot
    print(tpc.plot)
    
setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
pdf("2024TRNfeeding_constant.pdf",height = 8, width = 8)
# Display the final plot
print(tpc.plot)

