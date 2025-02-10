# Taylor M. Hatcher 
# This script visualizes the constant TPC data from Joel Kingsolver in 1999 and recent constant TPC data 
#
install.packages("lme4")
install.packages("DHARMa")
install.packages("car")
# Load required libraries 
library(ggplot2)
library(data.table)
library(dplyr)
library(patchwork)
library(reshape2)
library(viridis)
library(tidyverse)
library(lubridate)
library(car)

# set working directory to github repository
setwd("~/Desktop/Repos/WARP2024/Data")

tpcvis <- read.csv("PastPresentFilteredConstantTpc2024.csv")

# Filter out current data set
tpcvis_current <- tpcvis %>%
  filter(time.per == "current")

# Plot rgrlog growth rate for 2024 constant tpc data set
current_plot_log <- ggplot(tpcvis_current, aes( x = temp, y = rgrlog, color = mom)) +
  geom_point() +
  #geom_line(aes(group = mom), alpha = 0.7) +
  facet_grid(durbin ~ instar) +
  theme_bw() +
  xlab("Temperature (C)") +
  ylab("RGR (mg/mg/h") +
  ggtitle("Present Data (2024)") +
  ylim(0, 0.20) +
  scale_color_viridis(discrete = TRUE)
  print(current_plot_log)
  
# Plot rgrarith growth rate for 2024 constant tpc data set
current_plot_arith <- ggplot(tpcvis_current, aes( x = temp, y = rgrarith, color = mom)) +
  geom_point() +
  #geom_line(aes(group = mom), alpha = 0.7) +
  facet_grid(durbin ~ instar) +
  theme_bw() +
  xlab("Temperature (C)") +
  ylab("Arith. Growth Rate") +
  ggtitle("TPC Constant Present Data (2024)") +
  ylim(0, 12.00) +
  scale_color_viridis(discrete = TRUE) 
  print(current_plot_arith)

# Combined plots comparing log scale vs arith scale for current data
  combined_current_scale_comparison_plot <- current_plot_log + current_plot_arith + plot_layout(ncol = 1)
  print(combined_current_scale_comparison_plot)  
  
# Filter out past/historic data set
  tpcvis_past <- tpcvis %>%
    filter(time.per == "past")
  
# Plot rgrlog growth rate for 1999 constant tpc data set
    historic_plot_log <- ggplot(tpcvis_past, aes( x = temp, y = rgrlog, color = mom)) +
    geom_point() +
    #geom_line(aes(group = mom), alpha = 0.7) +
    facet_grid(durbin ~ instar) +
    theme_bw() +
    xlab("Temperature (°C)") +
    ylab("RGR (mg/mg/h") +
    ggtitle("Log Scale Historic Data (1999)") +
    ylim(0, 0.20) +
    scale_color_viridis(discrete = TRUE)
  print(historic_plot_log)

# Plot rgrarith growth rate for 1999 constant tpc data set 
  historic_plot_arith <- ggplot(tpcvis_past, aes( x = temp, y = rgrarith, color = mom)) +
    geom_point() +
    #geom_line(aes(group = mom), alpha = 0.7) +
    facet_grid(durbin ~ instar) +
    theme_bw() +
    xlab("Temperature (°C)") +
    ylab("Arith RGR (mg/mg/h)") +
    ggtitle("Arith Scale Historic Data (1999)") +
    ylim(0, 12.00) +
    scale_color_viridis(discrete = TRUE)
    print(historic_plot_arith)
    
# Combine both scale plots for historic 1999 data set    
combined_historic_scale_comparison_plot <- historic_plot_log + historic_plot_arith + plot_layout(ncol = 1)
    print(combined_historic_scale_comparison_plot)  

# Plot both historic and current log scales  
  tpc.logplot <- ggplot(tpcvis, aes(x=temp, y=rgrlog, color=time.per)) +
      geom_point(alpha=0.5) +
      #geom_line(aes(group = mom, color = time.per), alpha = 0.7) + # lineage lines
      facet_grid(durbin ~ instar) +
      theme_bw() +
      xlab("Temperature (C)") +
      ylab("RGR (mg/mg/h)") +
      ggtitle("2024 vs. 1999 Constant TPC Log Scale RGR") +
      ylim(-0.02, 0.12)+
      scale_color_manual(values = c("current" = "#EE6A50", "past" = "#7AC5CD"))
    print(tpc.logplot)
    #setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
    pdf("2024ConstantTPCComparison")
    dev.off()

# Plot both historic and current arithmetic scales
    tpc.arithplot <- ggplot(tpcvis, aes(x=temp, y=rgrarith, color=time.per)) +
      geom_point(alpha=0.5) +
     # geom_line(aes(group = mom, color = time.per), alpha = 0.7) + # lineage lines
      facet_grid(durbin ~ instar) +
      theme_bw() +
      xlab("Temperature (C)") +
      ylab("RGR (mg/mg/h)") +
      ggtitle("2024 vs. 1999 Constant TPC Arithmetic Scale RGR") +
      ylim(-0.02, 11.0)+
      scale_color_manual(values = c("current" = "#EE6A50", "past" = "#7AC5CD"))
    print(tpc.arithplot)
    #setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
    pdf("2024ConstantTPCComparison")
    dev.off()
  
    
    
# Aggregate mean values with standard error calculation for logarithmic scale
    tpc.agglog <- tpcvis %>%
     group_by(temp, time.per, durbin, instar) %>% # need to actually calculate the precise durations and not just a flat 6 hr and 24 hr durations for the present data set
    dplyr::summarise(
     mean = mean(rgrlog, na.rm = TRUE),
     se = sd(rgrlog, na.rm = TRUE) / sqrt(n())
     )
    
# plotting family means w duration
# plotting family means with error bars
  tpc.plotlog <- ggplot(tpc.agglog[tpc.agglog$durbin %in% c(6, 24), ],aes(x = temp, y = mean, color = time.per, group = time.per)) +
  geom_point(alpha = 0.7, size = 2) + # Points for mean values
    geom_errorbar(aes(ymin = mean - se, ymax = mean +se), width = 0.3, alpha = 0.6)+ # Error bars
  facet_grid(durbin ~ instar) + #Facets for duration and instar
  theme_bw() +
  xlab("Temperature(°C)") +
  ylab("RGR (mg/mg/hr)") +
  ggtitle("Past vs. Present Constant TPC Logarithmic Scale") +
  ylim(-0.10, 0.14) +
  scale_color_manual(values = c("current" = "#EE6A50", "past" = "#7AC5CD"))
  tpcvis$time.per <- factor(tpcvis$time.per, levels = c("past", "current")) 
  print(tpc.plotlog)
    
  
# Aggregate mean values with standard error calculation for arithmetic scale 
  tpc.aggarith <- tpcvis%>%
    group_by(temp, time.per, durbin, instar) %>% # need to actually calculate the precise durations and not just a flat 6 hr and 24 hr durations for the present data set
    dplyr::summarise(
      mean = mean(rgrarith, na.rm = TRUE),
      se = sd(rgrarith, na.rm = TRUE) / sqrt(n())
    )
  
# plotting family means with error bars
  tpc.plot.arith <- ggplot(tpc.aggarith[tpc.aggarith$durbin %in% c(6, 24), ],aes(x = temp, y = mean, color = time.per, group = time.per)) +
    geom_point(alpha = 0.7, size = 2) + # Points for mean values
    geom_errorbar(aes(ymin = mean - se, ymax = mean +se), width = 0.3, alpha = 0.6) + # Error bars
    facet_grid(durbin ~ instar) + #Facets for duration and instar
    theme_bw() +
    xlab("Temperature(°C)") +
    ylab("RGR (mg/mg/hr)") +
    ggtitle("Past vs. Present Constant TPC Arithmetic Scale") +
    ylim(-0.10, 6.00) +
    scale_color_manual(values = c("current" = "#EE6A50", "past" = "#7AC5CD"))
  tpcvis$time.per <- factor(tpcvis$time.per, levels = c("past", "current")) 
  print(tpc.plot.arith)  
  
  
#plot family mean values 
  tpc.agg.f <- tpcvis %>%
    group_by(mom, temp, durbin, time.per, instar) %>%
    dplyr::summarise(mean_rgrlog = mean(rgrlog, na.rm=TRUE))  
  

tpc.plot.agg.f <- ggplot(tpc.agg.f[tpc.agg.f$durbin %in% c(6,24), ], aes(x = temp, y = rgrlog, color = mom, group = time.per)) +
  geom_point(alpha = 0.7, size = 2) + # Points for mean values
  facet_grid(durbin ~ instar) + #Facets for duration and instar
  theme_bw() +
  xlab("Temperature(°C)") +
  ylab("RGR (mg/mg/hr)") +
  ggtitle("Past vs. Present Constant TPC Family Mean Log Scale") +
  ylim(-0.10, 0.2)
print(tpc.plot.agg.f) 



# Filter na's from rgrlog out
tpcvis_cleancurrent <- tpcvis_current %>% filter(is.finite(rgrlog))

# Temp is a factor for analyzing
tpcvis_cleancurrent$temp <- as.factor(tpcvis_cleancurrent$temp)

# Analysis approach
# 1. Fit the mixed model with a quadratic time effect
# 2. Check model fit using summary & ANOVA
# 3. Simulate residuals with DHARMa
# 4. Run residual diagnostics 
# 5. Compare models (linear vs. quadratic)
# 6. Refit the final model using REML
# 7. Visualize residuals and predictions using DHARMa 
library(lme4)
# Fit mixed model with a quadratic time effect-- rgrlog= response variable, poly(duration, 2) - models duration as a quadratic polynomial, temp is a catergorical fixed effect, (1 | ID) random intercept for indivdual
mod.lmer <- lmer(rgrlog ~ poly(duration, 2) + temp + (1 | ID),
                 data = tpcvis_cleancurrent,
                 REML = FALSE)

summary(mod.lmer)
anova(mod.lmer)

# Simulate residuals
sim_res <- simulateResiduals(fittedModel = mod.lmer)

print(sim_res)
dev.off()

plot(sim_res)
# Check for uniformity
testUniformity(sim_res)

# Check for overdispersion
testDispersion(sim_res)

# Check for outliers
testOutliers(sim_res)

mod.lmer_linear <- lmer(rgrlog ~ duration + temp + (1 | ID), 
                        data = tpcvis_cleancurrent, REML = FALSE)

anova(mod.lmer_linear, mod.lmer)  # Compare linear vs. quadratic



mod.lmer_final <- lmer(rgrlog ~ poly(duration, 2) + temp + (1 | ID), 
                       data = tpcvis_cleancurrent, 
                       REML = TRUE)  # Now using REML


library(DHARMa)
# Simulate residuals
sim_res <- simulateResiduals(fittedModel = mod.lmer_final)

# Plot residual diagnostics
plot(sim_res)


library(ggplot2)

# Extract predictions
tpcvis_cleancurrent$predicted <- predict(mod.lmer_final)

ggplot(tpcvis_cleancurrent, aes(x = predicted, y = rgrlog)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Observed vs. Predicted Values",
       x = "Predicted Log Relative Growth Rate",
       y = "Observed Log Relative Growth Rate") +
  theme_minimal()








