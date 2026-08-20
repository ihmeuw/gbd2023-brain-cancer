---
title: "Brain_Figure4"
author: [NAME]
date: "2025-12-02"
output: html_document
editor_options: 
  markdown: 
    wrap: 72
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

``` r
# version_ids for make_aggregates() 
dalynator_version = 102
como_version = 1762
codcorrect_version = 528

# compare_version_ids for get_outputs()
compare_version =  8352
dalynator_compare_version = 8352
como_compare_version = 8352
codcorrect_compare_version =  8352

###############################################################
# SET GLOBALS that may need modifications
###############################################################

cause_name <- "Brain"
plot_cause <- 477   # Brain
brain <- 477

both_sex <- 2
plot_sex <- both_sex
sexes <- c(1,2)

###############################################################
# load libraries
library(data.table)
library(ggplot2)
library(gridExtra)
library(patchwork)

# Load Central Functions
source("[filepath]/make_aggregates.R")
source("[filepath]/get_draws.R")
source("[filepath]/get_age_metadata.R")
source("[filepath]/get_cause_metadata.R")
source("[filepath]/get_location_metadata.R")
source("[filepath]/get_covariate_estimates.R")
source("[filepath]/get_outputs.R")
source("[filepath]/get_population.R")
source("[filepath]/get_rei_metadata.R")

#Additional Globals

all_ages <- 22
year <- 2023
release <- 16 

# measures
cases <- 6
deaths <- 1
dalys <- 2
# metrics
number <- 1
rate <- 3
percent <- 2
# locations
global <- 1
locs <- get_location_metadata(release_id = 16, location_set_id = 35)[level==1]
colors_w_global <- c("#000000", "#1b9e77", "#d95f02", "#7570b3", "#f076b4",  "#66a61e", "#e6ab02","#a6761d")
location_colors <- setNames(colors_w_global, c("Global", "Central Europe, Eastern Europe, and Central Asia", "High-income",
                                                 "Latin America and Caribbean", "North Africa and Middle East", "South Asia",
                                                 "Southeast Asia, East Asia, and Oceania", "Sub-Saharan Africa"))  
```

``` r
## INCIDENCE 

# pull FHS vals 
asir <- get_outputs("cause", release_id = 6, cause_id = plot_cause, year_id = c(2023:2050), metric_id=3, measure_id = 6,
                    age_group_id=27, location_id=c(1, locs$location_id), sex_id=sex, forecasted = TRUE)
asir <- asir[year_id>=2023,.(age_group_id, acause, location_id, year_id, location_name, mean_forecast=val*100000, upper_forecast=upper*100000, lower_forecast=lower*100000)]

# pull GBD2023 ASIRs from 1990-2023
asir_old <- get_outputs('cause',cause_id=plot_cause, release_id = release_id, year_id = c(1990:2023), 
                            sex_id = sex, age_group_id = 27, measure_id = 6, 
                            metric_id = 3, location_id = c(1, locs$location_id), 
                            compare_version_id = compare_version)

# subset to cols we need and transform to per 100000 space 
asir_old_sub <- asir_old[, .(mean_present=val*100000,
                                     lower_present=lower*100000,
                                     upper_present=upper*100000,
                                     location_id, location_name, year_id)]

# merge with FHS vals 
dt <- merge(asir_old_sub, asir, by=c("location_id", "location_name", "year_id"), all.x=T, all.y=T)

# generate scaler and apply for mean, upper and lower 
scaler <- dt[year_id==2023, .(scaler_mean=mean_present/mean_forecast), by="location_name"] # gen scaler for 2023
dt <- merge(dt, scaler, by=c("location_name"))
dt <- dt[year_id>2023, int_shift_mean := scaler_mean*mean_forecast, by="location_name"] # apply scaler for years > 2023

scaler_lower <- copy(dt)
scaler_lower <- scaler_lower[year_id==2023, current_scaler_lower := mean_present-lower_present, by="location_name"] # gen scaler for 2023
scaler_lower <- scaler_lower[year_id==2023, forecast_scaler_lower := mean_forecast-lower_forecast, by="location_name"] # gen scaler for 2023

dt <- merge(dt, scaler_lower[year_id==2023, .(current_scaler_lower,forecast_scaler_lower,location_name)], by=c("location_name"))
dt <- dt[year_id>2023, int_shift_lower := int_shift_mean-(current_scaler_lower*(mean_forecast-lower_forecast)/forecast_scaler_lower), by="location_name"] # apply scaler for years > 2023

scaler_upper <- copy(dt)
scaler_upper <- scaler_upper[year_id==2023, current_scaler_upper := upper_present-mean_present, by="location_name"] # gen scaler for 2023
scaler_upper <- scaler_upper[year_id==2023, forecast_scaler_upper := upper_forecast-mean_forecast, by="location_name"] # gen scaler for 2023

dt <- merge(dt, scaler_upper[year_id==2023, .(current_scaler_upper,forecast_scaler_upper,location_name)], by=c("location_name"))
dt <- dt[year_id>2023, int_shift_upper := int_shift_mean+(current_scaler_upper*(upper_forecast-mean_forecast)/forecast_scaler_upper), by="location_name"] # apply scaler for years > 2023

# rename based on int shifted values
dt <- dt[year_id<=2023, mean_final := mean_present]
dt <- dt[year_id<=2023, lower_final := lower_present]
dt <- dt[year_id<=2023, upper_final := upper_present]

dt <- dt[year_id>2023, mean_final := int_shift_mean]
dt <- dt[year_id>2023, lower_final := int_shift_lower]
dt <- dt[year_id>2023, upper_final := int_shift_upper]

dt <- dt[lower_final < 0, lower_final := 0.000001] # non zero floor when lower bound is < 0

dt_final_asir <- dt[,.(year_id, location_name, mean_final, lower_final, upper_final, mean_forecast, lower_forecast, upper_forecast)]
```

```{r}
## MORTALIRY 

# pull FHS vals 
asmr <- get_outputs("cause", release_id = 6, cause_id = plot_cause, year_id = c(2023:2050), metric_id=3, measure_id = 1,
                          age_group_id=27, location_id=c(1, locs$location_id), sex_id=sex, forecasted = TRUE) 

asmr <- asmr[year_id>=2023,.(age_group_id, acause, location_id, year_id, location_name, mean_forecast=val*100000, upper_forecast=upper*100000, lower_forecast=lower*100000)]

# pull GBD2023 ASMRs from 1990-2023
asmr_old <- get_outputs('cause',cause_id=plot_cause, release_id = release_id, year_id = c(1990:2023), 
                            sex_id = sex, age_group_id = 27, measure_id = 1, 
                            metric_id = 3, location_id = c(1, locs$location_id), 
                            compare_version_id = compare_version)

# subset to cols we need and transform to per 100000 space 
asmr_old_sub <- asmr_old[, .(mean_present=val*100000,
                                     lower_present=lower*100000,
                                     upper_present=upper*100000,
                                     location_id, location_name, year_id)]

# merge with FHS vals
dt <- merge(asmr_old_sub, asmr, by=c("location_id", "location_name", "year_id"), all.x=T, all.y=T)

# generate scaler and apply for mean, upper and lower 
scaler <- dt[year_id==2023, .(scaler_mean=mean_present/mean_forecast), by="location_name"] # gen scaler for 2023
dt <- merge(dt, scaler, by=c("location_name"))
dt <- dt[year_id>2023, int_shift_mean := scaler_mean*mean_forecast, by="location_name"] # apply scaler for years > 2023

scaler_lower <- copy(dt)
scaler_lower <- scaler_lower[year_id==2023, current_scaler_lower := mean_present-lower_present, by="location_name"] # gen scaler for 2023
scaler_lower <- scaler_lower[year_id==2023, forecast_scaler_lower := mean_forecast-lower_forecast, by="location_name"] # gen scaler for 2023

dt <- merge(dt, scaler_lower[year_id==2023, .(current_scaler_lower,forecast_scaler_lower,location_name)], by=c("location_name"))
dt <- dt[year_id>2023, int_shift_lower := int_shift_mean-(current_scaler_lower*(mean_forecast-lower_forecast)/forecast_scaler_lower), by="location_name"] # apply scaler for years > 2023

scaler_upper <- copy(dt)
scaler_upper <- scaler_upper[year_id==2023, current_scaler_upper := upper_present-mean_present, by="location_name"] # gen scaler for 2023
scaler_upper <- scaler_upper[year_id==2023, forecast_scaler_upper := upper_forecast-mean_forecast, by="location_name"] # gen scaler for 2023

dt <- merge(dt, scaler_upper[year_id==2023, .(current_scaler_upper,forecast_scaler_upper,location_name)], by=c("location_name")) # gen scaler for 2023
dt <- dt[year_id>2023, int_shift_upper := int_shift_mean+(current_scaler_upper*(upper_forecast-mean_forecast)/forecast_scaler_upper), by="location_name"] # apply scaler for years > 2023

# rename based on int shifted values
dt <- dt[year_id<=2023, mean_final := mean_present]
dt <- dt[year_id<=2023, lower_final := lower_present]
dt <- dt[year_id<=2023, upper_final := upper_present]

dt <- dt[year_id>2023, mean_final := int_shift_mean]
dt <- dt[year_id>2023, lower_final := int_shift_lower]
dt <- dt[year_id>2023, upper_final := int_shift_upper]

dt <- dt[lower_final < 0, lower_final := 0.000001] # non zero floor when lower bound is < 0

dt_final_asmr <- dt[,.(year_id, location_name, mean_final, lower_final, upper_final, mean_forecast, lower_forecast, upper_forecast)]

```

```{r}
#plot

# set order based on loc name 
dt_final_asir$location_name <- factor(dt_final_asir$location_name, levels = c("Global", "Central Europe, Eastern Europe, and Central Asia", "High-income", "Latin America and Caribbean", "North Africa and Middle East", "South Asia", "Southeast Asia, East Asia, and Oceania", "Sub-Saharan Africa"))

dt_final_asmr$location_name <- factor(dt_final_asmr$location_name, levels = c("Global", "Central Europe, Eastern Europe, and Central Asia", "High-income", "Latin America and Caribbean", "North Africa and Middle East", "South Asia", "Southeast Asia, East Asia, and Oceania", "Sub-Saharan Africa"))

plot1 <- ggplot(data=dt_final_asir)+
  geom_ribbon(aes(x=year_id, ymin = lower_final, ymax = upper_final, fill = location_name,color=NULL), alpha=0.15)+
  # Historical solid line (1990-2023)
  geom_line(data = dt_final_asir[year_id <= 2024],
            aes(x = year_id, y = mean_final, color = location_name),
            linetype = "solid", linewidth = 1) +
  # Forecast dashed line (2024-2050)
  geom_line(data = dt_final_asir[year_id >= 2023],
            aes(x = year_id, y = mean_final, color = location_name),
            linetype = "dashed", linewidth = 1) +
  scale_y_continuous(expand = c(0,0), labels = label_comma(), limits=c(0,max(dt_final_asir$upper_final)*1.2)) + 
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2030, 2040, 2050), limits = c(1990, 2051), expand = c(0, 0)) + # set x-axis
  scale_color_manual(values = colors_w_global, name = "GBD super-region") + 
  scale_fill_manual(values = colors_w_global, name = "GBD super-region") +
  theme(panel.spacing = unit(1, units = "cm"), 
        strip.placement = "outside", 
        strip.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
  ) +
  labs(x="Year", y="Age-standardised incidence rate (per 100,000)") 


plot2 <- ggplot(data=dt_final_asmr)+
  geom_ribbon(aes(x=year_id, ymin = lower_final, ymax = upper_final, fill = location_name,color=NULL), alpha=0.15)+
  # Historical solid line (1990-2023)
  geom_line(data = dt_final_asmr[year_id <= 2024],
            aes(x = year_id, y = mean_final, color = location_name),
            linetype = "solid", linewidth = 1) +
  # Forecast dashed line (2024-2050)
  geom_line(data = dt_final_asmr[year_id >= 2023],
            aes(x = year_id, y = mean_final, color = location_name),
            linetype = "dashed", linewidth = 1) +
  scale_y_continuous(expand = c(0,0), labels = label_comma(), limits=c(0,max(dt_final_asmr$upper_final)*1.2)) + 
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2030, 2040, 2050), limits = c(1990, 2051), expand = c(0, 0)) + # set x-axis
  scale_color_manual(values = colors_w_global, name = "GBD super-region") + 
  scale_fill_manual(values = colors_w_global, name = "GBD super-region") +
  theme(panel.spacing = unit(1, units = "cm"),
        strip.placement = "outside", 
        strip.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
  ) +
  labs(x="Year", y="Age-standardised mortality rate (per 100,000)") 


pdf(file = paste0("[filepath]", "_Figure4.pdf"), height = 9, width = 11, pointsize = 20)
full_plot <- grid.arrange(plot1, plot2, ncol = 1)
dev.off()
```
