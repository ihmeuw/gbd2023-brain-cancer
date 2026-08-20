---
title: "Brain_Figure2"
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
wbi_metadata <- get_location_metadata(location_set_id=26, release_id=release)[level==1]
wbi_locs <- as.vector(wbi_metadata$location_id)
wbi_set <- wbi_metadata[1, location_set_id]

ages <- get_age_metadata(age_group_set_id = 24, release_id = release)
ages <- ages[, age_midpoint := round(((age_group_years_start + age_group_years_end)/2),1)]
ages <- ages[age_group_years_end >5, .(age_group_id, age_group_alternative_name, age_midpoint, age_group_years_end)]
youngest <- data.table(age_group_id=1,
                       age_group_alternative_name = "0-4 years",
                       age_midpoint=2,
                       age_group_years_end=4)
ages <- rbind(youngest, ages, fill=TRUE)
ages <- ages[, age_group_name_plot := gsub(" years", "", age_group_alternative_name)]


order = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
          "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+")
```

``` r
# pull data
dt <- get_outputs('cause',cause_id=plot_cause, release_id = release, year_id = year, sex_id = sexes, age_group_id = ages$age_group_id, measure_id = c(cases, deaths), metric_id = c(number, rate), location_id = global, compare_version_id = compare_version)

dt <- merge(dt,ages, by="age_group_id")
# Remove rows where val is NA
dt <- dt[!is.na(val)]
dt <- dt[!val==0]


fwrite(dt, paste0("[filepath]", "_figure2_inputs.csv")) # save out inputs

dt <- dt[metric_name=="Rate"&measure_name=="Incidence", measure := "Incidence"]
dt <- dt[metric_name=="Rate"&measure_name=="Deaths", measure := "Mortality"]
dt <- dt[metric_name=="Number"&measure_name=="Incidence", measure := "Cases"]
dt <- dt[metric_name=="Number"&measure_name=="Deaths", measure := "Deaths"]

dt_rate <- dt[metric_name=="Rate"]
dt_number <- dt[metric_name=="Number"]

dt_rate$age_group_name_plot <- factor(dt_rate$age_group_name_plot, levels = order)

plot_f_rates <- ggplot(data=dt_rate[sex_id==2]) + 
  geom_ribbon(aes(x = age_group_name_plot, ymin = lower*100000, ymax = upper*100000, fill = measure, group = measure), alpha = 0.5) +
  geom_line(aes(x = age_group_name_plot, y = val*100000, color = measure, group = measure)) +
  scale_fill_manual(values = c("Incidence" = "#3333CD", "Mortality" = "#CD3333")) + # Adjust colors as needed
  scale_color_manual(values = c("Incidence" = "#23238B", "Mortality" = "#8B2323")) + # Adjust colors as needed
  theme(
    panel.spacing = unit(1, units = "cm"), 
    strip.placement = "outside", 
    strip.background = element_rect(fill = "white"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.text = element_text(size = 10),
    legend.key.size = unit(.5, 'cm'),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "Age group (Years)", y = "Age-specific rate (per 100,000)", color = "Measure", fill = "Measure") +
  scale_x_discrete(limits = dt_rate$age_group_name_plot, expand = c(0,0)) + # Use scale_x_discrete for factor or character x values
  #scale_x_continuous(breaks = seq(from = min(dt_rate$age_midpoint)-2, to = max(dt_rate$age_midpoint)-2, by = 10), limits = c(NA, 100)) +
  scale_y_continuous(limits = c(0, max(dt_rate$upper*100000)), expand = c(0,0))
  #print(plot_f_rates)

plot_m_rates <- ggplot(data=dt_rate[sex_id==1]) + 
  geom_ribbon(aes(x = age_group_name_plot, ymin = lower*100000, ymax = upper*100000, fill = measure,  group = measure), alpha = 0.5) +
  geom_line(aes(x = age_group_name_plot, y = val*100000, color = measure,  group = measure)) +
  scale_fill_manual(values = c("Incidence" = "#3333CD", "Mortality" = "#CD3333")) + # Adjust colors as needed
  scale_color_manual(values = c("Incidence" = "#23238B", "Mortality" = "#8B2323")) + # Adjust colors as needed
  theme(
    panel.spacing = unit(1, units = "cm"), 
    strip.placement = "outside", 
    strip.background = element_rect(fill = "white"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.text = element_text(size = 10),
    legend.key.size = unit(.5, 'cm'),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "Age group (Years)", y = "Age-specific rate (per 100,000)", color = "Measure", fill = "Measure") +
  scale_x_discrete(limits = dt_rate$age_group_name_plot, expand = c(0,0)) + # Use scale_x_discrete for factor or character x values
  #scale_x_continuous(breaks = seq(from = min(dt_rate$age_midpoint)-2, to = max(dt_rate$age_midpoint)-2, by = 10), limits = c(NA, 100)) +
  scale_y_continuous(limits = c(0, max(dt_rate$upper*100000)), expand = c(0,0))
#print(plot_m_rates)

plot_f_num <- ggplot(data=dt_number[sex_id==2]) + 
  geom_ribbon(aes(x = age_group_name_plot, ymin = lower, ymax = upper, fill = measure,  group = measure), alpha = 0.5) +
  geom_line(aes(x = age_group_name_plot, y = val, color = measure,  group = measure)) +
  scale_fill_manual(values = c("Cases" = "#3333CD", "Deaths" = "#CD3333")) + # Adjust colors as needed
  scale_color_manual(values = c("Cases" = "#23238B", "Deaths" = "#8B2323")) + # Adjust colors as needed
  theme(
    panel.spacing = unit(1, units = "cm"), 
    strip.placement = "outside", 
    strip.background = element_rect(fill = "white"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.text = element_text(size = 10),
    legend.key.size = unit(.5, 'cm'),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "Age group (Years)", y = "Age-specific count", color = "Measure", fill = "Measure") +
  scale_x_discrete(limits = dt_rate$age_group_name_plot, expand = c(0,0)) + # Use scale_x_discrete for factor or character x values
  #scale_x_continuous(breaks = seq(from = min(dt_number$age_midpoint)-2, to = max(dt_number$age_midpoint)-2, by = 10), limits = c(NA, 100)) +
  scale_y_continuous(limits = c(0, max(dt_number$upper)*1.1), expand = c(0,0), labels = scales::comma)
#print(plot_f_num)

plot_m_num <- ggplot(data=dt_number[sex_id==1]) + 
  geom_ribbon(aes(x = age_group_name_plot, ymin = lower, ymax = upper, fill = measure,  group = measure), alpha = 0.5) +
  geom_line(aes(x = age_group_name_plot, y = val, color = measure,  group = measure)) +
  scale_fill_manual(values = c("Cases" = "#3333CD", "Deaths" = "#CD3333")) + # Adjust colors as needed
  scale_color_manual(values = c("Cases" = "#23238B", "Deaths" = "#8B2323")) + # Adjust colors as needed
  theme(
    panel.spacing = unit(1, units = "cm"), 
    strip.placement = "outside", 
    strip.background = element_rect(fill = "white"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.text = element_text(size = 10),
    legend.key.size = unit(.5, 'cm'),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "Age group (Years)", y = "Age-specific count", color = "Measure", fill = "Measure") +
  scale_x_discrete(limits = dt_rate$age_group_name_plot, expand = c(0,0)) + # Use scale_x_discrete for factor or character x values
  #scale_x_continuous(breaks = seq(from = min(dt_number$age_midpoint)-2, to = max(dt_number$age_midpoint)-2, by = 10), limits = c(NA, 100)) +
  scale_y_continuous(limits = c(0, max(dt_number$upper)*1.1), expand = c(0,0), labels = scales::comma)
#print(plot_m_num)


pdf(file = paste0("[filepath]", "_Figure2.pdf"), height = 10, width = 12, pointsize = 20)
  full_plot <- grid.arrange(plot_f_rates, plot_f_num, plot_m_rates, plot_m_num, ncol = 2)

dev.off()
```
