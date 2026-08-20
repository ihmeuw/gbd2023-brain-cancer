---
title: "Brain_Figure3"
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
locs <- get_location_metadata(location_set_id = 35, release_id = 16)
locs_glb_regions <- locs[level %in% c(0,2)]

ages <- get_age_metadata(age_group_set_id = 24, release_id = release)

pop <- get_population(release_id = release, year_id = c(1990,2023), sex_id = c(1,2), 
                      location_id = c(locs_glb_regions$location_id), age_group_id = c(ages$age_group_id))
pop_age <- pop[,.(location_id, sex_id, year_id, age_group_id, population_age=population)]

age_structure <- pop[, age_structure := population/sum(population), by = c("location_id", "sex_id", "year_id")] # make age structure variable 
population <- pop[, population := sum(population), by = c("location_id", "sex_id", "year_id")] # make pop variable 

paf <- get_outputs("rei", rei_id = 169, cause_id = plot_cause, location_id=c(locs_glb_regions$location_id), sex_id=c(1,2), year_id = c(1990,2023), release_id = release, measure_id = dalys, metric_id = percent, age_group_id=c(ages$age_group_id), compare_version_id = compare_version) # get pafs
```

``` r
paf_cause <- paf[cause_id==plot_cause,.(age_group_id, location_id, sex_id, year_id, paf = val)] # get PAF risk factor variable

daly <- get_outputs('cause',cause_id=plot_cause, release_id = release, year_id = c(1990,2023),
                   sex_id = c(1,2), age_group_id = c(ages$age_group_id), measure_id = c(dalys), 
                   metric_id = c(number,rate), location_id = c(locs_glb_regions$location_id), 
                   compare_version_id = compare_version)

daly_num <- daly[metric_id==number,.(age_group_id, location_id, year_id, sex_id, daly_num=val)]
daly_rate <- daly[metric_id==rate,.(age_group_id, location_id, year_id, sex_id, daly_rate=val)]

dt <- merge(age_structure[,.(age_group_id, location_id, year_id, sex_id, age_structure)],
            population[,.(age_group_id, location_id, year_id, sex_id, population)], 
            by=c("age_group_id", "location_id", "year_id", "sex_id"))
dt <- merge(dt, pop_age, by=c("age_group_id", "location_id", "year_id", "sex_id"))
dt <- merge(dt, daly_num, by=c("age_group_id", "location_id", "year_id", "sex_id"))
dt <- merge(dt, daly_rate, by=c("age_group_id", "location_id", "year_id", "sex_id"))

dt <- dt[!is.na(daly_num)] # dropping NA cases (remove age groups below modeling threshold)
dt <- dt[daly_num!=0] # dropping NA cases (remove age groups below modeling threshold)
dt <- dt[, year_id := ifelse(year_id == 1990, 1, 2)]
dt <- dt[, daly_rate := daly_num/population_age] 

dt <- dcast(dt, location_id + age_group_id + sex_id ~ year_id, value.var = c("population", "age_structure", "daly_num", "daly_rate"))
```

```{r}
# three factor decomp (population, age structure, DALYS)

dt <- dt[, population_effect := ((age_structure_1 * daly_rate_1 + age_structure_2 * daly_rate_2) / 3 +
                                     (age_structure_1 * daly_rate_2 + age_structure_2 * daly_rate_1) / 6) * (population_2 - population_1)]

dt <- dt[, age_structure_effect := ((population_1 * daly_rate_1 + population_2 * daly_rate_2) / 3 +
                                        (population_1 * daly_rate_2 + population_2 * daly_rate_1) / 6) * (age_structure_2 - age_structure_1)]

dt <- dt[, cause_effect := ((population_1 * age_structure_1 + population_2 * age_structure_2) / 3 +
                                (population_1 * age_structure_2 + population_2 * age_structure_1) / 6) * (daly_rate_2 - daly_rate_1)]

dt_sum <- dt[, lapply(.SD, sum), by = "location_id",
               .SDcols = c("daly_num_1", "daly_num_2", "population_effect", "age_structure_effect", "cause_effect")]


dt_sum <- dt_sum[, total_pct := (daly_num_2 - daly_num_1) / daly_num_1]
dt_sum <- dt_sum[, total_pct1 := (population_effect+age_structure_effect+cause_effect) / daly_num_1] #MAKE SURE THESE ARE THE SAME
dt_sum$total_pct1 <- NULL

dt_sum <- dt_sum[, c("population_effect", "cause_effect", "age_structure_effect") :=
                     .(population_effect / daly_num_1,
                       cause_effect / daly_num_1,
                       age_structure_effect / daly_num_1)]
dt_sum <- dt_sum[, c("daly_num_1", "daly_num_2") := .(NULL, NULL)]

dt_sum <- melt(dt_sum,
                 id.vars = c("location_id"),
                 value.vars = c(grep("effect", names(dt_sum), value = TRUE), "total_pct"),
                 variable.name = "factor_format",
                 value.name = "change")
total_dt <- dt_sum[factor_format=="total_pct",list(location_id,change)]
setnames(total_dt,"change","total")
dt_sum <- merge(dt_sum[factor_format!="total_pct",],total_dt,by=c("location_id"))

dt_sum <- merge(dt_sum, locs_glb_regions[, .(location_id, lancet_label, sort_order)])
dt_sum <- dt_sum[order(-sort_order)]
dt_sum[, lancet_label := factor(lancet_label, levels = unique(lancet_label))]
dt_sum[, factor_format := factor(factor_format,
                                   level = c("age_structure_effect", "population_effect", "cause_effect"),
                                   label = c("Change due to population ageing",
                                             "Change due to population growth",
                                             "Change due to risk-deleted DALY rate"))]
fwrite(dt_sum, paste0("[filepath]", "_Figure3.csv"))

```

```{r}
#plot

  colors <- c("Change due to population ageing" = "#A2C851",
              "Change due to population growth" = "#218380",
              "Change due to risk-deleted DALY rate" = "dodgerblue2")
  
  plot <- ggplot() +
    geom_point(data=dt_sum,aes(x=lancet_label,y=total),
               size=-1,na.rm=T,color="white")+ 
    geom_bar(data = dt_sum[change < 0,],
             aes(x = lancet_label, y = change*100, fill = factor_format), stat = "identity", width = .75, na.rm = TRUE) +
    geom_bar(data = dt_sum[change >= 0,],
             aes(x = lancet_label, y = change*100, fill = factor_format), stat = "identity", width = .75, na.rm = TRUE) +
    geom_point(data = dt_sum,
               aes(x = lancet_label, y = total*100, color = "Total percent change"),
               size = 3, na.rm = TRUE) +
    geom_hline(aes(yintercept = 0), colour = "black", linetype = "dashed") +
    scale_fill_manual(values = colors, drop = FALSE) +
    scale_color_manual(values = c("Total percent change" = "black")) +
    scale_y_continuous(limits = c(-50, 250),
                       breaks = seq(-50, 250, by = 50),
                       expand = c(0, 0)) +
    coord_flip() +
    labs(y = "Percent Change (%)", x = "", fill = "") +
           theme_bw() +
    theme(plot.title = element_text(vjust = 2, hjust = 0, size = 15),
          axis.text.x = element_text(size=7),
          legend.key.size = unit(0.5, "cm"),
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.box = "horizontal") +
    guides(fill = guide_legend(nrow = 2, reverse = FALSE))
  pdf(file = paste0("[filepath]", "_Figure3.pdf"), height = 8, width = 10, pointsize = 20)
  print(plot)
dev.off()
```
