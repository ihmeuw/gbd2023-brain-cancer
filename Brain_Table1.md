---
title: "Brain_Table1"
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
compare_version = 8352
dalynator_compare_version = 8352
como_compare_version = 8352
codcorrect_compare_version =  8352

###############################################################
# SET GLOBALS that may need modifications
###############################################################

cause_name <- "Brain"
plot_cause <- 477	# Brain
brain <- 477

both_sex <- 2
plot_sex <- both_sex 

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
year <- year_late
year_early <- 1990
year_late <- 2023
release <- 16 

# all ages
all_age_id <- 22 #all ages 
age_stnd_age_id <- 27 #age-standardized 
ages <- get_age_metadata(age_group_set_id = 24, release_id = release)
age_ids <- unique(ages$age_group_id)


# locations
global <- 1
locs <- get_location_metadata(location_set_id = 35, release_id = release)
wbi_locs <- get_location_metadata(location_set_id=26, release_id=release)[level==1]
gbd_locs <- locs[level<=3]

# measures
cases <- 6
deaths <- 1
dalys <- 2
# metrics
number <- 1
rate <- 3
percent <- 2
```

``` r
# pull data

# mortality, deaths

mor_gbd <- make_aggregates(entity = "cause",
                                  location_id=c(gbd_locs$location_id),
                                  location_set_id=35,
                                  year_id=year_late,
                                  age_group_id = all_age_id,  # All ages
                                  age_group_child_ids = c(age_ids),
                                  release_id=release,
                                  cause_id = plot_cause,
                                  metric_id = number,
                                  measure_id=deaths,
                                  sex_id = plot_sex,                              
                                  best_population_fallback = TRUE,
                                  compare_version_id=codcorrect_compare_version,                                      estimates=c('UI', 'PE'))
mor_wbig <- make_aggregates(entity = "cause",
                                   location_id=c(wbi_locs$location_id),
                                   location_set_id=26,
                                   year_id=year_late,
                                   age_group_id = all_age_id,  # All ages
                                   age_group_child_ids = c(age_ids),
                                   release_id=release,
                                   cause_id = plot_cause,
                                   metric_id = number,
                                   measure_id=deaths,
                                   sex_id = plot_sex,                              
                                   best_population_fallback = TRUE,
                                   compare_version_id=codcorrect_compare_version,                                      estimates=c('UI', 'PE'))

mor_all <- setDT(rbind(mor_gbd, mor_wbig))

# mortality, ASMR

asmr_gbd <- make_aggregates(entity = "cause",
                           location_id=c(gbd_locs$location_id),
                           location_set_id=35,
                           year_id=year_late,
                           age_group_id=age_stnd_age_id,
                           age_standardized_child_ids = c(age_ids),
                           release_id=release,
                           cause_id = plot_cause,
                           metric_id = rate,
                           measure_id=deaths,
                           sex_id = plot_sex,
                           sex_child_ids = c(1, 2),
                           best_population_fallback = TRUE,
                           compare_version_id=codcorrect_compare_version,          
                           estimates=c('UI', 'PE'))
asmr_wbig <- make_aggregates(entity = "cause",
                            location_id=c(wbi_locs$location_id),
                            location_set_id=26,
                            year_id=year_late,
                            age_group_id=age_stnd_age_id,
                            age_standardized_child_ids = c(age_ids),
                            release_id=release,
                            cause_id = plot_cause,
                            metric_id = rate,
                            measure_id=deaths,
                            sex_id = plot_sex,
                            sex_child_ids = c(1, 2),                             
                            best_population_fallback = TRUE,
                            compare_version_id=codcorrect_compare_version,
                            estimates=c('UI', 'PE'))
asmr_all <- setDT(rbind(asmr_gbd, asmr_wbig))


# incidence, cases


cases_gbd <- make_aggregates(entity = "cause",
                           location_id=c(gbd_locs$location_id),
                           location_set_id=35,
                           year_id=year_late,
                           age_group_id = all_age_id,  # All ages
                           age_group_child_ids = c(age_ids),
                           release_id=release,
                           cause_id = plot_cause,
                           metric_id = number,
                           measure_id=cases,
                           sex_id = plot_sex,
                           sex_child_ids = c(1, 2),                               
                           best_population_fallback = TRUE,
                           compare_version_id=como_compare_version,                              
                           estimates=c('UI', 'PE'))
cases_wbig <- make_aggregates(entity = "cause",
                            location_id=c(wbi_locs$location_id),
                            location_set_id=26,
                            year_id=year_late,
                            age_group_id = all_age_id,  # All ages
                            age_group_child_ids = c(age_ids),
                            release_id=release,
                            cause_id = plot_cause,
                            metric_id = number,
                            measure_id=cases,
                            sex_id = plot_sex,
                            sex_child_ids = c(1, 2),                              
                            best_population_fallback = TRUE,
                            compare_version_id=como_compare_version,                
                            estimates=c('UI', 'PE'))
cases_all <- setDT(rbind(cases_gbd, cases_wbig))

# incidence, ASIR

asir_gbd <- make_aggregates(entity = "cause",
                             location_id=c(gbd_locs$location_id),
                             location_set_id=35,
                             year_id=year_late,
                             age_group_id=age_stnd_age_id,
                             age_standardized_child_ids = c(age_ids),
                             release_id=release,
                             cause_id = plot_cause,
                             metric_id = rate,
                             measure_id=cases,
                             sex_id = plot_sex,
                             sex_child_ids = c(1, 2),                               
                             best_population_fallback = TRUE,
                             compare_version_id=como_compare_version,               
                            estimates=c('UI', 'PE'))
asir_wbig <- make_aggregates(entity = "cause",
                              location_id=c(wbi_locs$location_id),
                              location_set_id=26,
                              year_id=year_late,
                              age_group_id=age_stnd_age_id,
                              age_standardized_child_ids = c(age_ids),
                              release_id=release,
                              cause_id = plot_cause,
                              metric_id = rate,
                              measure_id=cases,
                              sex_id = plot_sex,
                              sex_child_ids = c(1, 2),                                                            best_population_fallback = TRUE,
                              compare_version_id=como_compare_version,
                             estimates=c('UI', 'PE'))
asir_all <- setDT(rbind(asir_gbd, asir_wbig))

# mortality, deaths, percent change (draws)

mor_gbd_draws <- make_aggregates(entity = "cause",
                           location_id=c(gbd_locs$location_id),
                           location_set_id=35,
                           year_id=c(year_early,year_late),
                           age_group_id = all_age_id,  # All ages
                           age_group_child_ids = c(age_ids),
                           release_id=release,
                           cause_id = plot_cause,
                           metric_id = number,
                           measure_id=deaths,
                           sex_id = plot_sex,
                           sex_child_ids = c(1, 2),                              
                           best_population_fallback = TRUE,
                           compare_version_id=codcorrect_compare_version, 
                           estimates='draws')
mor_wbig_draws <- make_aggregates(entity = "cause",
                            location_id=c(wbi_locs$location_id),
                            location_set_id=26,
                            year_id=c(year_early,year_late),
                            age_group_id = all_age_id,  # All ages
                            age_group_child_ids = c(age_ids),
                            release_id=release,
                            cause_id = plot_cause,
                            metric_id = number,
                            measure_id=deaths,
                            sex_id = plot_sex,
                            sex_child_ids = c(1, 2),                               
                            best_population_fallback = TRUE,
                            compare_version_id=codcorrect_compare_version, 
                            estimates='draws')
mor_draws_all <- setDT(rbind(mor_gbd_draws, mor_wbig_draws))

# mortality, ASMR, percent change (draws)


asmr_gbd_draws <- make_aggregates(entity = "cause",
                            location_id=c(gbd_locs$location_id),
                            location_set_id=35,
                            year_id=c(year_early,year_late),
                            age_group_id=age_stnd_age_id,
                            age_standardized_child_ids = c(age_ids),
                            release_id=release,
                            cause_id = plot_cause,
                            metric_id = rate,
                            measure_id=deaths,
                            sex_id = plot_sex,
                            sex_child_ids = c(1, 2),                               
                            best_population_fallback = TRUE,
                            compare_version_id=codcorrect_compare_version, 
                            estimates='draws')
asmr_wbig_draws <- make_aggregates(entity = "cause",
                             location_id=c(wbi_locs$location_id),
                             location_set_id=26,
                             year_id=c(year_early,year_late),
                             age_group_id=age_stnd_age_id,
                             age_standardized_child_ids = c(age_ids),
                             release_id=release,
                             cause_id = plot_cause,
                             metric_id = rate,
                             measure_id=deaths,
                             sex_id = plot_sex,
                             sex_child_ids = c(1, 2),                              
                             best_population_fallback = TRUE,
                             compare_version_id=codcorrect_compare_version, 
                             estimates='draws')
asmr_draws_all <- setDT(rbind(asmr_gbd_draws, asmr_wbig_draws))


# incidence, cases, percent change (draws)


cases_gbd_draws <- make_aggregates(entity = "cause",
                             location_id=c(gbd_locs$location_id),
                             location_set_id=35,
                             year_id=c(year_early,year_late),
                             age_group_id = all_age_id,  # All ages
                             age_group_child_ids = c(age_ids),
                             release_id=release,
                             cause_id = plot_cause,
                             metric_id = number,
                             measure_id=cases,
                             sex_id = plot_sex,
                             sex_child_ids = c(1, 2),                               
                             best_population_fallback = TRUE,
                             compare_version_id=como_compare_version, 
                             estimates='draws')
cases_wbig_draws <- make_aggregates(entity = "cause",
                              location_id=c(wbi_locs$location_id),
                              location_set_id=26,
                              year_id=c(year_early,year_late),
                              age_group_id = all_age_id,  # All ages
                              age_group_child_ids = c(age_ids),
                              release_id=release,
                              cause_id = plot_cause,
                              metric_id = number,
                              measure_id=cases,
                              sex_id = plot_sex,
                              sex_child_ids = c(1, 2),                                                            best_population_fallback = TRUE,
                              compare_version_id=como_compare_version, 
                              estimates='draws')
cases_draws_all <- setDT(rbind(cases_gbd_draws, cases_wbig_draws))


# incidence, ASIR, percent change (draws)


asir_gbd_draws <- make_aggregates(entity = "cause",
                            location_id=c(gbd_locs$location_id),
                            location_set_id=35,
                            year_id=c(year_early,year_late),
                            age_group_id=age_stnd_age_id,
                            age_standardized_child_ids = c(age_ids),
                            release_id=release,
                            cause_id = plot_cause,
                            metric_id = rate,
                            measure_id=cases,
                            sex_id = plot_sex,
                            sex_child_ids = c(1, 2),                               
                            best_population_fallback = TRUE,
                            compare_version_id=como_compare_version, 
                            estimates='draws')
asir_wbig_draws <- make_aggregates(entity = "cause",
                             location_id=c(wbi_locs$location_id),
                             location_set_id=26,
                             year_id=c(year_early,year_late),
                             age_group_id=age_stnd_age_id,
                             age_standardized_child_ids = c(age_ids),
                             release_id=release,
                             cause_id = plot_cause,
                             metric_id = rate,
                             measure_id=cases,
                             sex_id = plot_sex,
                             sex_child_ids = c(1, 2),                              
                             compare_version_id=como_compare_version, 
                             estimates='draws',
                             best_population_fallback = TRUE)
asir_draws_all <- setDT(rbind(asir_gbd_draws, asir_wbig_draws))
```

``` r
# cleaning up, making percent change from draws 

# mortality, deaths
mor_draws_all_long <- data.table::melt(
  mor_draws_all,
  id.vars = c("location_id", "year_id", "sex_id", "age_group_id", "measure_id", "metric_id", "cause_id"),
  measure.vars = patterns("^draw_"),
  variable.name = "draw",  
  value.name = "value")

mor_draws_all_long_1990 <- mor_draws_all_long[year_id==1990, .(location_id, sex_id, age_group_id, measure_id, metric_id, cause_id, draw, val_1990=value)]
mor_draws_all_long_2023 <- mor_draws_all_long[year_id==2023, .(location_id, sex_id, age_group_id, measure_id, metric_id, cause_id, draw, val_2023=value)]
mor_draws_all_long_combo <- merge(mor_draws_all_long_1990, mor_draws_all_long_2023, by=c("location_id", "sex_id", "age_group_id", "measure_id", "metric_id", "cause_id", "draw"))
mor_draws_all_long_combo <- mor_draws_all_long_combo[, pct_change := 100*((val_2023-val_1990)/val_1990)]
mor_draws_all_long_combo_summary <- mor_draws_all_long_combo[,.(point_estimate = ((mean(val_2023)-mean(val_1990))/mean(val_1990))*100,
                                                                lower = quantile(pct_change, 0.025),
                                                                upper = quantile(pct_change, 0.975)),
                                                             by=c("location_id", "sex_id", "age_group_id", "measure_id", "metric_id", "cause_id")]

# mortality, asmr
asmr_draws_all_long <- data.table::melt(
  asmr_draws_all,
  id.vars = c("location_id", "year_id", "sex_id", "age_group_id", "measure_id", "metric_id", "cause_id"),
  measure.vars = patterns("^draw_"),
  variable.name = "draw",  
  value.name = "value"     
)
asmr_draws_all_long_1990 <- asmr_draws_all_long[year_id==1990, .(location_id, sex_id, age_group_id, measure_id, metric_id, cause_id, draw, val_1990=value)]
asmr_draws_all_long_2023 <- asmr_draws_all_long[year_id==2023, .(location_id, sex_id, age_group_id, measure_id, metric_id, cause_id, draw, val_2023=value)]
asmr_draws_all_long_combo <- merge(asmr_draws_all_long_1990, asmr_draws_all_long_2023, by=c("location_id", "sex_id", "age_group_id", "measure_id", "metric_id", "cause_id", "draw"))
asmr_draws_all_long_combo <- asmr_draws_all_long_combo[, pct_change := 100*((val_2023-val_1990)/val_1990)]
asmr_draws_all_long_combo_summary <- asmr_draws_all_long_combo[,.(point_estimate = ((mean(val_2023)-mean(val_1990))/mean(val_1990))*100,
                                                                lower = quantile(pct_change, 0.025),
                                                                upper = quantile(pct_change, 0.975)),
                                                             by=c("location_id", "sex_id", "age_group_id", "measure_id", "metric_id", "cause_id")]

# incidence, cases
cases_draws_all <- cases_draws_all[, !"population_group_id"]
cases_draws_all_long <- data.table::melt(
  cases_draws_all,
  id.vars = c("location_id", "year_id", "sex_id", "age_group_id", "measure_id", "metric_id", "cause_id"),
  measure.vars = patterns("^draw_"),
  variable.name = "draw",  
  value.name = "value"     
)
cases_draws_all_long_1990 <- cases_draws_all_long[year_id==1990, .(location_id, sex_id, age_group_id, measure_id, metric_id, cause_id, draw, val_1990=value)]
cases_draws_all_long_2023 <- cases_draws_all_long[year_id==2023, .(location_id, sex_id, age_group_id, measure_id, metric_id, cause_id, draw, val_2023=value)]
cases_draws_all_long_combo <- merge(cases_draws_all_long_1990, cases_draws_all_long_2023, by=c("location_id", "sex_id", "age_group_id", "measure_id", "metric_id", "cause_id", "draw"))
cases_draws_all_long_combo <- cases_draws_all_long_combo[, pct_change := 100*((val_2023-val_1990)/val_1990)]
cases_draws_all_long_combo_summary <- cases_draws_all_long_combo[,.(point_estimate = ((mean(val_2023)-mean(val_1990))/mean(val_1990))*100,
                                                                lower = quantile(pct_change, 0.025),
                                                                upper = quantile(pct_change, 0.975)),
                                                             by=c("location_id", "sex_id", "age_group_id", "measure_id", "metric_id", "cause_id")]

# incidence, asir
asir_draws_all <- asir_draws_all[, !"population_group_id"]
asir_draws_all_long <- data.table::melt(
  asir_draws_all,
  id.vars = c("location_id", "year_id", "sex_id", "age_group_id", "measure_id", "metric_id", "cause_id"),
  measure.vars = patterns("^draw_"),
  variable.name = "draw",  
  value.name = "value"     
)
asir_draws_all_long_1990 <- asir_draws_all_long[year_id==1990, .(location_id, sex_id, age_group_id, measure_id, metric_id, cause_id, draw, val_1990=value)]
asir_draws_all_long_2023 <- asir_draws_all_long[year_id==2023, .(location_id, sex_id, age_group_id, measure_id, metric_id, cause_id, draw, val_2023=value)]
asir_draws_all_long_combo <- merge(asir_draws_all_long_1990, asir_draws_all_long_2023, by=c("location_id", "sex_id", "age_group_id", "measure_id", "metric_id", "cause_id", "draw"))
asir_draws_all_long_combo <- asir_draws_all_long_combo[, pct_change := 100*((val_2023-val_1990)/val_1990)]
asir_draws_all_long_combo_summary <- asir_draws_all_long_combo[,.(point_estimate = ((mean(val_2023)-mean(val_1990))/mean(val_1990))*100,
                                                                    lower = quantile(pct_change, 0.025),
                                                                    upper = quantile(pct_change, 0.975)),
                                                                 by=c("location_id", "sex_id", "age_group_id", "measure_id", "metric_id", "cause_id")]
```

``` r
# cleaning up, bind together estimates, round, format

all_values <- rbind(mor_all, asmr_all, cases_all, asir_all, fill=TRUE)

pct_change_all_values <- rbind(mor_draws_all_long_combo_summary, asmr_draws_all_long_combo_summary, cases_draws_all_long_combo_summary, asir_draws_all_long_combo_summary)

# round counts 
all_values_rounded <- all_values[metric_id==1, point_estimate_w_UIs:= paste0(signif(point_estimate/1000, 3) %>% formatC(big.mark=" ", format="fg", digits=3, flag = "#") %>% trimws(which = "left"), 
                         "\n(", signif(lower/1000, 3) %>% formatC(big.mark=" ", format="fg", digits=3, flag = "#") %>% trimws(which = "left"),
                         " to ", signif(upper/1000, 3) %>% formatC(big.mark=" ", format="fg", digits=3, flag = "#") %>% trimws(which = "left"), ")")]
# round rates 
all_values_rounded <- all_values[metric_id==3, point_estimate_w_UIs:= paste0(sprintf("%.1f",round(point_estimate*100000,1)),"\n",
                          "(", sprintf("%.1f",round(as.numeric(lower)*100000,1)),
                          " to ", sprintf("%.1f",round(as.numeric(upper)*100000,1)), ")")]
# round pct change 
pct_change_all_values_rounded <- pct_change_all_values[, point_estimate_w_UIs:= paste0(sprintf("%.1f",round(point_estimate, 1)), "\n",
                         "(", sprintf("%.1f",round(as.numeric(lower), 1)),
                         " to ", sprintf("%.1f",round(as.numeric(upper), 1)), ")")]

full_dt <- rbind(all_values_rounded[,.(location_id, cause_id, metric_id, measure_id, type="2023_val", point_estimate_w_UIs)],
                 pct_change_all_values_rounded[,.(location_id, cause_id, metric_id, measure_id, type="pct_change", point_estimate_w_UIs)])

full_dt <- merge(full_dt, causes[,.(cause_id, lancet_label)], by="cause_id", all.x=T)

wbi_locs <- wbi_locs[location_name=="World Bank High Income", sort_order := 0.1]
wbi_locs <- wbi_locs[location_name=="World Bank Upper Middle Income", sort_order := 0.2]
wbi_locs <- wbi_locs[location_name=="World Bank Lower Middle Income", sort_order := 0.3]
wbi_locs <- wbi_locs[location_name=="World Bank Low Income", sort_order := 0.4]

all_locs <- rbind(locs, wbi_locs)
all_locs <- all_locs[location_name=="Global", sort_order := 0]

full_dt <- merge(full_dt, all_locs[,.(location_id, location_name, sort_order)], by="location_id", all.x=T)

full_dt <- full_dt[measure_id==6 & metric_id==1 & type =="2023_val", col_name :="Incident cases 2023, in thousands (UI)"]
full_dt <- full_dt[measure_id==6 & metric_id==1 & type =="pct_change", col_name :="Incident cases, percent change 1990 to 2023 (UI)"]
full_dt <- full_dt[measure_id==6 & metric_id==3 & type =="2023_val", col_name :="Age-standardised incidence rate 2023, per 100,000 (UI)"]
full_dt <- full_dt[measure_id==6 & metric_id==3 & type =="pct_change", col_name :="Age-standardised incidence rate, percent change 1990 to 2023 (UI)"]
full_dt <- full_dt[measure_id==1 & metric_id==1 & type =="2023_val", col_name :="Deaths 2023, in thousands (UI)"]
full_dt <- full_dt[measure_id==1 & metric_id==1 & type =="pct_change", col_name :="Deaths, percent change 1990 to 2023 (UI)"]
full_dt <- full_dt[measure_id==1 & metric_id==3 & type =="2023_val", col_name :="Age-standardised mortality rate 2023, per 100,000 (UI)"]
full_dt <- full_dt[measure_id==1 & metric_id==3 & type =="pct_change", col_name :="Age-standardised mortality rate, percent change 1990 to 2023 (UI)"]

full_dt_wide <- dcast(full_dt, location_name + sort_order + lancet_label ~ col_name, value.var = "point_estimate_w_UIs")

full_dt_wide <- setDT(setorder(full_dt_wide, sort_order))

full_dt_wide_clean <- full_dt_wide[, .(`Location Name`,
                                       `Incident cases 2023, in thousands (UI)`,
                                 `Incident cases, percent change 1990 to 2023 (UI)`=`% change, cases, 1990 to 2023`,
                                       `Incident cases, percent change 1990 to 2023 (UI)`,
                                       `Age-standardised incidence rate 2023, per 100,000 (UI)`,
                                 `Age-standardised incidence rate, percent change 1990 to 2023 (UI)`=`% change, age-standardised incidence rate, 1990 to 2023`,
                                       `Age-standardised incidence rate, percent change 1990 to 2023 (UI)`,
                                       `Deaths 2023, in thousands (UI)`,
                                 `Deaths, percent change 1990 to 2023 (UI)`=`% change, deaths, 1990 to 2023`,
                                       `Deaths, percent change 1990 to 2023 (UI)`,
                                       `Age-standardised mortality rate 2023, per 100,000 (UI)`,
                                 `Age-standardised mortality rate, percent change 1990 to 2023 (UI)`=`% change, age-standardised death rate, 1990 to 2023`,
                                       `Age-standardised mortality rate, percent change 1990 to 2023 (UI)`)]
```
