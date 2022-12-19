# Google Traffic ITS Models
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 02/05/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: 


####**************
#### N: Notes ####
####**************

# Na Description

# Resource for adding autocorrelation structure to mixed model with nlme::lme
# http://bbolker.github.io/mixedmodels-misc/ecostats_chap.html
# short summary from stack exchange, which linked to B Bolker's github:
# https://stackoverflow.com/questions/49796175/how-to-check-and-control-for-autocorrelation-in-a-mixed-effect-model-of-longitud


####********************
#### 0: Preparation #### 
####********************

# 0a Load Packages
packages <- c("tidyverse", "fst", "lubridate", "nycgeo", "sf", "scales")
lapply(packages, library, character.only = TRUE)

# 0b Load data
ice_gt <- fst::read_fst(here::here('data', 'ice_gt_jantomar2020.fst'))
ice_census <- fst::read_fst(here::here('data', 'ice_census_vars.fst')) %>% 
  mutate(poly_id = paste0(state_code, county_code, tract_code))


####**************************************
#### 1: Prepare Data for ITS Analysis #### 
####**************************************

# 1a Join census and gt data
formods <- ice_gt %>% left_join(ice_census, by = "poly_id")

# 1b Create ITS variables
formods <- formods %>% 
  mutate(intervention = as.logical(case_when(
           captured_datetime < lubridate::ymd_hms("2020-03-22 0:00:00") ~ 0, 
           captured_datetime >= lubridate::ymd_hms("2020-03-22 0:00:00") ~ 1)),
         time_of_day = factor(lubridate::hour(captured_datetime)),
         day_of_week = factor(lubridate::wday(captured_datetime, label = FALSE)),
         weekend = factor(ifelse(day_of_week == 6 | day_of_week == 7, 
                                 "weekend", "weekday")))
  
# 1c Create time_elapsed variable such that each possible hour is included
#    Note: There is 24*(31+31_29)=2184 hours in Jan-Mar 2020
#          However, since the data is currently processed at 3-hour intervals
#          we are missing March 31, 22:30 and 23:30, for an n=2182
# 1c.i Create df of all possible hours for Jan-Mar 2020
ice_gt %>% dplyr::select(captured_datetime) %>% 
  distinct(captured_datetime) -> time_elapsed
# 1c.ii Create time_elapsed variable
time_elapsed <- time_elapsed %>% mutate(time_elapsed = row_number())
# 1c.iii Add to formods df
formods <- formods %>% left_join(time_elapsed, by = "captured_datetime")

# 1d Restrict to complete cases
formods <- formods %>% 
  dplyr::select(ice_gt, intervention, time_elapsed, weekend, day_of_week, 
                time_of_day, poly_id, captured_datetime, ice_race, ice_hhincome, 
                ice_hhincome_race) %>% na.omit()

# 1e Calculate mean and sd GT ICE value
mean(formods$ice_gt, na.rm = T) # 0.4314912
sd(formods$ice_gt, na.rm = T)   # 0.1751122


####***********************
#### 2: Run ITS Models #### 
####***********************

# 2a Overall model
mod_pause <- nlme::lme(ice_gt ~ intervention + time_elapsed + 
                         weekend + time_of_day, random = ~1|poly_id,
                       data = formods, method = "ML")

# 2b Interaction w ice_race
mod_pause_race <- nlme::lme(ice_gt ~ intervention*ice_race + time_elapsed + 
                              weekend + time_of_day, random = ~1|poly_id,
                            data = formods, method = "ML",
                            control = nlme::lmeControl(opt = "optim"))

# 2c Interaction w ice_hhincome
mod_pause_hhincome <- nlme::lme(ice_gt ~ intervention*ice_hhincome + time_elapsed + 
                                  weekend + time_of_day, random = ~1|poly_id,
                                data = formods, method = "ML",
                                control = nlme::lmeControl(opt = "optim"))

# 2d Interaction w ice_hhincome_race
mod_pause_comb <- nlme::lme(ice_gt ~ intervention*ice_hhincome_race + time_elapsed + 
                              weekend + time_of_day, random = ~1|poly_id,
                            data = formods, method = "ML",
                            control = nlme::lmeControl(opt = "optim"))

# 2e Stratified Models
# 2e.i Calculate 20th and 80th quantiles
q_race = quantile(formods$ice_race, probs = c(0.2, 0.8))
q_income = quantile(formods$ice_hhincome, probs = c(0.2, 0.8))
q_comb = quantile(formods$ice_hhincome_race, probs = c(0.2, 0.8))
# 2e.ii Prepare nested dataset
formods_strat <- formods %>% 
  mutate(race_low = ifelse(ice_race <= q_race[1], ice_race, NA),
         race_high = ifelse(ice_race >= q_race[2], ice_race, NA),
         income_low = ifelse(ice_hhincome <= q_income[1], ice_hhincome, NA),
         income_high = ifelse(ice_hhincome >= q_income[2], ice_hhincome, NA),
         comb_low = ifelse(ice_hhincome_race <= q_comb[1], ice_hhincome_race, NA),
         comb_high = ifelse(ice_hhincome_race >= q_comb[2], ice_hhincome_race, NA)) %>% 
  dplyr::select(-ice_race, -ice_hhincome, -ice_hhincome_race) %>% 
  pivot_longer(race_low:comb_high, names_to = "ice_strata", 
               values_to = "ice_strata_value") %>% 
  na.omit() %>% 
  group_by(ice_strata) %>% 
  nest()
# 2e.iii Create function for model
strat_lme = function(df){
  nlme::lme(ice_gt ~ intervention + time_elapsed + weekend + time_of_day, 
            random = ~1|poly_id, data = df, method = "ML")
}
# 2e.iv Map models
formods_strat <- formods_strat %>% 
  mutate(mod_pause_strat = map(data, strat_lme))


####*****************************************************
#### 3: Pull Intervention Effects: Main & Stratified #### 
####*****************************************************

# 3a Create empty tibble to hold effect estimates
results_strat <- tibble(strata = NA, beta = NA, se = NA, poly_id = NA)

# 3b Add main model results to tibble
results_strat <- results_strat %>% 
  add_row(strata = "full",
          beta = summary(mod_pause)$tTable[2,1],
          se = summary(mod_pause)$tTable[2,2],
          poly_id = list(unique(formods$poly_id)))

# 3b Run for loop to pull data from each strata
for(i in 1:length(formods_strat$ice_strata)){
  results_strat <- results_strat %>% 
    add_row(strata = formods_strat$ice_strata[[i]],
            beta = summary(formods_strat$mod_pause_strat[[i]])$tTable[2,1],
            se = summary(formods_strat$mod_pause_strat[[i]])$tTable[2,2],
            poly_id = list(unique(formods_strat$data[[i]]$poly_id)))
}

# 3c Calculate 95% CIs
results_strat <- results_strat %>% 
  filter(!is.na(strata)) %>% 
  mutate(lci = beta - 1.96*se, uci = beta + 1.96*se) %>% 
  mutate_at(c("beta", "se", "lci","uci"), ~round(., digits = 4))

# 3d Save intervention betas
saveRDS(results_strat, "outputs/results_stratified_models.rds")


####******************************************************
#### 4: Pull Intervention Effects: Interaction Models #### 
####******************************************************

# 4a Create tibble row for race/ethnicity interaction
results_interaction <- 
  tibble(model = "Race / Ethnicity", 
         beta_intervention = summary(mod_pause_race)$tTable[2,1], 
         beta_interaction = summary(mod_pause_race)$tTable[13,1], 
         var_intervention = vcov(mod_pause_race)[2,2],
         var_interaction = vcov(mod_pause_race)[13,13],
         covar_intervention_interaction = vcov(mod_pause_race)[2,13])

# 4b Create tibble row for hhincome interaction
results_interaction <- results_interaction %>% 
  add_row(model = "Household Income", 
          beta_intervention = summary(mod_pause_hhincome)$tTable[2,1], 
          beta_interaction = summary(mod_pause_hhincome)$tTable[13,1], 
          var_intervention = vcov(mod_pause_hhincome)[2,2],
          var_interaction = vcov(mod_pause_hhincome)[13,13],
          covar_intervention_interaction = vcov(mod_pause_hhincome)[2,13])

# 4c Create tibble row for combined race and hhincome interaction
results_interaction <- results_interaction %>% 
  add_row(model = "Combined Race & Income", 
          beta_intervention = summary(mod_pause_comb)$tTable[2,1], 
          beta_interaction = summary(mod_pause_comb)$tTable[13,1], 
          var_intervention = vcov(mod_pause_comb)[2,2],
          var_interaction = vcov(mod_pause_comb)[13,13],
          covar_intervention_interaction = vcov(mod_pause_comb)[2,13]) %>% 
  mutate(model = factor(model, levels = c("Race / Ethnicity",
                                          "Household Income",
                                          "Combined Race & Income")))

# 4d Save interaction results table
saveRDS(results_interaction, "outputs/results_interaction_models.rds")


# sensitivity analysis ideas:
# if using proportion green pixels in ct do sensitivity analysis of green+gray
#   vs just green

# in discussion of paper
#  be sure to discuss how pixel count accounts for road class



