# Tables & Figures for Manuscript
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 03/30/2023

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
# In this script we 

# table 1: cols = pop, prepause traf, postpause traf, perc white, perc black, median hhincome
#          rows = 5 strata for ICE and 5 strata for EJI
# fig 1: traffic pre and post pause by census tract (2-panel map)
# fig 2: 2-panel chloropleth w ICE and EJI
# fig 3: time series of traffic
# fig 4: forrest plot of strata model results
# sup table 1: coefs and cis for fig 3
# fig 5 (if different from total, if not, goes in supp): forrest plot of eji modules
# sup table 2: coefs and cis for fig 4


####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages & passwords
source(paste0(project.folder, 'packages.R'))

# 0c Set up filepath(s)
data_path <- paste0(project.folder, 'data/processed_data/')
model_path <- paste0(project.folder, 'outputs/models/')

# 0d Load data
fullData <- read_rds(paste0(data_path, 'full_dataset_wcovars_daily.rds'))


####***********************************
#### 1: Prepare Manuscript Table 1 #### 
####***********************************

# 1a Filter dataset to exclude Phase 1 reopening and beyond
#    Note: NYC entered Phase 1 reopening on June 8, 2020
fullDataF <- fullData %>% filter(date < '2020-06-08')

# 1b Nest by strata
# 1b.i ICE HH Income and BW Race, 5 quantiles
fullDataS <- fullDataF %>% group_by(ice_hhincome_bw_5) %>% nest() %>% 
  rename(strata = ice_hhincome_bw_5) %>% 
  mutate(strata = case_when(
    strata == 'Q1' ~ 'iceHhincomeBwQ1', strata == 'Q2' ~ 'iceHhincomeBwQ2',
    strata == 'Q3' ~ 'iceHhincomeBwQ3', strata == 'Q4' ~ 'iceHhincomeBwQ4',
    strata == 'Q5' ~ 'iceHhincomeBwQ5'))
# 1b.ii EJI, 5 quantiles
fullDataS.2 <- fullDataF %>% group_by(eji_5) %>% nest() %>% 
  rename(strata = eji_5) %>% 
  mutate(strata = case_when(
    strata == 'Q1' ~ 'ejiQ1', strata == 'Q2' ~ 'ejiQ2',
    strata == 'Q3' ~ 'ejiQ3', strata == 'Q4' ~ 'ejiQ4',
    strata == 'Q5' ~ 'ejiQ5'))

# 1c Bind together
fullDataS <- fullDataS %>% 
  bind_rows(fullDataS.2) %>% 
  filter(!is.na(strata))

# 1d Set strata to factor with assigned levels
fullDataS <- fullDataS %>% 
  mutate(strata = factor(strata, levels = c('iceHhincomeBwQ1', 'iceHhincomeBwQ2',
                                            'iceHhincomeBwQ3', 'iceHhincomeBwQ4',
                                             'iceHhincomeBwQ5', 'ejiQ1', 'ejiQ2',
                                             'ejiQ3', 'ejiQ4', 'ejiQ5'))) %>% 
  arrange(strata)

# 1d Clean environment
rm(fullData, fullDataF, fullDataS.2)

# 1e Calculate column values for mean and SD (or median and IQR) of census vars
#    population, perc white, perc black
t1_census <- fullDataS %>% 
  mutate(num_CTs = map(.x = data, ~ n_distinct(.x$poly_id)),
         mean_pop = map(.x = data, ~ mean(.x$all_race_est, na.rm = T)),
         sd_pop = map(.x = data, ~ sd(.x$all_race_est, na.rm = T)),
         mean_pwhite = map(.x = data, ~ (mean(.x$nhwhite_race_est/.x$all_race_est, na.rm = T)*100)),
         sd_pwhite = map(.x = data, ~ (sd(.x$nhwhite_race_est/.x$all_race_est, na.rm = T)*100)),
         mean_pblack = map(.x = data, ~ (mean(.x$nhblack_race_est/.x$all_race_est, na.rm = T)*100)),
         sd_pblack = map(.x = data, ~ (sd(.x$nhblack_race_est/.x$all_race_est, na.rm = T)*100))) %>% 
  dplyr::select(-data) %>% 
  mutate(across(where(is.list), ~ as.numeric(.x)),
         across(where(is.numeric), ~ round(.x, digits = 1))) %>% 
  arrange(strata)

# 1f Calculate column values for traffic pre and post pause
# 1f.i Make function to calculate mean and sd traf values pre and post pause
aggTrafByPause = function(df){
  df <- df %>% group_by(pause) %>% 
    summarise(mean_propGreen = round(mean(prop_green, na.rm = T), digits = 1),
              sd_propGreen = round(sd(prop_green, na.rm = T), digits = 1))
}
# 1f.ii Run function and convert df to table-like format
t1_traf <- fullDataS %>% 
  mutate(agg_traf = map(.x = data, ~ aggTrafByPause(.x))) %>% 
  dplyr::select(-data) %>% unnest(agg_traf) %>% 
  pivot_wider(names_from = pause,
              values_from = c(mean_propGreen, sd_propGreen)) %>% 
  janitor::clean_names() %>% 
  dplyr::select(strata, mean_prop_green_pre_pause, sd_prop_green_pre_pause,
                mean_prop_green_pause, sd_prop_green_pause) %>% 
  arrange(strata)


####*********************************
#### 2: Prepare Manuscript Fig 1 #### 
####*********************************

# Two-panel chloropleth map showing traffic on a day before and a day after
# implementation of NY on Pause

# 2a 





