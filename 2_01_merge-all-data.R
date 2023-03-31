# Merge Traffic, ICE, EJI data
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 03/16/2023

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Merge and Assess Missingness
# 2: Determine Quantiles for ICE and EJI
# 3: Add PAUSE Var and Time Covariates
# 4: Add CT Centroids to Account for Spatial Autocorrelation


####**************
#### N: Notes ####
####**************

# Na Description
# In this script we merge traffic data with EJI and ICE data. We also added 
# needed covariates to assess PAUSE using an ITS design, and variables to account
# for spatial and temporal autocorrelation.


####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages & passwords
source(paste0(project.folder, 'packages.R'))
source(paste0(project.folder, 'passwords.R'))

# 0c Set up filepath(s)
data_path <- paste0(project.folder, 'data/processed_data/')
polygons_of_interest_path = here::here('data', 'nyc_census_tracts', 'nycgeo_census_tracts.shp')

# 0d Load data
# 0b.i Load traffic data
#      Note: 445 census tracts
traf <- read_fst(paste0(data_path, 'gt18-20_2010CTs.fst'))
# 0b.ii Load EJI data
eji <- read_fst(paste0(data_path, 'eji_nyc.fst'))
# 0b.iii Load ICE census data
ice <- read_fst(paste0(data_path, 'ice_census_vars_2010CTs.fst'))
# 0b.iv Load census tract shapefile for mapping
tracts_sf <- st_read(polygons_of_interest_path) %>%  
  dplyr::select(geoid, geometry)


####*************************************
#### 1: Merge and Assess Missingness #### 
####*************************************

# 1a Merge data
traf_eji_ice <- traf %>% 
  left_join(eji, by = c('poly_id' = 'geoid')) 
traf_eji_ice <- traf_eji_ice %>% 
  left_join(ice, by = c('poly_id'))
traf_eji_ice <- traf_eji_ice %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid'))

# 1b Check for missing vars
#    Notes: n = 10,641,453 observations: 
#               ((26301 possible datetimes - 2393 missing datetimes) 
#               * 445 CTs) + 2393 missing datetimes
#           n = 2,393 captured_datetimes are missing images, as assessed in 1_02
#           n = 217,565 datetime-CT obs are missing EJI data, corresponding 
#               to the 8 missing CTs assessed 1_05 and an additional CT missing, 
#               36061029700, Inwood Hill Park
#           n = 193,657 datetime-CTs are missing ICE census data, corresponding
#               to the 8 missing CTs assessed in 1_04
table(traf_eji_ice$no_image, useNA = 'always')
summary(traf_eji_ice)
8*(length(unique(traf_eji_ice$captured_datetime))-2393)+2393 # 8 CTs missing ICE
9*(length(unique(traf_eji_ice$captured_datetime))-2393)+2393 # 9 CTs missing EJI

# 1c Map to confirm everything looks good
# 1c.i Map EJI
traf_eji_ice %>% 
  filter(captured_datetime == '2020-02-08 08:00') %>% 
  ggplot() +
  geom_sf(aes(fill = rpl_eji, geometry = geometry), lwd = 0) +
  scale_fill_viridis_c(name = "EJI") +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
# 1c.ii Map main ICE variable
min(traf_eji_ice$ice_hhincome_bw, na.rm = T)
max(traf_eji_ice$ice_hhincome_bw, na.rm = T)
traf_eji_ice %>% 
  filter(captured_datetime == '2020-02-08 08:00') %>% 
  ggplot() +
  geom_sf(aes(fill = ice_hhincome_bw, geometry = geometry), lwd = 0) +
  scale_fill_viridis_c(name = "", 
                       option = "inferno",
                       breaks = c(-0.38, 0.65),
                       labels = c("Syst. \nDisadvantaged", 
                                  "Syst. \nPriveleged")) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))

# 1d Save full dataset
traf_eji_ice %>% write_rds(file = paste0(data_path, 'full_dataset.rds'))


####***********************************************
#### 2: Aggregate to Daily Temporal Resolution #### 
####***********************************************

# Note: Running the models with hourly resolution is too computationally 
#       expensive for my computer. We will instead run the models at daily 
#       resolution, and check for effect modification by time using two binary
#       variables: rush hour / not rush hour and daytime / nighttime

# 2a Aggregate to daily resolution
#    Note: At the end of this aggregation, we remove 187 observations. These are dates 
#          without poly_ids, which correspond to hours missing images, which 
#          could not be assigned a poly_id in script 1_01
traf_daily <- traf %>% 
  mutate(date = date(captured_datetime),
         no_image = as.numeric(ifelse(no_image == 'no image', 1, 0))) %>% 
  group_by(poly_id, date) %>% 
  summarise(speed_reduct_fact = mean(speed_reduct_fact, na.rm = T),
            green_gray_85 = mean(green_gray_85, na.rm = T),
            gt_pixcount_streets = mean(gt_pixcount_streets, na.rm = T),
            prop_maroon_red = mean(prop_maroon_red, na.rm = T),
            prop_green = mean(prop_green, na.rm = T),
            gt_pixcount_maroon = mean(gt_pixcount_maroon, na.rm = T),
            gt_pixcount_red = mean(gt_pixcount_red, na.rm = T),
            gt_pixcount_orange = mean(gt_pixcount_orange, na.rm = T),
            gt_pixcount_green = mean(gt_pixcount_green, na.rm = T),
            gt_pixcount_gray = mean(gt_pixcount_gray, na.rm = T),
            gt_pixcount_construction = mean(gt_pixcount_construction, na.rm = T),
            gt_pixcount_emergency = mean(gt_pixcount_emergency, na.rm = T),
            gt_pixcount_notsampled = mean(gt_pixcount_notsampled, na.rm = T),
            gt_pixcount_background = mean(gt_pixcount_background, na.rm = T),
            gt_pixcount_tot = mean(gt_pixcount_tot, na.rm = T),
            no_image = sum(no_image),
            gt_pixcount_notstreets = mean(gt_pixcount_notstreets, na.rm = T),
            ice_gt = mean(ice_gt, na.rm = T))

# 2b Explore missing dates & remove observations with an NA poly_id if
#    the date is missing all 24 hours of traffic data
#    Note: We want to have a case complete dataset where every date is represented,
#          even if the date has no traffic data due to missing images for all 24 hours.
#          Currently, if a date was missing any number of hours, it has an observation 
#          for every CT with the traffic data for that date from non-missing hours,
#          and an additional observation with poly_id = NA with missing traffic data.
#          We should end with n = 454,864 observations: 
#            ((1,096 possible dates - 74 dates missing all 24 hours) 
#            * 445 CTs) + 74 dates missing all 24 hours
table(traf_daily$no_image, useNA = 'always')
traf_daily2 <- traf_daily %>% mutate(remove = ifelse(is.na(poly_id) & no_image < 24, 1, 0)) %>% 
  filter(remove == 0) 

# 2c Merge data
traf_eji_ice_daily <- traf_daily2 %>% 
  left_join(eji, by = c('poly_id' = 'geoid')) 
traf_eji_ice_daily <- traf_eji_ice_daily %>% 
  left_join(ice, by = c('poly_id'))
traf_eji_ice_daily <- traf_eji_ice_daily %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid'))

# 2d Check for missing vars
#    Notes: n = 454,864 observations: 
#               ((1,096 possible dates - 74 dates missing all 24 hours) 
#               * 445 CTs) + 74 dates missing all 24 hours
#           n = 74 dates are missing images for all 24 hours
#           n = 217,565 datetime-CT obs are missing EJI data, corresponding 
#               to the 8 missing CTs assessed 1_05 and an additional CT missing, 
#               36061029700, Inwood Hill Park
#           n = 193,657 datetime-CTs are missing ICE census data, corresponding
#               to the 8 missing CTs assessed in 1_04
table(traf_eji_ice_daily$no_image, useNA = 'always')
summary(traf_eji_ice_daily)
8*(length(unique(traf_eji_ice_daily$date))-74)+74 # 8 CTs missing ICE
9*(length(unique(traf_eji_ice_daily$date))-74)+74 # 9 CTs missing EJI

# 2e Map to confirm everything looks good
# 2e.i Map EJI
traf_eji_ice_daily %>% 
  filter(date == '2020-02-08') %>% 
  ggplot() +
  geom_sf(aes(fill = rpl_eji, geometry = geometry), lwd = 0) +
  scale_fill_viridis_c(name = "EJI") +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
# 2e.ii Map main ICE variable
min(traf_eji_ice_daily$ice_hhincome_bw, na.rm = T)
max(traf_eji_ice_daily$ice_hhincome_bw, na.rm = T)
traf_eji_ice_daily %>% 
  filter(date == '2020-02-08') %>% 
  ggplot() +
  geom_sf(aes(fill = ice_hhincome_bw, geometry = geometry), lwd = 0) +
  scale_fill_viridis_c(name = "", 
                       option = "inferno",
                       breaks = c(-0.38, 0.65),
                       labels = c("Syst. \nDisadvantaged", 
                                  "Syst. \nPriveleged")) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))

# 2f Save full dataset
traf_eji_ice_daily %>% write_rds(file = paste0(data_path, 'full_dataset_daily.rds'))


####********************************************
#### 3: Determine Quantiles for ICE and EJI #### 
####********************************************

# 3a Calculate quantiles
#    Note: N Krieger uses quantiles to stratify by ICE in her NYC example
#    Sub-scales use tertiles because not enough variation of SVM scale for quantiles
#    Quantiles are *very* slightly different if they are determined in the full dataset
#    vs in the original EJI/ICE datasets with unique CT obs. Because the difference
#    is so small, I don't think it matters in which dataset the quantiles are computed.
quants_ice_bw = quantile(traf_eji_ice_daily$ice_hhincome_bw, na.rm = T, probs = c(0, .2, .4, .6, .8, 1))
quants_eji = quantile(traf_eji_ice_daily$rpl_eji, na.rm = T, probs = c(0, .2, .4, .6, .8, 1))
quants_eji_sens = quantile(traf_eji_ice_daily$rpl_eji_sens, na.rm = T, probs = c(0, .2, .4, .6, .8, 1))
quants_eji_ebm = quantile(traf_eji_ice_daily$rpl_ebm, na.rm = T, probs = c(0, .33, .66, 1))
quants_eji_svm = quantile(traf_eji_ice_daily$rpl_svm, na.rm = T, probs = c(0, .33, .66, 1))
quants_eji_hvm = quantile(traf_eji_ice_daily$rpl_hvm, na.rm = T, probs = c(0, .33, .66, 1))

# 3b Assign new stratified variables
traf_eji_ice_daily <- traf_eji_ice_daily %>% 
  mutate(
    ice_hhincome_bw_5 = factor(case_when(
      ice_hhincome_bw < quants_ice_bw[2] ~ 'Q1',
      ice_hhincome_bw >= quants_ice_bw[2] & ice_hhincome_bw < quants_ice_bw[3] ~ 'Q2',
      ice_hhincome_bw >= quants_ice_bw[3] & ice_hhincome_bw < quants_ice_bw[4] ~ 'Q3',
      ice_hhincome_bw >= quants_ice_bw[4] & ice_hhincome_bw < quants_ice_bw[5] ~ 'Q4',
      ice_hhincome_bw >= quants_ice_bw[5] ~ 'Q5')),
    eji_5 = factor(case_when(
      rpl_eji < quants_eji[2] ~ 'Q1',
      rpl_eji >= quants_eji[2] & rpl_eji < quants_eji[3] ~ 'Q2',
      rpl_eji >= quants_eji[3] & rpl_eji < quants_eji[4] ~ 'Q3',
      rpl_eji >= quants_eji[4] & rpl_eji < quants_eji[5] ~ 'Q4',
      rpl_eji >= quants_eji[5] ~ 'Q5')),
    eji_sens_5 = factor(case_when(
      rpl_eji_sens < quants_eji_sens[2] ~ 'Q1',
      rpl_eji_sens >= quants_eji_sens[2] & rpl_eji_sens < quants_eji_sens[3] ~ 'Q2',
      rpl_eji_sens >= quants_eji_sens[3] & rpl_eji_sens < quants_eji_sens[4] ~ 'Q3',
      rpl_eji_sens >= quants_eji_sens[4] & rpl_eji_sens < quants_eji_sens[5] ~ 'Q4',
      rpl_eji_sens >= quants_eji_sens[5] ~ 'Q5')),
    eji_ebm_3 = factor(case_when(
      rpl_ebm < quants_eji_ebm[2] ~ 'Q1',
      rpl_ebm >= quants_eji_ebm[2] & rpl_ebm < quants_eji_ebm[3] ~ 'Q2',
      rpl_ebm >= quants_eji_ebm[3] ~ 'Q3')),
    eji_svm_3 = factor(case_when(
      rpl_svm < quants_eji_svm[2] ~ 'Q1',
      rpl_svm >= quants_eji_svm[2] & rpl_svm < quants_eji_svm[3] ~ 'Q2',
      rpl_svm >= quants_eji_svm[3] ~ 'Q3')),
    eji_hvm_3 = factor(case_when(
      rpl_hvm < quants_eji_hvm[2] ~ 'Q1',
      rpl_hvm >= quants_eji_hvm[2] & rpl_hvm < quants_eji_hvm[3] ~ 'Q2',
      rpl_hvm >= quants_eji_hvm[3] ~ 'Q3')))

# 3c Run tables to confirm
#    missing obs counts match those above
table(traf_eji_ice_daily$ice_hhincome_bw_5, useNA = 'always')
table(traf_eji_ice_daily$eji_5, useNA = 'always')
table(traf_eji_ice_daily$eji_sens_5, useNA = 'always')
table(traf_eji_ice_daily$eji_ebm_3, useNA = 'always')
table(traf_eji_ice_daily$eji_svm_3, useNA = 'always')
table(traf_eji_ice_daily$eji_hvm_3, useNA = 'always')

  
####******************************************
#### 4: Add PAUSE Var and Time Covariates #### 
####******************************************  
  
# 4a Add PAUSE variable and time covariates
traf_eji_ice_daily <- traf_eji_ice_daily %>% 
  mutate(
    pause = factor(ifelse(date > '2020-03-22', 'pause', 'pre-pause'),
                   levels = c('pre-pause', 'pause')),
    dow = factor(wday(date, label = T)),
    month = factor(month(date)),
    year = factor(year(date))
  )

# 4b Review new vars
table(traf_eji_ice_daily$pause, useNA = 'always')
table(traf_eji_ice_daily$dow, useNA = 'always')
table(traf_eji_ice_daily$month, useNA = 'always')
table(traf_eji_ice_daily$year, useNA = 'always')

# 4c Add time_elapsed variable such that each possible day is included
#    Note: There are 1,096 possible days (26,304 possible hours) in 2018-2020, 
#          however we removed 3 duplicate 1am timepoints from Daylight Savings 
#          earlier in the hourly data cleaning, leaving a totl of 26,301 hours
time_elapsed_df <- data.frame(date = seq(ymd('2018-01-01'), ymd('2020-12-31'), by = 'days'))
time_elapsed_df <- time_elapsed_df %>% mutate(time_elapsed = row_number())
traf_eji_ice_daily <- traf_eji_ice_daily %>% 
  left_join(time_elapsed_df, by = 'date')


####****************************************************************
#### 5: Add CT Centroids to Account for Spatial Autocorrelation #### 
####****************************************************************

# 5a Get census tracts centroids
# 5a.i Obtain centroids
cents <- sf::st_centroid(tracts_sf) %>% sf::st_transform('WGS84')
# 5a.ii Convert to dataframe
cents <- data.frame(poly_id = cents$geoid, sf::st_coordinates(cents))
# 5a.iii Rename coordinate vars
colnames(cents)[2:3] <- c('lon', 'lat')
# 5a.iv Plot to confirm
cents %>% ggplot(aes(x = lon, y = lat)) + geom_point()

# 5b Add to traffic + covariate dataset
traf_eji_ice_daily <- traf_eji_ice_daily %>% dplyr::select(-geometry) %>% 
  left_join(cents, by = 'poly_id')

# 5c Save dataset
traf_eji_ice_daily %>% write_rds(file = paste0(data_path, 'full_dataset_wcovars_daily.rds'))



