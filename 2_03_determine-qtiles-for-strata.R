# Merge traffic, ICE, weather, pollution data
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 11/01/2022

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

# the google traffic data are snapshots in time (reconfirm w Markus after getting full
# hourly dataset). For air pollution data, the time stamp is the sample begin time
# https://aqs.epa.gov/aqsweb/documents/about_aqs_data.html#sample-vs-sample-measurement


####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages & passwords
source(paste0(project.folder, 'packages.R'))
source(paste0(project.folder, 'passwords.R'))

# 0c Set up filepath(s)
data_path <- paste0(project.folder, 'data/')

# 0b Load data
# 0b.i Load traffic data
traf <- read_fst(paste0(data_path, 'google_traffic_jantomar2020.fst')) %>% 
  dplyr::select(poly_id, captured_datetime, everything()) %>% 
  filter(!is.na(poly_id)) %>% 
  mutate(poly_id = as.character(poly_id))
# 0b.ii Load census tract sf file
tracts_sf <- nycgeo::nyc_boundaries(geography = "tract")
# 0b.iii


# 0c Merge traffic data with sf of tract boundaries for visualization
traf_sf <- traf %>% left_join(tracts_sf, by = c('poly_id' = 'geoid'))
traf_sf %>% ggplot() + 
  geom_sf(aes(geometry = geometry, fill = gt_pixcount_tot))


table(traf$captured_datetime)

# first, build a 'case complete' df where all 634 census tracts have every
# possible datetime. Then add the data we do have to this df, so we can 
# accurately determine missingness

