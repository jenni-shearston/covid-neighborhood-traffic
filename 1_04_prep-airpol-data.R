# Load and Prepare Air Pollution Data
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 11/01/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Load and clean AQS data


####**************
#### N: Notes ####
####**************

# Na Description
# In this script we load NO2 data for the AQS monitor at IS52. 


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


####*************************************************
#### 1: Pull air pollution data from EPA AQS API #### 
####*************************************************

# Instructions for API use: 
# https://aqs.epa.gov/aqsweb/documents/data_api.html#email

# 1a Define time variables
#min(traf$captured_datetime)
#max(traf$captured_datetime)
bdate <- "20200107"
edate <- "20200331"

# 1b Define NO2 vars
#    Note: Pulling NO2 data from the IS52 monitor 
param_no2 <- "42602"
county_no2 <- "005"
site_no2 <- "0110"

# 1c Pull NO2 from IS 52
pull_no2 <- GET(paste0("https://aqs.epa.gov/data/api/sampleData/bySite?email=",aqs_email,"&key=",aqs_key,"&param=",param_no2,"&bdate=",bdate,"&edate=",edate,"&state=36&county=",county_no2,"&site=",site_no2)) %>% 
  content("text") %>%
  jsonlite::fromJSON() %>%
  as_tibble()

# 1d Select only needed variables
no2 <- pull_no2$Data %>%
  dplyr::select(county_code:poc, parameter:units_of_measure, 
                sample_duration, detection_limit, method_code)

# 1e Assign census tract id to air pollution data
#    Note: IS 52 is at 681 Kelly St in the Bronx. This address is in 
#          census tract 83. However, it is 1 block from 85, and 1.5
#          blocks from 79.
#    Source: https://www1.nyc.gov/assets/planning/download/pdf/about/publications/maps/bx-census-tracts-map.pdf
no2 <- no2 %>% 
  mutate(poly_id = '36005008300')

# 1e Save collected data
no2 %>% write_fst(paste0(data_path, 'no2_janToMar2020_is52.fst'))





