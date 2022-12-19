# Load and Prepare Env Just Area Designation Data
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 11/10/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 

####**************
#### N: Notes ####
####**************

# Na Description
# In this script we 

# Source: https://data.cityofnewyork.us/City-Government/Environmental-Justice-Area-Census-Tract-Designatio/ircm-rcjd
# Data dictionary in documentation folder

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
# 0b.i Load ej designation
ej <- read_csv(paste0(data_path, 'Environmental_Justice_Area_Census_Tract_Designation.csv'))
# 0b.ii Load census tracts in traffic data
traf <- read_fst(paste0(data_path, 'google_traffic_jantomar2020.fst')) %>% 
  dplyr::select(poly_id) %>% filter(!is.na(poly_id)) %>% 
  mutate(poly_id = as.character(poly_id)) %>% distinct()


# Build full FIPS code for ej dataset
ej <- ej %>% 
  mutate(poly_id = case_when(
    BoroCode == 1 ~ '36061',
    BoroCode == 2 ~ '36005',
    BoroCode == 3 ~ '36047',
    BoroCode == 4 ~ '36081',
    BoroCode == 5 ~ '36085'
  )) %>% 
  mutate(BoroCT2010 = str_sub(BoroCT2010, start = 2),
         poly_id = paste0(poly_id, BoroCT2010))

# Restrict EJ dataset to census tracts in traffic data
ej <- ej %>% filter(poly_id %in% traf$poly_id)

# Review distribution of EJ designation
ej %>% ggplot(aes(x = EJDesignat)) + geom_histogram(stat = 'count')


