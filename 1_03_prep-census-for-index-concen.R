# Load and Prepare Census Data for Index of Conc at Extremes
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 12/20/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Load and clean census data
# 2: Create ICE variables
# 3: Combine and save


####**************
#### N: Notes ####
####**************

# Na Description
# In this script we use census data to create two index of concentration at
# the extremes variables for a combination of race/ethnicity and household income
# at the census tract level for NYC: (1) NH White vs Black, (2) NH White vs Hispanic

# Nb Index of Concentration at the Extremes Calculation
# ICEi = (Ai-Pi)/Ti
# where, say, in the case of the ICE for income,
# Ai is equal to the number of affluent persons in unit of analysis i 
# (e.g., in the 80th income percentile), 
# Pi is equal to the number of poor persons in unit of analysis i 
# (e.g., in the 20th income percentile), 
# Ti is equal to the total population with known income in unit of analysis i

# Nc Household Income by Race and Ethnicity
# The census does not provide household income data for Non-Hispanic Black
# householders. Therefore, for the combined race/ethnicity and income ICE calculation
# for Black folks, we use household income for Non-Hispanic White householders as  
# one extreme and household income for Black householders (Hispanic origin not 
# disaggregated) as the other extreme. This paper does the same and notes the lack of
# race/ethnicity household income data for NH Black folks in the census:
# Krieger N, Feldman JM, Waterman PD, Chen JT, Coull BA, Hemenway D. Local Residential 
# Segregation Matters: Stronger Association of Census Tract Compared to Conventional 
# City-Level Measures with Fatal and Non-Fatal Assaults (Total and Firearm Related), 
# Using the Index of Concentration at the Extremes (ICE) for Racial, Economic, and 
# Racialized Economic Segregation, Massachusetts (US), 1995-2010. J Urban Health. 
# 2017;94(2):244-258.


####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load Packages
source(paste0(project.folder, 'packages.R'))

# 0c Set up filepath(s)
census_data_path <- paste0(project.folder, 'data/raw_data/census_data/')
processed_data_path <- paste0(project.folder, 'data/processed_data/')

# 0d Load list of census tracts with traffic data
gt_fips <- read_fst(paste0(processed_data_path, 'gt_polyids.fst')) %>% na.omit()


####***********************************
#### 1: Load and clean census data #### 
####***********************************

# 1a Load & tidy race data
race <- read_csv(paste0(census_data_path, "nhgis_race-ethnicity_tract.csv")) %>% 
  filter(GISJOIN != "GIS Join Match Code") %>% 
  dplyr::select(GISJOIN, STATEA, COUNTYA, TRACTA, AOOCE001, AOOCE002, AOOCE003,
                AOOCE004, AOOCE005, AOOCE006, AOOCE007, AOOCE008, AOOCE009,
                AOOCE012, AOOCM001, AOOCM002, AOOCM003, AOOCM004, AOOCM005,
                AOOCM006, AOOCM007, AOOCM008, AOOCM009, AOOCM012) %>% 
  rename(fips_code = GISJOIN, state_code = STATEA, county_code = COUNTYA,
         tract_code = TRACTA, all_est = AOOCE001, nhwhite_est = AOOCE003,
         nhblack_est = AOOCE004, nhai_est = AOOCE005, nhasian_est = AOOCE006,
         nhhawaii_est = AOOCE007, nhother_est = AOOCE008, nhmulti_est = AOOCE009,
         hispanic_est = AOOCE012, nh_est = AOOCE002,
         all_moe = AOOCM001, nhwhite_moe = AOOCM003,
         nhblack_moe = AOOCM004, nhai_moe = AOOCM005, nhasian_moe = AOOCM006,
         nhhawaii_moe = AOOCM007, nhother_moe = AOOCM008, nhmulti_moe = AOOCM009,
         hispanic_moe = AOOCM012, nh_moe = AOOCM002) %>% 
  filter(state_code == "36") %>% 
  filter(county_code == "005" | county_code == "047" | county_code == "061" | 
           county_code == "081" | county_code == "085") %>% 
  mutate_at(vars(matches('_est')), as.numeric) %>% 
  mutate_at(vars(matches('_moe')), as.numeric) %>% 
  mutate(poly_id = paste0(state_code, county_code, tract_code))

# 1b Load & tidy NYC household income percentiles
#    Notes: 20% percentile upper limit = $23,191
#           80% percentile upper limit = $158,999
nyc_income_ptiles <- read_csv(paste0(census_data_path, 
                                     "nhgis_income_place.csv")) %>% 
  filter(GISJOIN != "GIS Join Match Code") %>% 
  dplyr::select(GISJOIN, STATEA, PLACEA, AO7ZE001, AO7ZE002, AO7ZE003, AO7ZE004,
                AO7ZE005, AO7ZM001, AO7ZM002, AO7ZM003, AO7ZM004, AO7ZM005) %>% 
  rename(fips_code = GISJOIN, state_code = STATEA, place_code = PLACEA,
         q20_upper_est = AO7ZE001, q40_upper_est = AO7ZE002, q60_upper_est = AO7ZE003,
         q80_upper_est = AO7ZE004, q95_lower_est = AO7ZE005, q20_upper_moe = AO7ZM001,
         q40_upper_moe = AO7ZM002, q60_upper_moe = AO7ZM003, q80_upper_moe = AO7ZM004,
         q95_lower_moe = AO7ZM005) %>% 
  filter(state_code == "36") %>% filter(place_code == '51000') %>% 
  mutate_at(vars(matches('_est')), as.numeric) %>% 
  mutate_at(vars(matches('_moe')), as.numeric)
  
# 1c Load & tidy household income by race data
#    Notes: Only keeping the income categories closest to the 20th percentile
#           (and below) and closest to the 80th percentile (and above), as these
#           will be used to create the extreme groups
#           $24,999 and less; $150,000 and more
hhincome_race <- read_csv(paste0(census_data_path, "nhgis_race-income_tract.csv")) %>% 
  filter(GISJOIN != "GIS Join Match Code") %>% 
  dplyr::select(GISJOIN, STATEA, COUNTYA, TRACTA, AO68E001:AO68E005, AO68E016,
                AO68E017, AO68M001:AO68M005, AO68M016, AO68M017, AO7EE001:AO7EE005,
                AO7EE016, AO7EE017, AO7EM001:AO7EM005, AO7EM016, AO7EM017) %>% 
  rename(fips_code = GISJOIN, state_code = STATEA, county_code = COUNTYA,
         tract_code = TRACTA, 
         black_all_est = AO68E001, black_iless10_est = AO68E002,
         black_i10000to14999_est = AO68E003, black_i15000to19999_est = AO68E004, 
         black_i20000to24999_est = AO68E005, black_i150000to199999_est = AO68E016, 
         black_i200000plus_est = AO68E017,
         black_all_moe = AO68M001, black_iless10_moe = AO68M002,
         black_i10000to14999_moe = AO68M003, black_i15000to19999_moe = AO68M004, 
         black_i20000to24999_moe = AO68M005, black_i150000to199999_moe = AO68M016, 
         black_i200000plus_moe = AO68M017,
         nhwhite_all_est = AO7EE001, nhwhite_iless10_est = AO7EE002,
         nhwhite_i10000to14999_est = AO7EE003, nhwhite_i15000to19999_est = AO7EE004, 
         nhwhite_i20000to24999_est = AO7EE005, nhwhite_i150000to199999_est = AO7EE016, 
         nhwhite_i200000plus_est = AO7EE017,
         nhwhite_all_moe = AO7EM001, nhwhite_iless10_moe = AO7EM002,
         nhwhite_i10000to14999_moe = AO7EM003, nhwhite_i15000to19999_moe = AO7EM004, 
         nhwhite_i20000to24999_moe = AO7EM005, nhwhite_i150000to199999_moe = AO7EM016, 
         nhwhite_i200000plus_moe = AO7EM017) %>% 
  filter(state_code == "36") %>% 
  filter(county_code == "005" | county_code == "047" | county_code == "061" | 
           county_code == "081" | county_code == "085") %>% 
  mutate_at(vars(matches('_est')), as.numeric) %>% 
  mutate_at(vars(matches('_moe')), as.numeric) 

# 1d Load & tidy total number of households reporting income for denominator of ICE
hhincome <- read_csv(paste0(census_data_path, "nhgis_income_tract.csv")) %>% 
  filter(GISJOIN != "GIS Join Match Code") %>% 
  rename(fips_code = GISJOIN, state_code = STATEA, county_code = COUNTYA,
         tract_code = TRACTA, 
         hhincome_all_est = AOQHE001, hhincome_all_moe = AOQHM001) %>% 
  filter(state_code == "36") %>% 
  filter(county_code == "005" | county_code == "047" | county_code == "061" | 
           county_code == "081" | county_code == "085") %>% 
  mutate_at(vars(matches('_est')), as.numeric) %>% 
  mutate_at(vars(matches('_moe')), as.numeric) %>% 
  dplyr::select(fips_code, hhincome_all_est, hhincome_all_moe) 


####**************************************************************
#### 2: Review race distributions in tracts with traffic data #### 
####**************************************************************



####*****************************
#### 3: Create ICE variables #### 
####*****************************

# 3a Create ICE variable for income and race/ethnicity combined, extreme groups
#    as NH White and Black
#    Notes: We set as the extreme groups Non-Hispanic White folks in the 80th 
#           income percentile and Black folks in the 20th income percentile
#           For the denominator we use the total number of households with 
#           income data from the hhincome table (not from either race subtable)
ice_hhincome_race <- hhincome_race %>% 
  full_join(hhincome, by = 'fips_code') %>% 
  mutate(lower_extreme = black_iless10_est + black_i10000to14999_est + black_i15000to19999_est +
           black_i20000to24999_est,
         higher_extreme = nhwhite_i150000to199999_est + nhwhite_i200000plus_est,
         total = hhincome_all_est,
         ice_hhincome_race = round((higher_extreme - lower_extreme)/total, digits = 2),
         poly_id = paste0(state_code, county_code, tract_code)) %>% 
  dplyr::select(poly_id, ice_hhincome_race)


####*******************************
#### 4: Map and review ICE var #### 
####*******************************



####**********************
#### 5: Save out data #### 
####**********************

# 5a Save out data
write_fst(ice, path = paste0(processed_data_path, 'ice_census_vars.fst')) 






