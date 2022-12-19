# Load and Prepare Census Data for Index of Conc at Extremes
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 01/28/2022

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
# In this script we use census data to create three index of concentration at
# the extremes variables, at the census tract level, for NYC: race/ethnicity,
# household income, and combined race/ethnicity and household income.

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
# householders. Therefore, for the combined race/ethnicity and income ICE calculation,
# we use household income for Non-Hispanic White householders as one extreme and 
# household income for Black householders (Hispanic origin not disaggregated) 
# for the other extreme. This paper notes does the same and notes the lack of
# race/ethnicity household income data in the census:
# Krieger N, Feldman JM, Waterman PD, Chen JT, Coull BA, Hemenway D. Local Residential 
# Segregation Matters: Stronger Association of Census Tract Compared to Conventional 
# City-Level Measures with Fatal and Non-Fatal Assaults (Total and Firearm Related), 
# Using the Index of Concentration at the Extremes (ICE) for Racial, Economic, and 
# Racialized Economic Segregation, Massachusetts (US), 1995-2010. J Urban Health. 
# 2017;94(2):244-258.


####********************
#### 0: Preparation #### 
####********************

# 0a Load Packages
packages <- c("tidyverse", "fst")
lapply(packages, library, character.only = TRUE)


####***********************************
#### 1: Load and clean census data #### 
####***********************************

# 1a Load & tidy race data
race <- read_csv("./data/nhgis0005_ds244_20195_tract.csv") %>% 
  filter(GISJOIN != "GIS Join Match Code") %>% 
  dplyr::select(GISJOIN, STATEA, COUNTYA, TRACTA, ALUKE001, ALUKE003, 
                ALUKE004, ALUKM001, ALUKM003, ALUKM004) %>% 
  rename(fips_code = GISJOIN, state_code = STATEA, county_code = COUNTYA,
         tract_code = TRACTA, all_est = ALUKE001, nhwhite_est = ALUKE003,
         nhblack_est = ALUKE004, all_moe = ALUKM001, nhwhite_moe = ALUKM003,
         nhblack_moe = ALUKM004) %>% 
  filter(state_code == "36") %>% 
  filter(county_code == "005" | county_code == "047" | county_code == "061" | 
           county_code == "081" | county_code == "085") %>% 
  mutate(all_est = as.numeric(all_est),
         nhwhite_est = as.numeric(nhwhite_est),
         nhblack_est = as.numeric(nhblack_est))

# 1b Load & tidy US household income percentiles
#    Notes: 20% percentile upper limit =  $25,766
#           80% percentile upper limit = $126,609
us_income_percentiles <- read_csv("./data/nhgis0009_ds245_20195_nation.csv") %>% 
  filter(GISJOIN != "GIS Join Match Code") %>% 
  rename(q20_upper_est = AMEJE001, q40_upper_est = AMEJE002, q60_upper_est = AMEJE003,
         q80_upper_est = AMEJE004, q95_lower_est = AMEJE005, q20_upper_moe = AMEJM001,
         q40_upper_moe = AMEJM002, q60_upper_moe = AMEJM003, q80_upper_moe = AMEJM004,
         q95_lower_moe = AMEJM005) %>% 
  dplyr::select(-GISJOIN, -STUSAB, -NATION, -NATIONA, -AIHHTLI,  -AITS, -MEMI, -UR, 
                -PCI, -NAME_E, -NAME_M)
  
# 1c Load & tidy household income data
#    Notes: Only keeping the income categories closest to the 20th percentile
#           (and below) and closest to the 80th percentile (and above), as these
#           will be used to create the extreme groups for the income ICE variable
#           $24,999 and less; $125,000 and more
hhincome <- read_csv("./data/nhgis0007_ds244_20195_tract.csv") %>% 
  filter(GISJOIN != "GIS Join Match Code") %>% 
  dplyr::select(GISJOIN, STATEA, COUNTYA, TRACTA, ALW0E001:ALW0E005, 
                ALW0E015:ALW0E017, ALW0M001:ALW0M005, ALW0M015:ALW0M017) %>% 
  rename(fips_code = GISJOIN, state_code = STATEA, county_code = COUNTYA,
         tract_code = TRACTA, all_est = ALW0E001, iless10_est = ALW0E002,
         i10000to14999_est = ALW0E003, i15000to19999_est = ALW0E004, 
         i20000to24999_est = ALW0E005, i125000to149999_est = ALW0E015, 
         i150000to199999_est = ALW0E016, i200000plus_est = ALW0E017,
         all_moe = ALW0M001, iless10_moe = ALW0M002,
         i10000to14999_moe = ALW0M003, i15000to19999_moe = ALW0M004, 
         i20000to24999_moe = ALW0M005, i125000to149999_moe = ALW0M015, 
         i150000to199999_moe = ALW0M016, i200000plus_moe = ALW0M017) %>% 
  mutate(across(all_est:i200000plus_moe, ~as.numeric(.x))) %>% 
  filter(state_code == "36") %>% 
  filter(county_code == "005" | county_code == "047" | county_code == "061" | 
           county_code == "081" | county_code == "085") 

# 1c Load & tidy household income for Non-Hispanic White folks
hhincome_nhwhite <- read_csv("./data/nhgis0010_ds245_20195_tract.csv") %>% 
  filter(GISJOIN != "GIS Join Match Code") %>% 
  dplyr::select(GISJOIN, STATEA, COUNTYA, TRACTA, AMDYE001:AMDYE005, 
                AMDYE015:AMDYE017, AMDYM001:AMDYM005, AMDYM015:AMDYM017) %>% 
  rename(fips_code = GISJOIN, state_code = STATEA, county_code = COUNTYA,
         tract_code = TRACTA, all_est_w = AMDYE001, iless10_est_w = AMDYE002,
         i10000to14999_est_w = AMDYE003, i15000to19999_est_w = AMDYE004, 
         i20000to24999_est_w = AMDYE005, i125000to149999_est_w = AMDYE015, 
         i150000to199999_est_w = AMDYE016, i200000plus_est_w = AMDYE017,
         all_moe_w = AMDYM001, iless10_moe_w = AMDYM002,
         i10000to14999_moe_w = AMDYM003, i15000to19999_moe_w = AMDYM004, 
         i20000to24999_moe_w = AMDYM005, i125000to149999_moe_w = AMDYM015, 
         i150000to199999_moe_w = AMDYM016, i200000plus_moe_w = AMDYM017) %>% 
  mutate(across(all_est_w:i200000plus_moe_w, ~as.numeric(.x))) %>% 
  filter(state_code == "36") %>% 
  filter(county_code == "005" | county_code == "047" | county_code == "061" | 
           county_code == "081" | county_code == "085") 

# 1d Load & tidy household income for Black folks
hhincome_black <- read_csv("./data/nhgis0011_ds245_20195_tract.csv") %>% 
  filter(GISJOIN != "GIS Join Match Code") %>% 
  dplyr::select(GISJOIN, STATEA, COUNTYA, TRACTA, AMDSE001:AMDSE005, 
                AMDSE015:AMDSE017, AMDSM001:AMDSM005, AMDSM015:AMDSM017) %>% 
  rename(fips_code = GISJOIN, state_code = STATEA, county_code = COUNTYA,
         tract_code = TRACTA, all_est_b = AMDSE001, iless10_est_b = AMDSE002,
         i10000to14999_est_b = AMDSE003, i15000to19999_est_b = AMDSE004, 
         i20000to24999_est_b = AMDSE005, i125000to149999_est_b = AMDSE015, 
         i150000to199999_est_b = AMDSE016, i200000plus_est_b = AMDSE017,
         all_moe_b = AMDSM001, iless10_moe_b = AMDSM002,
         i10000to14999_moe_b = AMDSM003, i15000to19999_moe_b = AMDSM004, 
         i20000to24999_moe_b = AMDSM005, i125000to149999_moe_b = AMDSM015, 
         i150000to199999_moe_b = AMDSM016, i200000plus_moe_b = AMDSM017) %>% 
  mutate(across(all_est_b:i200000plus_moe_b, ~as.numeric(.x))) %>% 
  filter(state_code == "36") %>% 
  filter(county_code == "005" | county_code == "047" | county_code == "061" | 
           county_code == "081" | county_code == "085") %>% 
  dplyr::select(-state_code, -county_code, -tract_code)


####*****************************
#### 2: Create ICE variables #### 
####*****************************

# 2a Create ICE variable for race/ethnicity
#    Notes: We set as the extreme groups persons who self-identified as 
#           non-Hispanic White versus non-Hispanic Black
ice_race <- race %>%  
  mutate(ice_race = round((nhwhite_est - nhblack_est)/all_est, digits = 2)) %>% 
  dplyr::select(fips_code, state_code, county_code, tract_code, ice_race)
  
# 2b Create ICE variable for income
#    Notes: We set as the extreme groups the income categories closest to the
#           national 20th and 80% categories, here corresponding to 24,999 and less
#           and 125000 and more
#           First we sum the categories above and below the cutpoints
ice_hhincome <- hhincome %>% 
  mutate(lower_extreme = iless10_est + i10000to14999_est + i15000to19999_est +
           i20000to24999_est,
         higher_extreme = i125000to149999_est + i150000to199999_est + i200000plus_est,
         ice_hhincome = round((higher_extreme - lower_extreme)/all_est, digits = 2)) %>% 
  dplyr::select(fips_code, ice_hhincome)

# 2c Create ICE variable for income and race/ethnicity combined
#    Notes: We set as the extreme groups Non-Hispanic White folks in the 80th 
#           income percentile and Black folks in the 20th income percentile
#           For the denominator we used the total number of households with 
#           income data from the hhincome table (not from either race subtable)
ice_hhincome_race <- hhincome_nhwhite %>% 
  full_join(hhincome_black, by = "fips_code") %>% 
  mutate(lower_extreme = iless10_est_b + i10000to14999_est_b + i15000to19999_est_b +
           i20000to24999_est_b,
         higher_extreme = i125000to149999_est_w + i150000to199999_est_w + i200000plus_est_w,
         total = hhincome$all_est,
         ice_hhincome_race = round((higher_extreme - lower_extreme)/total, digits = 2)) %>% 
  dplyr::select(fips_code, ice_hhincome_race)


####*************************
#### 3: Combine and Save #### 
####*************************

# 3a Combine all ice variables
ice <- ice_race %>% 
  full_join(ice_hhincome, by = "fips_code") %>% 
  full_join(ice_hhincome_race, by = "fips_code")

# 3b Save out data
write_fst(ice, path = "./data/ice_census_vars.fst")






