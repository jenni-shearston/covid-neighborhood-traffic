# Load and Prepare Census Data for Index of Conc at Extremes
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 03/16/2023

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Load and clean census data
# 2: Review race distributions in tracts with traffic data
# 3: Create ICE variables
# 4: Map and review ICE vars
# 5: Save out data

####**************
#### N: Notes ####
####**************

# Na Description
# In this script we use census data to create one index of concentration at
# the extremes variables for a combination of race/ethnicity and household income
# at the census tract level for NYC: (1) NH White & High income vs Black & Low income

# Nb Index of Concentration at the Extremes Calculation
# ICEi = (Ai-Pi)/Ti
# where, say, in the case of the ICE for income,
# Ai is equal to the number of affluent persons in unit of analysis i 
# (e.g., in the 80th income percentile), 
# Pi is equal to the number of poor persons in unit of analysis i 
# (e.g., in the 20th income percentile), 
# Ti is equal to the total population with known income in unit of analysis i

# Nc Household Income by Race and Ethnicity
# The census does not provide household income data for Non-Hispanic Black or Asian
# householders. Therefore, for the combined race/ethnicity and income ICE calculation
# we use household income for Non-Hispanic White householders as one extreme and
# household income for Black householders (Hispanic origin not disaggregated)
# or Asian householders (Hispanic origin not disaggregated) as the other extreme. 
# This paper does the same and notes the lack of race/ethnicity household income 
# data for NH Black folks in the census:
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
polygons_of_interest_path = here::here('data', 'nyc_census_tracts', 'nycgeo_census_tracts.shp')

# 0d Load non-census data
# 0d.i Load list of census tracts with traffic data
gt_fips <- read_rds(paste0(processed_data_path, 'gt_polyids_2010CTs.rds'))
# 0d.ii Load census tracts geo file for mapping
tracts <- st_read(polygons_of_interest_path)

####***********************************
#### 1: Load and clean census data #### 
####***********************************

# 1a Load & tidy race data
race <- read_csv(paste0(census_data_path, "nhgis_race-ethnicity_tract_15-19.csv")) %>% 
  filter(GISJOIN != "GIS Join Match Code") %>% 
  dplyr::select(GISJOIN, STATEA, COUNTYA, TRACTA, ALUKE001, ALUKE002, ALUKE003,
                ALUKE004, ALUKE005, ALUKE006, ALUKE007, ALUKE008, ALUKE009,
                ALUKE012, ALUKM001, ALUKM002, ALUKM003, ALUKM004, ALUKM005,
                ALUKM006, ALUKM007, ALUKM008, ALUKM009, ALUKM012) %>% 
  rename(fips_code = GISJOIN, state_code = STATEA, county_code = COUNTYA,
         tract_code = TRACTA, all_race_est = ALUKE001, nhwhite_race_est = ALUKE003,
         nhblack_race_est = ALUKE004, nhai_est = ALUKE005, nhasian_est = ALUKE006,
         nhhawaii_est = ALUKE007, nhother_est = ALUKE008, nhmulti_est = ALUKE009,
         hispanic_est = ALUKE012, nh_est = ALUKE002,
         all_moe = ALUKM001, nhwhite_moe = ALUKM003,
         nhblack_moe = ALUKM004, nhai_moe = ALUKM005, nhasian_moe = ALUKM006,
         nhhawaii_moe = ALUKM007, nhother_moe = ALUKM008, nhmulti_moe = ALUKM009,
         hispanic_moe = ALUKM012, nh_moe = ALUKM002) %>% 
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
                                     "nhgis_income_place_15-19.csv")) %>% 
  filter(GISJOIN != "GIS Join Match Code") %>% 
  dplyr::select(GISJOIN, STATEA, PLACEA, AMEJE001, AMEJE002, AMEJE003, AMEJE004,
                AMEJE005, AMEJM001, AMEJM002, AMEJM003, AMEJM004, AMEJM005) %>% 
  rename(fips_code = GISJOIN, state_code = STATEA, place_code = PLACEA,
         q20_upper_est = AMEJE001, q40_upper_est = AMEJE002, q60_upper_est = AMEJE003,
         q80_upper_est = AMEJE004, q95_lower_est = AMEJE005, q20_upper_moe = AMEJM001,
         q40_upper_moe = AMEJM002, q60_upper_moe = AMEJM003, q80_upper_moe = AMEJM004,
         q95_lower_moe = AMEJM005) %>% 
  filter(state_code == "36") %>% filter(place_code == '51000') %>% 
  mutate_at(vars(matches('_est')), as.numeric) %>% 
  mutate_at(vars(matches('_moe')), as.numeric)
  
# 1c Load & tidy household income by race data
#    Notes: Only keeping the income categories closest to the 20th percentile
#           (and below) and closest to the 80th percentile (and above), as these
#           will be used to create the extreme groups
#           $24,999 and less; $125,000 and more
hhincome_race <- read_csv(paste0(census_data_path, "nhgis_race-income_tract_15-19.csv")) %>% 
  filter(GISJOIN != "GIS Join Match Code") %>% 
  dplyr::select(GISJOIN, STATEA, COUNTYA, TRACTA, 
                AMDSE001:AMDSE005, AMDSE015, AMDSE016, AMDSE017, 
                AMDSM001:AMDSM005, AMDSM015, AMDSM016, AMDSM017, 
                AMDUE001:AMDUE005, AMDUE015, AMDUE016, AMDUE017,
                AMDUM001:AMDUM005, AMDUM015, AMDUM016, AMDUM017,
                AMDYE001:AMDYE005, AMDYE015, AMDYE016, AMDYE017, 
                AMDYM001:AMDYM005, AMDYM015, AMDYM016, AMDYM017) %>% 
  rename(fips_code = GISJOIN, state_code = STATEA, county_code = COUNTYA,
         tract_code = TRACTA, 
         black_all_est = AMDSE001, black_iless10_est = AMDSE002,
         black_i10000to14999_est = AMDSE003, black_i15000to19999_est = AMDSE004, 
         black_i20000to24999_est = AMDSE005, black_i125000to149999_est = AMDSE015,
         black_i150000to199999_est = AMDSE016, black_i200000plus_est = AMDSE017,
         black_all_moe = AMDSM001, black_iless10_moe = AMDSM002,
         black_i10000to14999_moe = AMDSM003, black_i15000to19999_moe = AMDSM004, 
         black_i20000to24999_moe = AMDSM005, black_i125000to149999_moe = AMDSM015,
         black_i150000to199999_moe = AMDSM016, black_i200000plus_moe = AMDSM017,
         asian_all_est = AMDUE001, asian_iless10_est = AMDUE002,
         asian_i10000to14999_est = AMDUE003, asian_i15000to19999_est = AMDUE004, 
         asian_i20000to24999_est = AMDUE005, asian_i125000to149999_est = AMDUE015,
         asian_i150000to199999_est = AMDUE016, asian_i200000plus_est = AMDUE017,
         asian_all_moe = AMDUM001, asian_iless10_moe = AMDUM002,
         asian_i10000to14999_moe = AMDUM003, asian_i15000to19999_moe = AMDUM004, 
         asian_i20000to24999_moe = AMDUM005, asian_i125000to149999_moe = AMDUM015,
         asian_i150000to199999_moe = AMDUM016, asian_i200000plus_moe = AMDUM017,
         nhwhite_all_est = AMDYE001, nhwhite_iless10_est = AMDYE002,
         nhwhite_i10000to14999_est = AMDYE003, nhwhite_i15000to19999_est = AMDYE004, 
         nhwhite_i20000to24999_est = AMDYE005, nhwhite_i125000to149999_est = AMDYE015,
         nhwhite_i150000to199999_est = AMDYE016, nhwhite_i200000plus_est = AMDYE017,
         nhwhite_all_moe = AMDYM001, nhwhite_iless10_moe = AMDYM002,
         nhwhite_i10000to14999_moe = AMDYM003, nhwhite_i15000to19999_moe = AMDYM004, 
         nhwhite_i20000to24999_moe = AMDYM005, nhwhite_i125000to149999_moe = AMDYM015,
         nhwhite_i150000to199999_moe = AMDYM016, nhwhite_i200000plus_moe = AMDYM017) %>% 
  filter(state_code == "36") %>% 
  filter(county_code == "005" | county_code == "047" | county_code == "061" | 
           county_code == "081" | county_code == "085") %>% 
  mutate_at(vars(matches('_est')), as.numeric) %>% 
  mutate_at(vars(matches('_moe')), as.numeric) 

# 1d Load & tidy total number of households reporting income for denominator of ICE
hhincome <- read_csv(paste0(census_data_path, "nhgis_income_tract_15-19.csv")) %>% 
  filter(GISJOIN != "GIS Join Match Code") %>% 
  rename(fips_code = GISJOIN, state_code = STATEA, county_code = COUNTYA,
         tract_code = TRACTA, 
         hhincome_all_est = ALW0E001, hhincome_all_moe = ALW0M001) %>% 
  filter(state_code == "36") %>% 
  filter(county_code == "005" | county_code == "047" | county_code == "061" | 
           county_code == "081" | county_code == "085") %>% 
  mutate_at(vars(matches('_est')), as.numeric) %>% 
  mutate_at(vars(matches('_moe')), as.numeric) %>% 
  dplyr::select(fips_code, hhincome_all_est, hhincome_all_moe) 

####**************************************************************
#### 2: Review race distributions in tracts with traffic data #### 
####**************************************************************

# 2a Convert race and ethnicity vars to long format for mapping
# 2a.i Race vars
race_chloropleth <- race %>% 
  dplyr::select(poly_id, all_race_est, nhwhite_race_est:nhmulti_est) %>% 
  pivot_longer(nhwhite_race_est:nhmulti_est, names_to = "race", values_to = "count") %>% 
  mutate(prop = round((count/all_race_est)*100, digits = 0)) %>% 
  mutate(race = case_when(
    race == 'nhwhite_race_est' ~ 'NH White',
    race == 'nhblack_race_est' ~ 'NH Black',
    race == 'nhai_est' ~ 'NH Amer. Indian or AK Native',
    race == 'nhasian_est' ~ 'NH Asian',
    race == 'nhhawaii_est' ~ 'NH Hawaiian or Pac. Isldr',
    race == 'nhother_est' ~ 'NH Other Race',
    race == 'nhmulti_est' ~ 'NH 2+ Races'
  ))
# 2a.ii Ethnicity vars
hisp_chloropleth <- race %>% 
  dplyr::select(poly_id, all_est, nh_est, hispanic_est) %>% 
  pivot_longer(nh_est:hispanic_est, names_to = "ethnicity", values_to = "count") %>% 
  mutate(prop = round((count/all_est)*100, digits = 0)) %>% 
  mutate(ethnicity = case_when(
    ethnicity == 'nh_est' ~ 'Not Hispanic',
    ethnicity == 'hispanic_est' ~ 'Hispanic'))

# 2b Join race and ethnicity vars with census tract geo file
# 2b.i Race vars
race_chloropleth <- race_chloropleth %>% 
  full_join(tracts, by = c('poly_id' = 'geoid'))
hisp_chloropleth <- hisp_chloropleth %>% 
  full_join(tracts, by = c('poly_id' = 'geoid'))

# 2c Create chloropleth map -- all of NYC
# 2c Race vars
race_chloropleth_map <- race_chloropleth %>% 
  filter(!is.na(race)) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry,fill = prop), lwd = 0) +
  scale_fill_viridis_c(name = "", 
                       option = "inferno") +
  facet_wrap(~ race) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
race_chloropleth_map
# 2c Ethnicity vars
hisp_chloropleth_map <- hisp_chloropleth %>% 
  filter(!is.na(ethnicity)) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry,fill = prop), lwd = 0) +
  scale_fill_viridis_c(name = "", 
                       option = "inferno") +
  facet_grid(~ ethnicity) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
hisp_chloropleth_map

# 2d Filter to include only census tracts with traffic data
# 2d.i Race vars
race_chloropleth_map_gtarea <- race_chloropleth %>% 
  filter(!is.na(race)) %>% 
  filter(poly_id %in% gt_fips) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry,fill = prop), lwd = 0) +
  scale_fill_viridis_c(name = "", 
                       option = "inferno") +
  facet_wrap(~ race) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
race_chloropleth_map_gtarea
# 2d.ii Ethnicity vars
hisp_chloropleth_map_gtarea <- hisp_chloropleth %>% 
  filter(!is.na(ethnicity)) %>% 
  filter(poly_id %in% gt_fips) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry,fill = prop), lwd = 0) +
  scale_fill_viridis_c(name = "", 
                       option = "inferno") +
  facet_grid(~ ethnicity) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
hisp_chloropleth_map_gtarea

# Notes: There is considerable overlap between tracts with a high proportion of 
#        Hispanic people and tracts with a high proportion of Black people
#        Racial groups with substantial census tract level populations are mostly
#        NH White, NH Black, and Hispanic. There are less tracts with a larger 
#        proportion of Asian people, but still a decent amount. 

#        Will make an ICE variable with NH White and Black
#        Considering an ICE variable with NH White and Asian
#        Cannot do an ICE variable with Hispanic folks because the census does
#        not disaggregate household income by race+ethnicity (except for NH White)

####*****************************
#### 3: Create ICE variables #### 
####*****************************

# 3a Create ICE variable for income and race/ethnicity combined, extreme groups
#    as NH White and Black
#    Notes: We set as the extreme groups Non-Hispanic White folks in the 80th 
#           income percentile and (1) Black folks in the 20th income percentile
#           and (2) Asian folks in the 20th income percentile
#           For the denominator we use the total number of households with 
#           income data from the hhincome table (not from either race subtable)
ice_hhincome_race <- hhincome_race %>% 
  full_join(hhincome, by = 'fips_code') %>% 
  mutate(lower_extreme_bw = black_iless10_est + black_i10000to14999_est + black_i15000to19999_est +
           black_i20000to24999_est,
         lower_extreme_aw = asian_iless10_est + asian_i10000to14999_est + asian_i150000to199999_est +
           asian_i20000to24999_est,
         higher_extreme = nhwhite_i125000to149999_est + nhwhite_i150000to199999_est + 
           nhwhite_i200000plus_est,
         total = hhincome_all_est,
         ice_hhincome_bw = round((higher_extreme - lower_extreme_bw)/total, digits = 2),
         ice_hhincome_aw = round((higher_extreme - lower_extreme_aw)/total, digits = 2),
         poly_id = paste0(state_code, county_code, tract_code)) 

# 3b Add vars w count of NH-Black pop, NH-White pop, and household income to
#    use in descriptive statistics later
ice_hhincome_race <- 
  merge(x = ice_hhincome_race, y = race[, c('fips_code', 'nhblack_race_est', 
                                            'nhwhite_race_est', 'all_race_est')], 
        by = 'fips_code', all.x = TRUE)

####********************************
#### 4: Map and review ICE vars #### 
####********************************

# 4a Join ice vars with census tract geo file
ice_census_chloropleth <- ice_hhincome_race %>% 
  full_join(tracts, by = c('poly_id' = "geoid"))

# 4b Create chloropleth map -- NH White and Black
ice_census_chloropleth_map_bw <- ice_census_chloropleth %>% 
  filter(!is.na(ice_hhincome_bw)) %>% 
  filter(poly_id %in% gt_fips) %>% 
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
ice_census_chloropleth_map_bw

# 4c Create chloropleth map -- NH White and Asian
ice_census_chloropleth_map_aw <- ice_census_chloropleth %>% 
  filter(!is.na(ice_hhincome_aw)) %>% 
  filter(poly_id %in% gt_fips) %>% 
  ggplot() +
  geom_sf(aes(fill = ice_hhincome_aw, geometry = geometry), lwd = 0) +
  scale_fill_viridis_c(name = "", 
                       option = "inferno",
                       breaks = c(-0.36, 0.65),
                       labels = c("Syst. \nDisadvantaged", 
                                  "Syst. \nPriveleged")) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
ice_census_chloropleth_map_aw

# 4d Review range and central tendency of ICE vars
ice_hhincome_race %>% filter(poly_id %in% gt_fips) %>% 
  summarise(min_bw = min(ice_hhincome_bw, na.rm = T),
            mean_bw = mean(ice_hhincome_bw, na.rm = T),
            sd_bw = sd(ice_hhincome_bw, na.rm = T),
            median_bw = median(ice_hhincome_bw, na.rm = T),
            iqr_bw = IQR(ice_hhincome_bw, na.rm = T),
            mis_bw = sum(is.na(ice_hhincome_bw)),
            min_aw = min(ice_hhincome_aw, na.rm = T),
            mean_aw = mean(ice_hhincome_aw, na.rm = T),
            sd_aw = sd(ice_hhincome_aw, na.rm = T),
            median_aw = median(ice_hhincome_aw, na.rm = T),
            iqr_aw = IQR(ice_hhincome_aw, na.rm = T),
            mis_aw = sum(is.na(ice_hhincome_aw)))

# 4e Histograms for ICE vars
# 4e.i NH White and Black
ice_hhincome_race %>% filter(poly_id %in% gt_fips) %>% 
  ggplot() + geom_histogram(aes(x = ice_hhincome_bw))
# 4e.ii NH White and Asian
ice_hhincome_race %>% filter(poly_id %in% gt_fips) %>% 
  ggplot() + geom_histogram(aes(x = ice_hhincome_aw))

# Notes: The ICE var with NH White and Asian as the extreme groups does not
#        have as much variability. 

# 4f Review CTs with missing data
#    Notes: n = 8 CTs with missing data, all but 2 had recorded pop of 0
#           (if using 2020 CTs, 15 CTs with missing data, all but 2 had pop = 0)
#           005 19.03 132nd to 140 including Locust and Walnut. Appears very
#               industrial, but could have people living, census reports pop = 0
#           005 19.04 North Brother Island (nothing there)
#           005 63.02 Yankee Stadium
#           005 171 Claremont Park
#           005 249 Bronx Community College
#           047 579.02 Newtown Creek Wastewater Treatment Plant
#           061 86.02 United Nations
#           061 143 Central Park
#           061 217.03 City College
#           061 297 Inwood Hill Park; pop of 21, no med household income reported
#           061 311 High Bridge Park along Harlem River Drive
#           061 319 Battery Park
#           081 1.03 Con Edison, very industrial
#           081 1.04 population of 261, no median household income reported
#           081 37 East River waterfront, mix of parks, Ravenswood Generating Station
mis <- ice_hhincome_race %>% 
  filter(poly_id %in% gt_fips) %>% 
  filter(is.na(ice_hhincome_bw) | is.na(ice_hhincome_aw))

####**********************
#### 5: Save out data #### 
####**********************

# 5a Save out data
ice_hhincome_race %>% 
  dplyr::select(poly_id, ice_hhincome_bw, ice_hhincome_aw,
                all_race_est, nhblack_race_est, nhwhite_race_est) %>% 
    write_fst(path = paste0(processed_data_path, 'ice_census_vars_2010CTs.fst')) 






