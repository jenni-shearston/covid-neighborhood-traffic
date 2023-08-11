# Load and Prepare Census Data for Index of Conc at Extremes
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 03/16/2023

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Recalculate EJI with rankings based on NYC tracts
# 2: Recalculate EJI with traffic related vars removed
# 3: Map and review EJI vars
# 4: Save out data

####**************
#### N: Notes ####
####**************

# Na Description
# In this script, we prepare the CDC / ATSDR environmental justice index for use
# in NYC: https://www.atsdr.cdc.gov/placeandhealth/eji/technical_documentation.html
# In order to make NYC-level rankings, we recalculated rankings using only NYC tracts 
# as EJI rankings were originally calculated with all tracts nationally. We also
# calculated a second EJI where we removed traffic related variables, for use in 
# a sensitivity analysis. 

# Of note, the EJI 2022 uses 2010 census tracts, which would leave 76 CTs missing
# EJI data if we use 2020 census tracts for traffic data and would create spatial
# misalignment between EJI CTs and traffic CTs. 
# I considered imputing some 2020 census tracts with 2010 EJI data, for example, 
# when a census tract was only split into smaller units. However, I chose not to use
# this approach as many of the EJI variables are aggregated from spatial areas other  
# than CTs, such as point source data with buffers for airport location and other vars. 
# If the EJI had been calculated using 2020 CTs, each of the sub-CTs from a single
# 2010 CT may actually have been given different values. Therefore, for this 
# analysis, we use traffic aggregated to 2010 census tracts

####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load Packages
source(paste0(project.folder, 'packages.R'))

# 0c Set up filepath(s)
raw_data_path <- paste0(project.folder, 'data/raw_data/')
processed_data_path <- paste0(project.folder, 'data/processed_data/')
polygons_of_interest_path = here::here('data', 'nyc_census_tracts', 'nycgeo_census_tracts.shp')

# 0d Load list of census tracts with traffic data
gt_fips <- read_rds(paste0(processed_data_path, 'gt_polyids_2010CTs.rds'))

# 0e Load census tracts geo file for mapping
tracts <- st_read(polygons_of_interest_path) %>% janitor::clean_names()

# 0f Load eji data and restrict to NYC
eji <- read_csv(paste0(raw_data_path, 'EJI_2022_NY.csv')) %>% 
  janitor::clean_names() %>% 
  filter(countyfp == 5 | countyfp == 47 | countyfp == 61 | 
           countyfp == 81 | countyfp == 85) %>% 
  mutate(geoid = as.character(geoid)) 

# 0g Load 2010-->2020 CT crosswalk
#    We chose not to use the 2020 CTs, so a crosswalk is no longer needed
#    Note: the number of census tracts in NYC increased from 2,168 in 2010 to
#          2,327 in 2020
#          https://storymaps.arcgis.com/stories/d30850ba28944619b94e8ee4f746d5c4
# cw <- read_csv(paste0(raw_data_path, 'nhgis_blk2010_blk2020_ge_36.csv'), col_types = 'ccnn') %>% 
#   janitor::clean_names() %>% 
#   mutate(countyfp = str_sub(geoid20, start = 3, end = 5),
#          tract10 = str_sub(geoid10, end = -5),
#          tract20 = str_sub(geoid20, end = -5)) %>% 
#   filter(countyfp == '005' | countyfp == '047' | countyfp == '061' | 
#            countyfp == '081' | countyfp == '085') %>% 
#   dplyr::select(tract10, tract20) %>% distinct()

####***********************************************************
#### 1: Recalculate EJI with rankings based on NYC tracts  #### 
####***********************************************************

# In this section, we recalculate rankings using only NYC tracts 
# as EJI rankings were originally calculated with all tracts nationally

# 1a Keep only geoid and variables that are estimates rather than percentile
#    ranks (indicated by a var name starting with ep_ or e_
eji_nyc <- eji %>% 
  dplyr::select(geoid, m_totpop, starts_with(c('ep_', 'e_')))

# 1b Calculate percentile ranks for each estimate and add 'epl' to end of var name
eji_nyc <- eji_nyc %>% 
  mutate_at(vars(starts_with(c('ep_', 'e_'))), list(epl = percent_rank))

# 1c Identify tracts with greater than 0.6666 percentile rank of health conditions
#    Note: 66.66% is the value used in EJI
eji_nyc <- eji_nyc %>% 
  mutate(f_bphigh = ifelse(ep_bphigh_epl > 0.6666, 1, 0),
         f_asthma = ifelse(ep_asthma_epl > 0.6666, 1, 0),
         f_cancer = ifelse(ep_cancer_epl > 0.6666, 1, 0),
         f_mhlth = ifelse(ep_mhlth_epl > 0.6666, 1, 0),
         f_diabetes = ifelse(ep_diabetes_epl > 0.6666, 1, 0))
  
# 1d Sum percentile ranks for each module
eji_nyc <- eji_nyc %>% 
  mutate(spl_ebm = # Environmental Burden Module
           e_ozone_epl + e_pm_epl + e_dslpm_epl + e_totcr_epl + # EBM dom 1
           e_npl_epl + e_tri_epl + e_tsd_epl + e_rmp_epl + e_coal_epl + e_lead_epl + # EBM dom 2
           e_park_epl + e_houage_epl + e_wlkind_epl + # EBM dom 3
           e_rail_epl + e_road_epl + e_airprt_epl + # EBM dom 4
           e_impwtr_epl, # EBM dom 5
         spl_svm = # Social Vulnerability Module
           ep_minrty_epl + # SVI dom 1
           ep_pov200_epl + ep_nohsdp_epl + ep_unemp_epl + ep_renter_epl + ep_houbdn_epl + ep_uninsur_epl + ep_noint_epl + # SVI dom 2
           ep_age65_epl + ep_age17_epl + ep_disabl_epl + ep_limeng_epl + # SVI dom 3
           ep_mobile_epl + ep_groupq_epl, # SVI dom 4
         f_hvm = # Health Vulnerability Module
           f_bphigh + f_asthma + f_cancer + f_mhlth + f_diabetes)

# 1e Calculate percentile ranks of sum for each module
eji_nyc <- eji_nyc %>% 
  mutate(rpl_ebm = percent_rank(spl_ebm),
         rpl_svm = percent_rank(spl_svm),
         rpl_hvm = f_hvm * 0.2)

# 1f Calculate overall EJI
eji_nyc <- eji_nyc %>% 
  mutate(spl_eji = rpl_ebm + rpl_svm + rpl_hvm,
         rpl_eji = percent_rank(spl_eji))
  
####***********************************************************
#### 2: Recalculate EJI with traffic related vars removed  #### 
####***********************************************************

# 2a Sum percentile ranks for Environmental Burden Module without traffic
#    related vars
#    Notes: Removed PM2.5, diesel particulate matter, high volume roads
eji_nyc <- eji_nyc %>% 
  mutate(spl_ebm_sens = # Environmental Burden Module
           e_ozone_epl + e_totcr_epl + # EBM dom 1
           e_npl_epl + e_tri_epl + e_tsd_epl + e_rmp_epl + e_coal_epl + e_lead_epl + # EBM dom 2
           e_park_epl + e_houage_epl + e_wlkind_epl + # EBM dom 3
           e_rail_epl + e_airprt_epl + # EBM dom 4
           e_impwtr_epl) # EBM dom 5

# 2b Calculate percentile rank for adjusted EBM score
eji_nyc <- eji_nyc %>% 
  mutate(rpl_ebm_sens = percent_rank(spl_ebm_sens))

# 2c Calculate overall EJI using adjusted EBM score
eji_nyc <- eji_nyc %>% 
  mutate(spl_eji_sens = rpl_ebm_sens + rpl_svm + rpl_hvm,
         rpl_eji_sens = percent_rank(spl_eji_sens))

####********************************
#### 3: Map and review EJI vars #### 
####********************************

# 3a Join eji vars with census tract geo file
# 3a.i EJI score recalculated with NYC only tracts in ranking
eji_nyc_chloropleth <- eji_nyc %>% 
  full_join(tracts, by = 'geoid')
# 3a.ii Original EJI score using national tracts in ranking
eji_chloropleth <- eji %>% 
  full_join(tracts, by = 'geoid')

# 3b Create chloropleth map -- EJI NYC rankings
# 3b.i EJI score recalculated with NYC only tracts in ranking
eji_nyc_chloropleth_map <- eji_nyc_chloropleth %>% 
  filter(geoid %in% gt_fips) %>% 
  ggplot() +
  geom_sf(aes(fill = rpl_eji, geometry = geometry), lwd = 0) +
  scale_fill_viridis_c(name = "EJI Score", 
                       option = "inferno",
                       breaks = c(0.05, 1),
                       labels = c('Low Env Burden', 
                                  'High Env Burden')) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
eji_nyc_chloropleth_map
# 3b.ii Original EJI score using national tracts in ranking
eji_chloropleth_map <- eji_chloropleth %>% 
  filter(geoid %in% gt_fips) %>% 
  ggplot() +
  geom_sf(aes(fill = rpl_eji, geometry = geometry), lwd = 0) +
  scale_fill_viridis_c(name = "EJI Score", 
                       option = "inferno",
                       breaks = c(0.23, 1),
                       labels = c('Low Env Burden', 
                                  'High Env Burden')) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
eji_chloropleth_map
# 3b.iii EJI score recalculated with NYC only tracts in ranking and an 
#        adjusted EB module to remove traffic items
eji_nyc_sens_chloropleth_map <- eji_nyc_chloropleth %>% 
  filter(geoid %in% gt_fips) %>% 
  ggplot() +
  geom_sf(aes(fill = rpl_eji_sens, geometry = geometry), lwd = 0) +
  scale_fill_viridis_c(name = "Mod. EJI Score", 
                       option = "inferno",
                       breaks = c(0.05, 1),
                       labels = c('Low Env Burden', 
                                  'High Env Burden')) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
eji_nyc_sens_chloropleth_map

# 3c Review range and central tendency of EJI score
# 3c.i EJI score recalculated with NYC only tracts in ranking
eji_nyc %>% filter(geoid %in% gt_fips) %>% 
  summarise(min = min(rpl_eji, na.rm = T),
            max = max(rpl_eji, na.rm = T),
            mean = mean(rpl_eji, na.rm = T),
            sd = sd(rpl_eji, na.rm = T),
            median = median(rpl_eji, na.rm = T),
            iqr = IQR(rpl_eji, na.rm = T))
# 3c.ii Original EJI score using national tracts in ranking
eji %>% filter(geoid %in% gt_fips) %>% 
  summarise(min = min(rpl_eji, na.rm = T),
            max = max(rpl_eji, na.rm = T),
            mean = mean(rpl_eji, na.rm = T),
            sd = sd(rpl_eji, na.rm = T),
            median = median(rpl_eji, na.rm = T),
            iqr = IQR(rpl_eji, na.rm = T))
# 3c.iii EJI score recalculated with NYC only tracts in ranking and an 
#        adjusted EB module to remove traffic items
eji_nyc %>% filter(geoid %in% gt_fips) %>% 
  summarise(min = min(rpl_eji_sens, na.rm = T),
            max = max(rpl_eji_sens, na.rm = T),
            mean = mean(rpl_eji_sens, na.rm = T),
            sd = sd(rpl_eji_sens, na.rm = T),
            median = median(rpl_eji_sens, na.rm = T),
            iqr = IQR(rpl_eji_sens, na.rm = T))

# 3d Histogram of EJI score
# 3d.i EJI score recalculated with NYC only tracts in ranking
eji_nyc %>% filter(geoid %in% gt_fips) %>% 
  ggplot() + geom_histogram(aes(x = rpl_eji))
# 3d.ii Original EJI score using national tracts in ranking
eji %>% filter(geoid %in% gt_fips) %>% 
  ggplot() + geom_histogram(aes(x = rpl_eji))
# 3d.iii EJI score recalculated with NYC only tracts in ranking and an 
#        adjusted EB module to remove traffic items
eji_nyc %>% filter(geoid %in% gt_fips) %>% 
  ggplot() + geom_histogram(aes(x = rpl_eji_sens))

# Notes: When using the NYC version there is greater variability (as to be expected),
#        but the mean and median are still very similar. 
#        When using the modified EJI for NYC, the min decreases to 0.0005

# 3e Determine correlation between EJI modules
eji_cors <- eji_nyc %>% dplyr::select(rpl_ebm, rpl_svm, rpl_hvm)
eji_cors1 <- cor(eji_cors, method = c('spearman'), use = 'complete.obs')

# 3f Count CTs missing EJI values
#    n = 8
#    These are CTs that do not have some census data (see Script 1_04)
CTs_mis_eji <- gt_fips[!gt_fips %in% eji_nyc$geoid]

####**********************
#### 4: Save out data #### 
####**********************

# 4a Keep needed vars
eji <- eji %>% dplyr::select(geoid, m_totpop, rpl_eji, rpl_ebm, rpl_svm, rpl_hvm)
eji_nyc <- eji_nyc %>% dplyr::select(geoid, m_totpop, 
                                     rpl_eji, rpl_ebm, rpl_svm, rpl_hvm,
                                     rpl_eji_sens, rpl_ebm_sens)

# 4b Save out data
write_fst(eji, path = paste0(processed_data_path, 'eji.fst')) 
write_fst(eji_nyc, path = paste0(processed_data_path, 'eji_nyc.fst')) 


