# Load and Prepare Census Data for Index of Conc at Extremes
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 12/21/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Load and clean 


####**************
#### N: Notes ####
####**************

# Na Description
# In this script we 

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

# 0d Load list of census tracts with traffic data
gt_fips <- read_fst(paste0(processed_data_path, 'gt_polyids.fst')) %>% na.omit()

# 0e Load census tracts geo file for mapping
tracts <- nycgeo::nyc_boundaries(geography = "tract")

# 0f Load eji data and restrict to NYC
eji <- read_csv(paste0(raw_data_path, 'EJI_2022_NY.csv')) %>% 
  janitor::clean_names() %>% 
  filter(countyfp == 5 | countyfp == 47 | countyfp == 61 | 
           countyfp == 81 | countyfp == 85) %>% 
  mutate(geoid = as.character(geoid)) %>% 
  na.omit()


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
  filter(geoid %in% gt_fips$poly_id) %>% 
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
  filter(geoid %in% gt_fips$poly_id) %>% 
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
  filter(geoid %in% gt_fips$poly_id) %>% 
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
eji_nyc %>% filter(geoid %in% gt_fips$poly_id) %>% 
  summarise(min = min(rpl_eji, na.rm = T),
            max = max(rpl_eji, na.rm = T),
            mean = mean(rpl_eji, na.rm = T),
            sd = sd(rpl_eji, na.rm = T),
            median = median(rpl_eji, na.rm = T),
            iqr = IQR(rpl_eji, na.rm = T))
# 3c.ii Original EJI score using national tracts in ranking
eji %>% filter(geoid %in% gt_fips$poly_id) %>% 
  summarise(min = min(rpl_eji, na.rm = T),
            max = max(rpl_eji, na.rm = T),
            mean = mean(rpl_eji, na.rm = T),
            sd = sd(rpl_eji, na.rm = T),
            median = median(rpl_eji, na.rm = T),
            iqr = IQR(rpl_eji, na.rm = T))
# 3c.iii EJI score recalculated with NYC only tracts in ranking and an 
#        adjusted EB module to remove traffic items
eji_nyc %>% filter(geoid %in% gt_fips$poly_id) %>% 
  summarise(min = min(rpl_eji_sens, na.rm = T),
            max = max(rpl_eji_sens, na.rm = T),
            mean = mean(rpl_eji_sens, na.rm = T),
            sd = sd(rpl_eji_sens, na.rm = T),
            median = median(rpl_eji_sens, na.rm = T),
            iqr = IQR(rpl_eji_sens, na.rm = T))

# 3d Histogram of EJI score
# 3d.i EJI score recalculated with NYC only tracts in ranking
eji_nyc %>% filter(geoid %in% gt_fips$poly_id) %>% 
  ggplot() + geom_histogram(aes(x = rpl_eji))
# 3d.ii Original EJI score using national tracts in ranking
eji %>% filter(geoid %in% gt_fips$poly_id) %>% 
  ggplot() + geom_histogram(aes(x = rpl_eji))
# 3d.iii EJI score recalculated with NYC only tracts in ranking and an 
#        adjusted EB module to remove traffic items
eji_nyc %>% filter(geoid %in% gt_fips$poly_id) %>% 
  ggplot() + geom_histogram(aes(x = rpl_eji_sens))

# Notes: When using the NYC version there is greater variability (as to be expected),
#        but the mean and median are still very similar. For both scores,
#        tracts in the South Bronx have the highest env burden scores.
#        When using the modified EJI for NYC, the min decreases to 0.0005


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


