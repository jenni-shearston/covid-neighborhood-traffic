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
  na.omit()


####***********************************************************
#### 1: Recalculate EJI with rankings based on NYC tracts  #### 
####***********************************************************

# In this section, we recalculate rankings using only NYC tracts 
# as EJI rankings were originally calculated with all tracts nationally

# 1a Keep only geoid and variables that are estimates rather than percentile
#    ranks (indicated by a var name starting with ep_ or e_
eji_nyc <- eji %>% 
  dplyr::select(geoid, starts_with(c('ep_', 'e_')))

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
  

####********************************
#### 4: Map and review EJI vars #### 
####********************************

# 4a Join eji vars with census tract geo file
ice_census_chloropleth <- ice_hhincome_race %>% 
  full_join(tracts, by = c('poly_id' = "geoid"))

# 4b Create chloropleth map -- NH White and Black
ice_census_chloropleth_map_bw <- ice_census_chloropleth %>% 
  filter(!is.na(ice_hhincome_bw)) %>% 
  filter(poly_id %in%gt_fips$poly_id) %>% 
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
  filter(poly_id %in%gt_fips$poly_id) %>% 
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
ice_hhincome_race %>% filter(poly_id %in%gt_fips$poly_id) %>% 
  summarise(min_bw = min(ice_hhincome_bw, na.rm = T),
            mean_bw = mean(ice_hhincome_bw, na.rm = T),
            sd_bw = sd(ice_hhincome_bw, na.rm = T),
            median_bw = median(ice_hhincome_bw, na.rm = T),
            iqr_bw = IQR(ice_hhincome_bw, na.rm = T),
            min_aw = min(ice_hhincome_aw, na.rm = T),
            mean_aw = mean(ice_hhincome_aw, na.rm = T),
            sd_aw = sd(ice_hhincome_aw, na.rm = T),
            median_aw = median(ice_hhincome_aw, na.rm = T),
            iqr_aw = IQR(ice_hhincome_aw, na.rm = T))

# 4e Histograms for ICE vars
# 4e.i NH White and Black
ice_hhincome_race %>% filter(poly_id %in%gt_fips$poly_id) %>% 
  ggplot() + geom_histogram(aes(x = ice_hhincome_bw))
# 4e.ii NH White and Asian
ice_hhincome_race %>% filter(poly_id %in%gt_fips$poly_id) %>% 
  ggplot() + geom_histogram(aes(x = ice_hhincome_aw))

# Notes:  


####**********************
#### 5: Save out data #### 
####**********************

# 5a Save out data
write_fst(eji, path = paste0(processed_data_path, 'eji.fst')) 



