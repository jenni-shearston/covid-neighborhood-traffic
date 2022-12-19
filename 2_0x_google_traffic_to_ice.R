# Create Google Traffic Ice Variable and Clean GT Data
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 02/05/2022

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


# Nb Index of Concentration at the Extremes Calculation
# ICEi = (Ai-Pi)/Ti
# where, say, in the case of the ICE for income,
# Ai is equal to the number of affluent persons in unit of analysis i 
# (e.g., in the 80th income percentile), 
# Pi is equal to the number of poor persons in unit of analysis i 
# (e.g., in the 20th income percentile), 
# Ti is equal to the total population with known income in unit of analysis i


####********************
#### 0: Preparation #### 
####********************

# 0a Load Packages
packages <- c("tidyverse", "fst", "lubridate", "nycgeo", "sf", "scales")
lapply(packages, library, character.only = TRUE)

# 0b Load data
gt_3mon <- fst::read_fst(here::here('data', 'google_traffic_jantomar2020.fst'))
tracts <- nycgeo::nyc_boundaries(geography = "tract")


####********************************************
#### 1: Review and Tidy Google Traffic Data #### 
####********************************************

# 1a Review data
names(gt_3mon)
summary(gt_3mon)

# 1b Clean poly_id and captured_datetime
gt_3mon <- gt_3mon %>% 
  mutate(poly_id = as.character(poly_id),
         captured_datetime = mdy_hm(captured_datetime))

# 1c Identify captured datetimes without gt data
#    Note: n=1603 datetimes with no gt data because data is every three hours 
#          (not hourly) and there are also missing chunks of time (e.g., when
#          the computer went down)
gt_3mon %>% filter(is.na(poly_id)) %>% 
  dplyr::select(captured_datetime) -> missing_datetimes

# 1d Count and list number of census tracts 
#    Note: 635 census tracts
n_distinct(gt_3mon$poly_id) 
gt_3mon %>% dplyr::select(poly_id) %>% distinct(poly_id) -> poly_id_list

# 1e Plot census tracts included in gt capture area
# 1e.i Merge gt data to census tracts geometry file
tracts_gt <- tracts %>% right_join(poly_id_list, by = c("geoid" = "poly_id"))
# 1e.ii Plot tracts in gt capture area
tracts_gt %>% 
  filter(!is.na(geoid)) %>% 
  ggplot() + geom_sf() + theme_void()


####********************************************
#### 2: Create Google Traffic ICE Variable #### 
####********************************************

# check this whole section with Markus
# 1. Is this a good way to come up with the total num pixels that could be a traffic
#    color for each census tract? --> Yes MH agrees
# 2. Should the most free-flowing group be green or gray or a combo? --> use
#    maroon and green + gray

# 2a Create total variable for ice calculation
#    Notes: Here we use the total pixels minus background
#           + not sampled pixels. We group by poly_id so that every
#           census tract with the same poly_id has the same number of
#           total pixels. We average each variable before adding and
#           subtracting becuase there are different values of background
#           and not sampled pixels for the same poly_id at different datetimes
gt_3mon <- gt_3mon %>% group_by(poly_id) %>% 
  mutate(total_for_ice = mean(gt_pixcount_tot, na.rm = T) - 
           (mean(gt_pixcount_background, na.rm = T) + 
              mean(gt_pixcount_notsampled, na.rm = T)),
         total_for_ice = round(total_for_ice, digits = 2)) %>% 
  ungroup()
         
# 2b Create ice variable
#    Note: Here we set the high extreme group to be green (free-flowing)
#          and the low extreme to be maroon (severe congestion)
gt_3mon <- gt_3mon %>% mutate(ice_gt = (gt_pixcount_green - gt_pixcount_maroon) 
                              / total_for_ice, 
                              ice_gt = round(ice_gt, digits = 2))
summary(gt_3mon$ice_gt)

# 2c Plot chloropleth map of mean value of ice var
# 2c.i Summarize ice_gt to mean value for each census tract
ice_gt_mean <- gt_3mon %>% group_by(poly_id) %>% 
  summarize(ice_gt_mean = mean(ice_gt, na.rm = T), 
            ice_gt_mean = round(ice_gt_mean, digits = 2))
summary(ice_gt_mean$ice_gt_mean)
# 2c.ii Merge gt data to census tracts geometry file
tracts_gt_ice <- tracts %>% right_join(ice_gt_mean, by = c("geoid" = "poly_id"))
# 2c.iii Determine min and max mean gt_ice values for plot legend
min_ice_gt_mean <- min(tracts_gt_ice$ice_gt_mean, na.rm = T)
max_ice_gt_mean <- max(tracts_gt_ice$ice_gt_mean, na.rm = T)
# 2c.iii Plot chloropleth or mean ice_gt
tracts_gt_ice %>% 
  filter(!is.na(ice_gt_mean)) %>% 
  ggplot() + geom_sf(aes(fill = ice_gt_mean), lwd = 0) + 
  scale_fill_gradient2(name = "Traffic \n Congestion",
    breaks = c(min_ice_gt_mean, max_ice_gt_mean),
    labels = c("Medium", "Free-flowing"),
    low = "red4", mid = "darkorange2", high = "green", midpoint = 0,
    space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"))


####***********************
#### 3: Save out files #### 
####***********************

# 3a Save out datafiles
write_fst(gt_3mon, here::here("data", "ice_gt_jantomar2020.fst"))
write_fst(missing_datetimes, here::here("data", "gt_missing_datetimes.fst"))
write_fst(poly_id_list, here::here("data", "gt_polyids.fst"))





