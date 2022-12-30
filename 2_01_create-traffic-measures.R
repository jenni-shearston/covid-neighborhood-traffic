# Create traffic measures
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
# In this script we create multiple measures of traffic at the census tract level.
# We start with five measures of traffic, corresponding to the count of pixels assigned
# to each congestion color code for each census tract (maroon, red, orange, green, grey).
# Ultimately, we want to develop (1) a single measure of traffic for each census tract, and 
# (2) traffic measure(s) that represent different components of exposure to traffic, such
# as air pollution, noise, or severance.

# TO DO: for deciding on denominator for proportion green or red+maroon vars,
#        check variability of road pixels / total pixels. If not much variability
#        it might not matter whether the denominator is road pixels vs total area

####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages & passwords
source(paste0(project.folder, 'packages.R'))
source(paste0(project.folder, 'passwords.R'))

# 0c Set up filepath(s)
raw_data_path <- paste0(project.folder, 'data/raw_data/')

# 0b Load data
# 0b.i Load traffic data
#      Note: 634 census tracts
traf <- read_fst(paste0(data_path, 'google_traffic_jantomar2020.fst')) %>% 
  dplyr::select(poly_id, captured_datetime, everything()) %>% 
  filter(!is.na(poly_id)) %>% 
  mutate(poly_id = as.character(poly_id)) 
# 0b.ii Load census tract sf file
tracts_sf <- nycgeo::nyc_boundaries(geography = "tract")
# 0b.iii Load NO2 data
no2 <- read_fst(paste0(data_path, 'no2_janToMar2020_is52.fst')) %>% 
  dplyr::select(poly_id, date_gmt, time_gmt, sample_measurement)

# 0c Merge traffic data with sf of tract boundaries for visualization
traf_sf <- traf %>% left_join(tracts_sf, by = c('poly_id' = 'geoid'))
traf_sf %>% filter(captured_datetime == '0107201530') %>% 
  ggplot() + geom_sf(aes(geometry = geometry, fill = gt_pixcount_tot))

# 0d Review distribution of five congestion colors
traf_sf %>% ggplot(aes(x = gt_pixcount_maroon/gt_pixcount_tot)) + geom_histogram()
traf_sf %>% ggplot(aes(x = gt_pixcount_red/gt_pixcount_tot)) + geom_histogram()
traf_sf %>% ggplot(aes(x = gt_pixcount_orange/gt_pixcount_tot)) + geom_histogram()
traf_sf %>% ggplot(aes(x = gt_pixcount_green/gt_pixcount_tot)) + geom_histogram()
traf_sf %>% ggplot(aes(x = gt_pixcount_gray/gt_pixcount_tot)) + geom_histogram()


####*******************************************
#### 1: Create traffic congestion measures #### 
####*******************************************

# 1a Create variables representing the number of road pixels in each CT, 
#    proportion of roads red + maroon for each CT/datetime, and proportion
#    of roads green for each CT/datetime
traf_sf <- traf_sf %>% ungroup() %>% 
  mutate(road_pixels1 = gt_pixcount_maroon + gt_pixcount_red + 
                             gt_pixcount_orange + gt_pixcount_green +
                             gt_pixcount_gray) %>% 
  group_by(poly_id) %>%
  mutate(road_pixels = max(road_pixels1)) %>% 
  ungroup() %>% 
  dplyr::select(-road_pixels1) %>% 
  mutate(prop_maroon_red = ((gt_pixcount_maroon + gt_pixcount_red) / road_pixels) * 100,
         prop_green = (gt_pixcount_green / road_pixels) * 100) %>% 
  dplyr::select(poly_id, captured_datetime, road_pixels, prop_maroon_red,
                prop_green, everything())

# 1b Create a free-flow variable defined as the 85th percentile of the number of 
#    green + gray pixels during off-peak time (weeknights 10-5) for a census tract
free_flow <- traf_sf %>% dplyr::select(poly_id, captured_datetime, 
                                       gt_pixcount_green, gt_pixcount_gray) %>% 
  mutate(datetime = lubridate::mdy_hm(captured_datetime, tz = 'America/New_York'),
         hour_of_day = lubridate::hour(datetime)) %>% 
  filter(hour_of_day > 21 | hour_of_day < 06) %>% 
  mutate(green_gray = gt_pixcount_green + gt_pixcount_gray) %>% 
  group_by(poly_id) %>% 
  mutate(green_gray_85 = quantile(green_gray, c(.85))) %>% 
  ungroup() %>% 
  dplyr::select(poly_id, green_gray_85) %>% distinct()

# 1c Create speed reduction factor and congestion variable based on the free-flow
#    variable defined in 1b
traf_sf <- traf_sf %>% 
  left_join(free_flow, by = 'poly_id') %>% 
  mutate(speed_reduct_fact = ((gt_pixcount_green + gt_pixcount_gray) / green_gray_85)*100,
         congest = case_when(
           speed_reduct_fact >= 80 ~ 'no to low congestion',
           speed_reduct_fact >= 65 & speed_reduct_fact < 80 ~ 'mod congestion',
           speed_reduct_fact < 65 ~ 'severe congestion'
         )) %>% 
  dplyr::select(poly_id, captured_datetime, congest, speed_reduct_fact, 
                green_gray_85, everything())

# 1d Use the index of concentration at the extremes to create one traffic variable
#    ICEi = (Ai-Pi)/Ti
traf_sf <- traf_sf %>% 
  mutate(ice_gt = (gt_pixcount_green - gt_pixcount_maroon) / road_pixels, 
         ice_gt = round(ice_gt, digits = 2))

# 1e Check variability of new vars with histograms and boxplots
traf_sf %>% ggplot(aes(x = prop_maroon_red)) + geom_histogram()
traf_sf %>% ggplot(aes(y = prop_maroon_red)) + geom_boxplot()
traf_sf %>% ggplot(aes(x = prop_green)) + geom_histogram()
traf_sf %>% ggplot(aes(y = prop_green)) + geom_boxplot()
traf_sf %>% ggplot(aes(x = speed_reduct_fact)) + geom_histogram()
traf_sf %>% ggplot(aes(y = speed_reduct_fact)) + geom_boxplot()
traf_sf %>% ggplot(aes(x = ice_gt)) + geom_histogram()
traf_sf %>% ggplot(aes(y = ice_gt)) + geom_boxplot()
traf_sf %>% ggplot(aes(x = congest)) + geom_bar()


####******************************************************************
#### 2: Evaluate correlations between traffic congestion measures #### 
####******************************************************************

# 2a Select variables to include in correlation matrix 
traf_cors <- traf_sf %>% 
  dplyr::select(ice_gt, speed_reduct_fact, green_gray_85, prop_green,
                prop_maroon_red, gt_pixcount_maroon, gt_pixcount_red,
                gt_pixcount_orange, gt_pixcount_green, gt_pixcount_gray) 

# 2b Run correlations
traf_cors1 <- cor(traf_cors, method = c('spearman'), use = 'complete.obs')
traf_cors2 <- rcorr(as.matrix(traf_cors))
traf_cors2_coeff <- traf_cors2$r
traf_cors2_p <- traf_cors2$P

# 2c Create correlation plot 
corrplot(traf_cors1)

# 2d Convert from wide to long to make scatterplots
traf_scatter <- traf_sf %>% 
  dplyr::select(poly_id, captured_datetime,
                ice_gt, speed_reduct_fact, green_gray_85, prop_green,
                prop_maroon_red, gt_pixcount_maroon, gt_pixcount_red,
                gt_pixcount_orange, gt_pixcount_green, gt_pixcount_gray) %>% 
  pivot_longer(cols = gt_pixcount_maroon:gt_pixcount_gray,
               names_to = 'gt_color',
               values_to = 'gt_value')

# 2e Scatterplots of each metric against original gt_pixcount values w Loess curves
#    Note: These plots take a long time to load
#          gt_pixcount_green and speed_reduct_fact have nonlinear relationship
#            where it is positive until the free-flowing cutoff and then negative
traf_scatter %>% ggplot(aes(x = ice_gt, y = gt_value)) + geom_point(shape = 1) +
  geom_smooth() + facet_wrap(~ gt_color) + ggtitle('ice_gt')
traf_scatter %>% ggplot(aes(x = speed_reduct_fact, y = gt_value)) + geom_point(shape = 1) +
  geom_smooth() + facet_wrap(~ gt_color) + ggtitle('speed_reduct_fact')
traf_scatter %>% ggplot(aes(x = prop_green, y = gt_value)) + geom_point(shape = 1) +
  geom_smooth() + facet_wrap(~ gt_color) + ggtitle('prop_green')
traf_scatter %>% ggplot(aes(x = prop_maroon_red, y = gt_value)) + geom_point(shape = 1) +
  geom_smooth() + facet_wrap(~ gt_color) + ggtitle('prop_maroon_red')


####******************************************************************
#### 3: Correlation between traffic congestion measures and NO2 #### 
####******************************************************************

# 3a Restrict to the one ct with no2 data and create datetime variable in GMT
#    Note: IS 52 is at 681 Kelly St in the Bronx. This address is in 
#          census tract 83. However, it is 1 block from 85, and 1.5
#          blocks from 79.
#    Source: https://www1.nyc.gov/assets/planning/download/pdf/about/publications/maps/bx-census-tracts-map.pdf
traf_no2 <- traf_sf %>% filter(poly_id == '36005008300') %>% 
  mutate(datetime = lubridate::mdy_hm(captured_datetime, tz = 'America/New_York'),
         datetime_gmt = lubridate::with_tz(datetime, tzone = 'GMT'),
         date_gmt = lubridate::date(datetime_gmt),
         hour_gmt = lubridate::hour(datetime_gmt))

# 3b Create datetime variable in NO2 data
no2 <- no2 %>% 
  mutate(date_gmt = lubridate::ymd(date_gmt, tz = 'GMT'), 
         time_gmt = lubridate::ymd_hm(paste0(date_gmt, time_gmt), tz = 'GMT'),
         hour_gmt = lubridate::hour(time_gmt)) %>% 
  dplyr::select(-time_gmt)

# 3c Merge NO2 data with traffic data 
traf_no2 <- traf_no2 %>% 
  left_join(no2, by = c('poly_id', 'date_gmt', 'hour_gmt'))

# 3d Select variables to include in correlation matrix 
traf_no2_cors <- traf_no2 %>% filter(poly_id == '36005008300') %>% 
  dplyr::select(ice_gt, speed_reduct_fact, prop_green,
                prop_maroon_red, gt_pixcount_maroon, gt_pixcount_red,
                gt_pixcount_orange, gt_pixcount_green, gt_pixcount_gray,
                sample_measurement) 

# 2b Run correlations
traf_no2_1 <- cor(traf_no2_cors, method = c('spearman'), use = 'complete.obs')
traf_no2_2 <- rcorr(as.matrix(traf_no2_cors))
traf_no2_2_coeff <- traf_no2_2$r
traf_no2_2_p <- traf_no2_2$P

# 2c Create correlation plot 
corrplot(traf_no2_1)




table(traf$captured_datetime)


tiff("./figures/sample_google_color_map.tiff",
     units = "in", width = 12, height = 7, res = 300)
traf_sf %>% filter(captured_datetime == '0121201530') %>% 
  pivot_longer(cols = gt_pixcount_maroon:gt_pixcount_gray,
               names_to = 'gt_color',
               values_to = 'count') %>% 
  mutate(gt_color = factor(str_sub(gt_color, start = 13),
                           levels = c('gray', 'green', 'orange', 'red', 'maroon'))) %>% 
  ggplot() + geom_sf(aes(geometry = geometry, fill = count/road_pixels)) +
  facet_wrap(~gt_color) + theme_void()
dev.off()
