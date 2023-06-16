# Create traffic measures
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 03/16/2023

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Review traffic data
# 2: Restrict to sampled census tracts not on edge of capture area
# 3: Create traffic congestion measures
# 4: Evaluate correlations between traffic congestion measures
# 5: Save out new traffic data

####**************
#### N: Notes ####
####**************

# Na Description
# In this script, we first investigate missingness in the traffic data. Then, we 
# create multiple measures of traffic at the census tract level and compare them.
# We start with five measures of traffic, corresponding to the count of pixels assigned
# to each congestion color code for each census tract (maroon, red, orange, green, grey),
# and create different traffic and congestion metrics. Ultimately, we want to choose
# a single measure of traffic for each census tract for use in the analyses.

####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages & passwords
source(paste0(project.folder, 'packages.R'))
source(paste0(project.folder, 'passwords.R'))
source(here::here('functions', 'timeseries3_make_captured_datetime_vector.R'))

# 0c Set up filepath(s)
data_path <- paste0(project.folder, 'data/processed_data/')
polygons_of_interest_path = here::here('data', 'nyc_census_tracts', 'nycgeo_census_tracts.shp')
extent_path = here::here('data', 'needed_for_gt_to_polygons_function', 'gt_extent.RData')
gt_dir = '/Users/jennishearston/Dropbox/JENNI_F31_DATA'

# 0b Load data
# 0b.i Load traffic data
#      Note: 634 census tracts
traf18 <- read_fst(paste0(data_path, 'gt2018_2010CTs.fst'))
traf19 <- read_fst(paste0(data_path, 'gt2019_2010CTs.fst')) 
traf20 <- read_fst(paste0(data_path, 'gt2020_2010CTs.fst')) 
# 0b.ii Load census tract sf file
tracts_sf <- st_read(polygons_of_interest_path)
# 0b.iii Load gt capture area extent
load(extent_path)
# 0b.iv Load street network image and poly_matrix from script 1_01
streets_matrix <- png::readPNG(here::here(gt_dir, 'gt_street_network.png'))
poly_matrix <- readRDS(file = here::here('data', 'needed_for_gt_to_polygons_function', 
                                         'poly_matrix_nyc_2010CTs.rds'))

# 0c Merge three years of traffic data & add var for datetimes missing traffic image
#    Remove duplicate rows for when Daylight Savings Time ends
traf<- traf18 %>% bind_rows(traf19) %>% bind_rows(traf20) %>% 
  distinct() %>% 
  mutate(poly_id = as.character(poly_id),
         captured_datetime = mdy_hm(captured_datetime, tz = 'America/New_York'),
         no_image = ifelse(is.na(poly_id) & is.na(gt_pixcount_tot), 'no image', 'image')) 

# 0d Merge traffic data with sf of tract boundaries for visualization
traf_sf <- traf %>% left_join(tracts_sf, by = c('poly_id' = 'geoid'))

# 0e Remove no longer needed objects
rm(traf18, traf19, traf20)

####****************************
#### 1: Review traffic data #### 
####****************************

# 1a Run summary statistics to review data & create var and df for images not present
#    Notes: n = 2393 datetimes with no traffic data (NA for polyid & all pixcount 
#           vars indicates no image)
summary(traf)
numDTsWNoImage <- nrow(traf[is.na(traf$poly_id),])
DTsWNoImage <- traf[is.na(traf$poly_id),]

# 1b Compare full datetime vector to captured_datetime in dataset to confirm
#    all dates are present
# 1b.i Create vector of all datetimes from 01/01/2018 - 12/31/2020
allDates <- make_captured_datetime_vector(
  base_date = '2018/01/01 00:00', end_date = '2020/12/31 23:00',
  sampling_quantity_units_direction = 'none', timezone = 'America/New_York')
# 1b.ii Identify all unique datetimes in traffic data
trafDates <- unique(traf$captured_datetime)
# 1b.iii Pull datetimes not present in traffic
#    Notes: The three missing dates are the EDT instances of 1 am when the 
#           transition from EDT to EST happened (ending of Daylight Savings)
#           It makes since they are missing, since when we reformat the datetime 
#           to match the filenames of the traffic images, we use the hour function
#           which creates an integer and does not keep timezones. So only one 1 am
#           hour would have been created in the reformatted datetime vector. This
#           is fine for our purposes, as only one traffic image was recorded at 
#           1 am during the switch from EDT to EST
trafDatesMis <- allDates[!allDates %in% trafDates]
# 1b.iv Remove three 1am EDT observations from datetime vector
allDates <- allDates[allDates %in% trafDates]

# 1c Determine possible number of datetimes per CT after removing datetimes w no image
#    n = 23,908 datetimes per CT possible
DTperCT <- length(allDates) - numDTsWNoImage

# 1d Create vector of original CTs in traffic data and remove NA (n = 634)
ctOrig_vec <- unique(traf$poly_id)
ctOrig_vec <- ctOrig_vec[!is.na(ctOrig_vec)]

# 1e Identify census tracts on edge of image 
# 1e.i Convert extent object to SpatialLines and then sf & set CRS
#      Note: Must go in this order as function won't allow extent to sf directly
gtBounds_sp <- as(gt_extent, 'SpatialLines')
gtBounds_sf <- as(gtBounds_sp, 'sf')
gtBounds_sf <- sf::st_set_crs(gtBounds_sf, 'WGS84')
plot(gtBounds_sf)
# 1e.ii Transform tracts_sf to WGS84
tracts_sf <- sf::st_transform(tracts_sf, 'WGS84')
plot(tracts_sf, add = T)
plot(gtBounds_sf, add = T)
# 1e.iii Intersect census tracts with capture area bounding box to id CTs on 
#        edge of image
tractsCrossingBounds <- tracts_sf[gtBounds_sf,]
plot(tractsCrossingBounds$geometry)
plot(gtBounds_sf, add = T)
# 1e.iv Create vector of CTs partially outside the capture area (n = 70)
ctOut_vec <- unique(tractsCrossingBounds$geoid)
# 1e.v Manually specify to keep tracts that are very close to 100% inside the area
#      (Not all tracks need to be manually specified in this way, depending on
#      whether 2010 or 2020 CTs are being used)
#      -Tracts chosen through manual inspection of maps, accounting for roads included
#      and population
#      -Keep tract 36061000700, as all parts where people live or can drive are
#      inside the capture area (a pier containing the Pier 2 Roller Rink across
#      the East River was outside the capture area, causing the tract to be dropped)
#      -Keep tract 36061000900, as all parts where people live or can drive are
#      inside the capture area (Piers 3 and 5 containing parks across
#      the East River were outside the capture area, causing the tract to be dropped)
#      -Also keep tract 36061029700 which includes Inwood Hill Park. All but the
#      absolute top of the tract is inside the capture area, although the pop is
#      low and no household income (so will be dropped from ICE analysis), it will
#      make maps look better visually and contains a major road
ctOut_vec <- ctOut_vec[!ctOut_vec %in% c('36061000700', '36061029700', 
                                         '36061000900')] # now n = 69

# 1f Create vector of CTs fully inside capture area (n = 565)
ctIn_vec <- ctOrig_vec[!ctOrig_vec %in% ctOut_vec]

# 1g Determine numb and prop of census tracts that have 100% coverage, 95%, 90%,
#    85%, 80%
#    Note: Include all census tracts that are fully inside the capture area
#          Should have n = 565
#          There were only 23,908 images collected (due to computer power outage
#          or at least one tile not being downloaded)
# 1g.i Create coverage dataframe
coverage <- traf %>% 
  filter(poly_id %in% ctIn_vec) %>% 
  mutate(percCoverage = 100 - (((gt_pixcount_notsampled)
                                / gt_pixcount_tot)*100),
         obsW100 = ifelse(percCoverage == 100, 1, 0),
         obsW95 = ifelse(percCoverage > 94.5, 1, 0),
         obsW90 = ifelse(percCoverage > 89.5, 1, 0),
         obsW85 = ifelse(percCoverage > 84.5, 1, 0),
         obsW80 = ifelse(percCoverage > 79.5, 1, 0)) %>% 
  group_by(poly_id) %>% 
  summarise(numObsW100 = sum(obsW100),
            numObsW95 = sum(obsW95),
            numObsW90 = sum(obsW90),
            numObsW85 = sum(obsW85),
            numObsW80 = sum(obsW80)) %>% 
  mutate(cov9595 = factor(ifelse((numObsW95/DTperCT) > 0.945, 1, 0))) %>% 
  filter(!is.na(poly_id)) # remove NA poly_id; these occur when datetimes have no traffic image
# 1g.ii Confirm with plot
coverage %>% left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% 
  ggplot(aes(fill = cov9595, geometry = geometry)) + 
  geom_sf() + 
  scale_fill_viridis("CT with 95%+ of Pixels Sampled for \n 95%+ of available Datetimes",
                     discrete = TRUE) + 
  theme_void() 

# 1h Create vector of CTs fully inside capture area & with 95%+ of 
#    Pixels Sampled for 95%+ of available Datetimes (n = 445, 79% of CTs in capture area)
ctInSampled_vec <- coverage %>% filter(cov9595 == 1) %>% dplyr::select(poly_id)
ctInSampled_vec <- as.vector(ctInSampled_vec$poly_id)

# 1i Determine number of census tracts with CCC data for each datetime 
#    (restricted to tracts fully inside capture area)
# 1i.i Create tibble with unique datetimes as rows 
mis <- tibble(datetime = allDates)
# 1i.ii Create variable of count of CTs with CCC data for each datetime 
#       Note: In script 1_01 (function timeseries2) datetimes with no 
#             corresponding traffic images were assigned NA for all vars; here
#             we assign them to 0 for CCC_present as there is no data
numCTsWData <- traf %>% 
  mutate(CCC_present = case_when(
    (gt_pixcount_maroon + gt_pixcount_red + gt_pixcount_orange + 
      gt_pixcount_green + gt_pixcount_gray + gt_pixcount_emergency + 
      gt_pixcount_construction) >= 1 & poly_id %in% ctInSampled_vec ~ 1,
    (gt_pixcount_maroon + gt_pixcount_red + gt_pixcount_orange + 
       gt_pixcount_green + gt_pixcount_gray + gt_pixcount_emergency + 
       gt_pixcount_construction) >= 1 & !poly_id %in% ctInSampled_vec ~ 0,
    (gt_pixcount_maroon + gt_pixcount_red + gt_pixcount_orange + 
      gt_pixcount_green + gt_pixcount_gray + gt_pixcount_emergency + 
      gt_pixcount_construction) < 1 ~ 0,
    is.na(poly_id) & is.na(gt_pixcount_tot) ~ 0
    )) %>% 
  group_by(captured_datetime) %>% 
  summarise(numCTsWData = sum(CCC_present))
# 1i.iv Merge count variable with datetime tibble
#       Max num of CTs w data should be n = 445 
#       If doing 2020 CTs, note that one CT does not
#         have any roads - it is North Brother Island 36005001904
mis <- mis %>% left_join(numCTsWData, by = c('datetime' = 'captured_datetime'))
# 1i.vi Create variable of percent sampled CTs with CCC data for each datetime
mis <- mis %>% mutate(perCTsWData = numCTsWData/length(ctInSampled_vec)*100)
# 1i.vii Review mis dataframe
summary(mis)
# 1i.viii Determine num and proportion datetimes with no CCC data
nrow(mis[mis$numCTsWData == 0,]) # n = 2,393
nrow(mis[mis$numCTsWData == 0,])/length(allDates) # proportion = 9.1%
# 1i.ix Review distribution
mis %>% ggplot() + geom_boxplot(aes(y = perCTsWData))

# 1j Determine if missingness in datetimes is differential before and after
#    stay at home orders began
#    Notes: There is sig. more missingness in the pre-pause period
# 1j.i Create stay-at-home order, month, and dow variables
dif_mis <- traf %>% mutate(pause = ifelse(captured_datetime > '2020-03-22 00:00', 
                                       'pause', 'pre-pause'),
                           dow = wday(captured_datetime),
                           month = month(captured_datetime))
# 1j.ii Run proportion table
round(prop.table(table(dif_mis$pause, dif_mis$no_image)),digits = 4)
# 1j.iii Regress missingness on pause 
#        Note: significantly different
mis_regress <- glm(factor(pause) ~ no_image, data = dif_mis, family = binomial(link = 'logit'))
summary(mis_regress) 
# 1j.iv Regress missingness on day of week
#       Note: not significant
mis_regress2 <- glm(factor(dow) ~ no_image, data = dif_mis, family = binomial(link = 'logit'))
summary(mis_regress2)
# 1j.v Regress missingness on month
#      Note: significant
mis_regress3 <- glm(factor(month) ~ no_image, data = dif_mis, family = binomial(link = 'logit'))
summary(mis_regress3)

# 1k Look at missingness over time
#    Note: It might be concerning if there is more missingness in the early days
#          of the pandemic
traf %>% filter(no_image == 'no image') %>% 
  ggplot(aes(x = captured_datetime, y = no_image)) +
  geom_point(fill = NA, alpha = 0.25) +
  geom_vline(xintercept = ymd_hm('2020-03-22 00:00')) + 
  geom_vline(xintercept = ymd_hm('2020-06-08 00:00'), color = 'red')

####**********************************************************************
#### 2: Restrict to sampled census tracts not on edge of capture area #### 
####**********************************************************************

# 2a Restrict to tracts not on edge of capture area
# 2a.i Restrict 
traf <- traf %>% filter((poly_id %in% ctIn_vec) | is.na(poly_id))
# 2a.ii Confirm n of unique CTs is correct and that datetimes missing images are preserved
length(ctOrig_vec) - length(ctOut_vec)
length(unique(traf$poly_id)) # 1 greater than line above because poly_id == NA is being counted
sum(is.na(traf$poly_id)) # should be n = 2,393

# 2b Restrict to tracts with 95% of pixels sampled 95% of the time
# 2b.i Restrict
traf <- traf %>% filter((poly_id %in% ctInSampled_vec) | is.na(poly_id))
# 2b.ii Confirm n of unique CTs is correct and that datetimes missing images are preserved
length(ctOrig_vec) - length(coverage$cov9595[coverage$cov9595 == 0]) - length(ctOut_vec)
length(unique(traf$poly_id)) # 1 greater than line above because poly_id == NA is being counted
sum(is.na(traf$poly_id)) # should be n =2,393

# 2c Plot to confirm
traf %>% left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% 
  filter(captured_datetime == '2018-01-01 00:00') %>% 
  ggplot(aes(geometry = geometry)) + 
  geom_sf() + 
  theme_void() 

# Notes: Original # CTs = 634
#        Remove CTs on edge of capture area (n = 69) = 565
#        Remove CTs with less than 95%+ of pixels sampled 
#          for 95%+ of available Datetimes (n = 120) = 445

#       n = 10,641,453 observations: 
#       ((26301 possible datetimes - 2393 missing datetimes) 
#       * 445 CTs) + 2393 missing datetimes

####*******************************************
#### 3: Create traffic congestion measures #### 
####*******************************************

# 3a Create road pixels variable from street network file used in image processing
#    Note: This variable will be the same for all obs of a given CT
#          The street network image is the same dimensions and resolution as the 
#            traffic map images, and can be converted to a df following code from
#            the function timeseries2_get_gt_agg_timepoint.R
# 3a.i Create df of street network by merging poly_matrix and streets_matrix based
#       on pixel position and then performing a grouped summarise where the group
#       is the census tract identifier
streets_df <- 
  data.frame(poly_id = as.vector(poly_matrix),
             gt_streets = as.vector(streets_matrix)) %>% 
  dplyr::group_by(poly_id) %>% 
  dplyr::summarize(gt_pixcount_notstreets       = sum(gt_streets == 1),
                   gt_pixcount_streets          = sum(gt_streets == 0)) %>% 
  dplyr::select(poly_id, everything()) %>% 
  dplyr::filter(!is.na(poly_id)) %>% mutate(poly_id = as.character(poly_id))
# 3a.ii Plot to confirm
#       Notes: looks about right
streets_df %>% mutate(tot = gt_pixcount_notstreets+gt_pixcount_streets,
                      perc_streets = (gt_pixcount_streets/tot)*100) %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% 
  ggplot(aes(geometry = geometry)) + 
  geom_sf(aes(fill = perc_streets)) + 
  scale_fill_viridis_c(option = "magma")+
  theme_void() 
# 3a.iii Add road pixel var to traffic df
traf <- traf %>% left_join(streets_df, by = 'poly_id')

# 3b Review distribution of five congestion colors
#    Notes: All distributions are heavily right skewed except for green, which
#           is nearly perfectly normal
traf %>% ggplot(aes(x = gt_pixcount_maroon/gt_pixcount_streets)) + geom_histogram()
traf %>% ggplot(aes(x = gt_pixcount_red/gt_pixcount_streets)) + geom_histogram()
traf %>% ggplot(aes(x = gt_pixcount_orange/gt_pixcount_streets)) + geom_histogram()
traf %>% ggplot(aes(x = gt_pixcount_green/gt_pixcount_streets)) + geom_histogram()
traf %>% ggplot(aes(x = gt_pixcount_gray/gt_pixcount_streets)) + geom_histogram()

# 3c Create variables representing the proportion of roads red + maroon for each 
#    CT/datetime, and proportion of roads green for each CT/datetime
traf <- traf %>%
  mutate(prop_maroon_red = ((gt_pixcount_maroon + gt_pixcount_red) / gt_pixcount_streets) * 100,
         prop_green = (gt_pixcount_green / gt_pixcount_streets) * 100) %>% 
  dplyr::select(poly_id, captured_datetime, gt_pixcount_streets, prop_maroon_red,
                prop_green, everything())

# 3d Create a free-flow variable defined as the 85th percentile of the number of 
#    green + gray pixels during off-peak time (weeknights 10-5) for a census tract
free_flow <- traf %>% dplyr::select(poly_id, captured_datetime, 
                                       gt_pixcount_green, gt_pixcount_gray) %>% 
  mutate(hour_of_day = lubridate::hour(captured_datetime)) %>% 
  filter(hour_of_day > 21 | hour_of_day < 06) %>% 
  mutate(green_gray = gt_pixcount_green + gt_pixcount_gray) %>% 
  group_by(poly_id) %>% 
  mutate(green_gray_85 = quantile(green_gray, c(.85), na.rm = T)) %>% 
  ungroup() %>% 
  dplyr::select(poly_id, green_gray_85) %>% distinct()

# 3e Create speed reduction factor and congestion variable based on the free-flow
#    variable defined in 1b
traf <- traf %>% 
  left_join(free_flow, by = 'poly_id') %>% 
  mutate(speed_reduct_fact = ((gt_pixcount_green + gt_pixcount_gray) / green_gray_85)*100,
         congest = case_when(
           speed_reduct_fact >= 80 ~ 'no to low congestion',
           speed_reduct_fact >= 65 & speed_reduct_fact < 80 ~ 'mod congestion',
           speed_reduct_fact < 65 ~ 'severe congestion'
         )) %>% 
  dplyr::select(poly_id, captured_datetime, congest, speed_reduct_fact, 
                green_gray_85, everything())

# 3f Use the index of concentration at the extremes to create one traffic variable
#    ICEi = (Ai-Pi)/Ti
traf <- traf %>% 
  mutate(ice_gt = (gt_pixcount_green - gt_pixcount_maroon) / gt_pixcount_streets, 
         ice_gt = round(ice_gt, digits = 2))

# 3g Check variability of new vars
#    Note: these plots take a long time to load
traf %>% ggplot(aes(x = prop_maroon_red)) + geom_histogram() + 
  geom_vline(aes(xintercept = mean(prop_maroon_red, na.rm = T))) # right skewed
traf %>% ggplot(aes(x = prop_green)) + geom_histogram() +
  geom_vline(aes(xintercept = mean(prop_green, na.rm = T))) # normal
traf %>% ggplot(aes(x = speed_reduct_fact)) + geom_histogram() +
  geom_vline(aes(xintercept = mean(speed_reduct_fact, na.rm = T))) # truncated
traf %>% ggplot(aes(x = ice_gt)) + geom_histogram() +
  geom_vline(aes(xintercept = mean(ice_gt, na.rm = T)))  # normal
traf %>% ggplot(aes(x = congest)) + geom_bar()

####******************************************************************
#### 4: Evaluate correlations between traffic congestion measures #### 
####******************************************************************

# 4a Select variables to include in correlation matrix 
traf_cors <- traf %>% 
  dplyr::select(ice_gt, speed_reduct_fact, green_gray_85, prop_green,
                prop_maroon_red, gt_pixcount_maroon, gt_pixcount_red,
                gt_pixcount_orange, gt_pixcount_green, gt_pixcount_gray) 

# 4b Run correlations
traf_cors1 <- cor(traf_cors, method = c('spearman'), use = 'complete.obs')
traf_cors2 <- rcorr(as.matrix(traf_cors))
traf_cors2_coeff <- traf_cors2$r
traf_cors2_p <- traf_cors2$P

# 4c Create correlation plot 
corrplot(traf_cors1, type = c('lower'))

# 4d Convert from wide to long to make scatterplots
traf_scatter <- traf %>% 
  dplyr::select(poly_id, captured_datetime,
                ice_gt, speed_reduct_fact, green_gray_85, prop_green,
                prop_maroon_red, gt_pixcount_maroon, gt_pixcount_red,
                gt_pixcount_orange, gt_pixcount_green, gt_pixcount_gray) %>% 
  pivot_longer(cols = gt_pixcount_maroon:gt_pixcount_gray,
               names_to = 'gt_color',
               values_to = 'gt_value')

# 4e Scatterplots of each metric against original gt_pixcount values w Loess curves
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

####**********************************
#### 5: Save out new traffic data #### 
####**********************************

# 5a Traffic data
traf %>% write_fst(paste0(data_path, 'gt18-20_2010CTs.fst'))

# 5b List of polyids in traffic data
ctInSampled_vec %>% write_rds(paste0(data_path, 'gt_polyids_2010CTs.rds'))










