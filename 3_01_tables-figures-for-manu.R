# Tables & Figures for Manuscript
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 03/30/2023

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
# In this script we 

# table 1: cols = pop, prepause traf, postpause traf, perc white, perc black, median hhincome
#          rows = 5 strata for ICE and 5 strata for EJI
# fig 1: traffic pre and post pause by census tract (2-panel map)
# fig 2: 2-panel chloropleth w ICE and EJI
# fig 3: time series of traffic
# fig 4: forrest plot of strata model results
# sup table 1: coefs and cis for fig 3
# fig 5 (if different from total, if not, goes in supp): forrest plot of eji modules
# sup table 2: coefs and cis for fig 4


####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages & passwords
source(paste0(project.folder, 'packages.R'))

# 0c Set up filepath(s)
data_path <- paste0(project.folder, 'data/processed_data/')
model_path <- paste0(project.folder, 'outputs/models/')
figure_path <- paste0(project.folder, 'outputs/figures/')
polygons_of_interest_path = here::here('data', 'nyc_census_tracts', 'nycgeo_census_tracts.shp')

# 0d Load data
fullData <- read_rds(paste0(data_path, 'full_dataset_wcovars_daily.rds'))
tracts_sf <- st_read(polygons_of_interest_path)


####***********************************
#### 1: Prepare Manuscript Table 1 #### 
####***********************************

# 1a Filter dataset to exclude Phase 1 reopening and beyond
#    Note: NYC entered Phase 1 reopening on June 8, 2020
fullDataF <- fullData %>% filter(date < '2020-06-08')

# 1b Nest by strata
# 1b.i ICE HH Income and BW Race, 5 quantiles
fullDataS <- fullDataF %>% group_by(ice_hhincome_bw_5) %>% nest() %>% 
  rename(strata = ice_hhincome_bw_5) %>% 
  mutate(strata = case_when(
    strata == 'Q1' ~ 'iceHhincomeBwQ1', strata == 'Q2' ~ 'iceHhincomeBwQ2',
    strata == 'Q3' ~ 'iceHhincomeBwQ3', strata == 'Q4' ~ 'iceHhincomeBwQ4',
    strata == 'Q5' ~ 'iceHhincomeBwQ5'))
# 1b.ii EJI, 5 quantiles
fullDataS.2 <- fullDataF %>% group_by(eji_5) %>% nest() %>% 
  rename(strata = eji_5) %>% 
  mutate(strata = case_when(
    strata == 'Q1' ~ 'ejiQ1', strata == 'Q2' ~ 'ejiQ2',
    strata == 'Q3' ~ 'ejiQ3', strata == 'Q4' ~ 'ejiQ4',
    strata == 'Q5' ~ 'ejiQ5'))

# 1c Bind together
fullDataS <- fullDataS %>% 
  bind_rows(fullDataS.2) %>% 
  filter(!is.na(strata))

# 1d Set strata to factor with assigned levels
fullDataS <- fullDataS %>% 
  mutate(strata = factor(strata, levels = c('iceHhincomeBwQ1', 'iceHhincomeBwQ2',
                                            'iceHhincomeBwQ3', 'iceHhincomeBwQ4',
                                             'iceHhincomeBwQ5', 'ejiQ1', 'ejiQ2',
                                             'ejiQ3', 'ejiQ4', 'ejiQ5'))) %>% 
  arrange(strata)

# 1d Clean environment
rm(fullDataF, fullDataS.2)

# 1e Calculate column values for mean and SD (or median and IQR) of census vars
#    population, perc white, perc black
t1_census <- fullDataS %>% 
  mutate(num_CTs = map(.x = data, ~ n_distinct(.x$poly_id)),
         mean_pop = map(.x = data, ~ mean(.x$all_race_est, na.rm = T)),
         sd_pop = map(.x = data, ~ sd(.x$all_race_est, na.rm = T)),
         mean_pwhite = map(.x = data, ~ (mean(.x$nhwhite_race_est/.x$all_race_est, na.rm = T)*100)),
         sd_pwhite = map(.x = data, ~ (sd(.x$nhwhite_race_est/.x$all_race_est, na.rm = T)*100)),
         mean_pblack = map(.x = data, ~ (mean(.x$nhblack_race_est/.x$all_race_est, na.rm = T)*100)),
         sd_pblack = map(.x = data, ~ (sd(.x$nhblack_race_est/.x$all_race_est, na.rm = T)*100))) %>% 
  dplyr::select(-data) %>% 
  mutate(across(where(is.list), ~ as.numeric(.x)),
         across(where(is.numeric), ~ round(.x, digits = 1))) %>% 
  arrange(strata)

# 1f Calculate column values for traffic pre and post pause
# 1f.i Make function to calculate mean and sd traf values pre and post pause
aggTrafByPause = function(df){
  df <- df %>% group_by(pause) %>% 
    summarise(mean_propGreen = round(mean(prop_green, na.rm = T), digits = 1),
              sd_propGreen = round(sd(prop_green, na.rm = T), digits = 1))
}
# 1f.ii Run function and convert df to table-like format
t1_traf <- fullDataS %>% 
  mutate(agg_traf = map(.x = data, ~ aggTrafByPause(.x))) %>% 
  dplyr::select(-data) %>% unnest(agg_traf) %>% 
  pivot_wider(names_from = pause,
              values_from = c(mean_propGreen, sd_propGreen)) %>% 
  janitor::clean_names() %>% 
  dplyr::select(strata, mean_prop_green_pre_pause, sd_prop_green_pre_pause,
                mean_prop_green_pause, sd_prop_green_pause) %>% 
  arrange(strata)


####*********************************
#### 2: Prepare Manuscript Fig 1 #### 
####*********************************

# Two-panel chloropleth map showing traffic on a day before and a day after
# implementation of NY on Pause

# 2a Determine min and max of two dates for plotting
#    Note: min = 12.9; max = 93.7
fullData %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% ungroup() %>% 
  filter(date == '2020-03-09') %>% 
  summarise(max = max(prop_green, na.rm = T), min = min(prop_green, na.rm = T))
fullData %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% ungroup() %>% 
  filter(date == '2020-03-23') %>% 
  summarise(max = max(prop_green, na.rm = T), min = min(prop_green, na.rm = T))

# 2b Create panel A - Monday before Pause was implemented
fig1_a <- 
  fullData %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% 
  filter(date == '2020-03-09') %>% 
  ggplot(aes(geometry = geometry, fill = prop_green)) + 
  geom_sf(lwd = 0.2) + 
  annotate('text', x = 985000, y = 255000, label = 'A: 3/09/2020', size = 16/.pt) +
  scale_fill_viridis(name = 'Perc. Streets w \n Free-flowing Traffic', 
                     option = 'turbo', direction = -1,
                     breaks = c(20, 40, 60, 80),
                     limits = c(12, 94)) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
fig1_a

# 2c Create panel B - Monday after Pause was implemented
fig1_b <- 
  fullData %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% 
  filter(date == '2020-03-23') %>% 
  ggplot(aes(geometry = geometry, fill = prop_green)) + 
  geom_sf(lwd = 0.2) + 
  annotate('text', x = 985000, y = 255000, label = 'B: 3/23/2020', size = 16/.pt) +
  scale_fill_viridis(name = 'Perc. Streets w \n Free-flowing Traffic', 
                     option = 'turbo', direction = -1,
                     breaks = c(20, 40, 60, 80),
                     limits = c(12, 94)) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
fig1_b

# 2d Combine panels into one plot and save
tiff(paste0(figure_path, 'fig1_PrePostPauseChloroMap.tiff'),
     units = "in", width = 14, height = 8, res = 300)
plot_grid(fig1_a, fig1_b)
dev.off()


####*********************************
#### 3: Prepare Manuscript Fig 2 #### 
####*********************************

# Two-panel chloropleth map showing ICE as one panel and EJI as the other
# We filter to a single day only for computation efficiency; ICE and EJI are 
# the same no matter what day is selected.

# 3a Determine min and max of ICE var for plotting
#    Note: min = -0.38; max = 0.65
min(fullData$ice_hhincome_bw, na.rm = T)
max(fullData$ice_hhincome_bw, na.rm = T)

# 3b Create panel A - ICE
fig2_a <- 
  fullData %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% 
  mutate(ice_hhincome_bw_5 = factor(ice_hhincome_bw_5, levels = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5'))) %>%
  filter(date == '2020-03-09') %>% 
  ggplot(aes(geometry = geometry, fill = ice_hhincome_bw_5)) + 
  geom_sf(lwd = 0.2) + 
  scale_fill_viridis(name = 'Index of Concentration \n at the Extremes', 
                     option = 'inferno',
                     discrete = T,
                     labels = c('Q1 (Disadvantaged)', 'Q2', 'Q3', 'Q4',
                                'Q5 (Privileged)', 'Not Enough Pop.')) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
fig2_a

# 2c Create panel B - EJI
fig2_b <- 
  fullData %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% 
  mutate(eji_5 = factor(eji_5, levels = c('Q5', 'Q4', 'Q3', 'Q2', 'Q1'))) %>% 
  filter(date == '2020-03-09') %>% 
  ggplot(aes(geometry = geometry, fill = eji_5)) + 
  geom_sf(lwd = 0.2) + 
  scale_fill_viridis(name = 'Environmental \n Justice Index', 
                     option = 'viridis',
                     discrete = T,
                     labels = c('Q5 (High Burden)', 'Q4', 'Q3', 'Q2', 
                                'Q1 (Low Burden)', 'Not Enough Data')) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
fig2_b

# 2d Combine panels into one plot and save
tiff(paste0(figure_path, 'fig2_IceEjiChloroMap.tiff'),
     units = "in", width = 14, height = 8, res = 300)
plot_grid(fig2_a, fig2_b, labels = 'AUTO')
dev.off()





