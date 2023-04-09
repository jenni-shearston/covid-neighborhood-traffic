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

# A note about ICE and EJI quantiles
#       All quantiles are calculated such that Q1 corresponds to the lower values,
#       and Q5 corresponds to the higher values. However, for ICE higher values 
#       correspond to the most privileged group, while lower values correspond to
#       the most disadvantaged group. In contrast for EJI, higher values correspond
#       to the most burdened group, while lower values correspond to the least
#       burdened group. For display purposes only, (3_01) the quantiles of EJI will
#       be reversed so that Q1 corresponds to the most burdened group.

# sup figure 1: choropleth map of EJI modules
# sup table 1: coefs and cis for forest plot(s)



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
mod_results <- read_csv(paste0(model_path, 'model_results_table.csv'))
load(here::here('data', 'needed_for_gt_to_polygons_function', 'gt_extent.RData'))

# 0e Create NYC inset map image
# 0e.i Convert both capture area extent and CT polygons to WGS 84 class sf
gtBounds_sp <- as(gt_extent, 'SpatialLines')
gtBounds_sf <- as(gtBounds_sp, 'sf')
gtBounds_sf <- sf::st_set_crs(gtBounds_sf, 'WGS84')
tracts_insert <- sf::st_transform(tracts_sf, 'WGS84')
# 0e.ii Create plot and save
jpeg(paste0(figure_path, 'nyc_inset.jpeg'),
     units = "cm", width = 4, height = 4, res = 300)
ggplot() + 
  geom_sf(aes(geometry = tracts_insert$geometry), lwd = 0.2, fill = 'white') +
  geom_sf(aes(geometry = gtBounds_sf$geometry), lwd = 0.4, fill = NA, color = 'red') +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"))
dev.off()
# 0e.iii Load new map as jpeg
inset <- jpeg::readJPEG(paste0(figure_path, 'nyc_inset.jpeg'), native = T)


####*********************************************************
#### 1: Prepare Descriptive Table for EJI/ICE Strata CTs #### 
####*********************************************************

## ADD FILTER STATEMENT DEPENDING ON WHAT 'MAIN' MODEL IS

# 1a Nest by strata
# 1a.i ICE HH Income and BW Race, 5 quantiles
fullDataS <- fullDataF %>% group_by(ice_hhincome_bw_5) %>% nest() %>% 
  rename(strata = ice_hhincome_bw_5) %>% 
  mutate(strata = case_when(
    strata == 'Q1' ~ 'iceHhincomeBwQ1', strata == 'Q2' ~ 'iceHhincomeBwQ2',
    strata == 'Q3' ~ 'iceHhincomeBwQ3', strata == 'Q4' ~ 'iceHhincomeBwQ4',
    strata == 'Q5' ~ 'iceHhincomeBwQ5'))
# 1a.ii EJI, 5 quantiles
fullDataS.2 <- fullDataF %>% group_by(eji_5) %>% nest() %>% 
  rename(strata = eji_5) %>% 
  mutate(strata = case_when(
    strata == 'Q1' ~ 'ejiQ1', strata == 'Q2' ~ 'ejiQ2',
    strata == 'Q3' ~ 'ejiQ3', strata == 'Q4' ~ 'ejiQ4',
    strata == 'Q5' ~ 'ejiQ5'))

# 1b Bind together
fullDataS <- fullDataS %>% 
  bind_rows(fullDataS.2) %>% 
  filter(!is.na(strata))

# 1c Set strata to factor with assigned levels
fullDataS <- fullDataS %>% 
  mutate(strata = factor(strata, levels = c('iceHhincomeBwQ1', 'iceHhincomeBwQ2',
                                            'iceHhincomeBwQ3', 'iceHhincomeBwQ4',
                                             'iceHhincomeBwQ5', 'ejiQ1', 'ejiQ2',
                                             'ejiQ3', 'ejiQ4', 'ejiQ5'))) %>% 
  arrange(strata)

# 1d Clean environment
rm(fullDataS.2)

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


####******************************************************
#### 2: Prepare Choropleth Traffic Pre/Post Pause Map #### 
####******************************************************

# Two-panel choropleth map showing traffic on a day before and a day after
# implementation of NY on Pause

# 2a Determine min and max of two dates for plotting
#    Note: min = 0; max = 34.1
fullData %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% ungroup() %>% 
  filter(date == '2020-03-09') %>% 
  summarise(max = max(prop_maroon_red, na.rm = T), min = min(prop_maroon_red, na.rm = T))
fullData %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% ungroup() %>% 
  filter(date == '2020-03-23') %>% 
  summarise(max = max(prop_maroon_red, na.rm = T), min = min(prop_maroon_red, na.rm = T))

# 2b Create panel A - Monday before Pause was implemented
fig1_a <- 
  fullData %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% 
  filter(date == '2020-03-09') %>% 
  ggplot(aes(geometry = geometry, fill = prop_maroon_red)) + 
  geom_sf(lwd = 0.2) + 
  annotate('text', x = 988000, y = 255000, label = 'A: 3/09/2020', size = 10/.pt,
           fontface = 2) +
  annotate('text', x = 988000, y = 240000, label = 'Manhattan', size = 9/.pt) +
  annotate('text', x = 1010000, y = 228000, label = 'Bronx', size = 9/.pt) +
  annotate('text', x = 1009000, y = 212000, label = 'Queens', size = 9/.pt) +
  annotate('text', x = 1009000, y = 199000, label = 'Brooklyn', size = 9/.pt) +
  scale_fill_viridis(name = '', 
                     option = 'viridis', 
                     breaks = c(10, 20, 30),
                     limits = c(0, 35)) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 10),
        legend.position = 'none')
fig1_a

# 2c Create panel B - Monday after Pause was implemented
fig1_b <- 
  fullData %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% 
  filter(date == '2020-03-23') %>% 
  ggplot(aes(geometry = geometry, fill = prop_maroon_red)) + 
  geom_sf(lwd = 0.2) + 
  annotate('text', x = 988000, y = 255000, label = 'B: 3/23/2020', size = 10/.pt,
           fontface = 2) +
  scale_fill_viridis(name = '% Streets w Traffic\nCongestion', 
                     option = 'viridis', 
                     breaks = c(10, 20, 30),
                     limits = c(0, 35)) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 10))
fig1_b

# 2d Combine panels into one plot and save
tiff(paste0(figure_path, 'fig1_PrePostPauseChoroMap.tiff'),
     units = "cm", width = 16, height = 9, res = 300)
plot_grid(fig1_a, fig1_b)
dev.off()


####*****************************************
#### 3: Prepare Choropleth EJI / ICE Map #### 
####*****************************************

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
  scale_fill_viridis(name = 'Index of Concentration\nat the Extremes', 
                     option = 'inferno',
                     discrete = T,
                     labels = c('Q1 (Disadvantaged)', 'Q2', 'Q3', 'Q4',
                                'Q5 (Privileged)', 'Not Enough Pop.')) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 10))
fig2_a

# 2c Create panel B - EJI
fig2_b <- 
  fullData %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% 
  mutate(eji_5 = factor(eji_5, levels = c('Q5', 'Q4', 'Q3', 'Q2', 'Q1'))) %>% 
  filter(date == '2020-03-09') %>% 
  ggplot(aes(geometry = geometry, fill = eji_5)) + 
  geom_sf(lwd = 0.2) + 
  scale_fill_viridis(name = 'Environmental\nJustice Index', 
                     option = 'viridis',
                     discrete = T,
                     labels = c('Q1 (High Burden)', 'Q2', 'Q3', 'Q3', 
                                'Q5 (Low Burden)', 'Not Enough Data')) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 10))
fig2_b

# 2d Combine panels into one plot and save
tiff(paste0(figure_path, 'fig2_IceEjiChoroMap.tiff'),
     units = "cm", width = 16, height = 9, res = 300)
plot_grid(fig2_a, fig2_b, labels = 'AUTO')
dev.off()


####*********************************
#### 4: Prepare Time Series Plot #### 
####*********************************

# One panel time series plot with EJI and ICE quintiles specified in color

# 4a Create function to aggregate traffic data (otherwise plot takes a long
#    time to load because of all the CTs for each date)
aggTrafByStrata = function(df){
  df <- df %>% group_by(date) %>% 
    summarise(mean_propMaroonRed = round(mean(prop_maroon_red, na.rm = T), digits = 1))
}

# 4b Aggregate traffic data to EJI and ICE quintiles
f3_traf_strata <- 
  fullDataS %>% 
  filter(str_detect(strata, 'iceHhincome|ejiQ')) %>% 
  mutate(agg_traf_strata = map(.x = data, ~ aggTrafByStrata(.x))) %>% 
  dplyr::select(-data) %>% unnest(agg_traf_strata) %>% 
  mutate(facet_label = ifelse(str_detect(strata, 'ejiQ'), 'EJI', 'ICE'),
         facet_label = factor(facet_label, levels = c('ICE', 'EJI'))) %>% 
  mutate(strata_label = strata,
         strata_label = str_replace(strata_label, 'eji', 'EJI '), 
         strata_label = str_replace(strata_label, 'ice', 'ICE '),
         strata_label = str_replace(strata_label, 'HhincomeBw', ''),
         strata_label = factor(strata_label, 
                                  levels = c('ICE Q1', 'ICE Q2', 'ICE Q3', 
                                             'ICE Q4', 'ICE Q5', 'EJI Q5',
                                             'EJI Q4', 'EJI Q3', 'EJI Q2',
                                             'EJI Q1'))) 

# 4c Create time series plot
fig3 <- f3_traf_strata %>% 
  ggplot(aes(x = date, y = mean_propMaroonRed, color = strata_label)) +
  geom_line(alpha = 0.25) +
  geom_vline(aes(xintercept = ymd("2020-03-22")), color = "black",
             linetype = "dashed") +
  annotate('text', x = ymd('2020-03-10'), y = 23, label = 'Pause\nBegins', 
           size = 8/.pt, fontface = 2, hjust = 1) +
  geom_vline(aes(xintercept = ymd('2020-06-08')), color = 'blue',
             linetype = 'dashed') +
  annotate('text', x = ymd('2020-06-20'), y = 23, label = 'Recovery\nBegins', 
           size = 8/.pt, fontface = 2, hjust = 0, color = 'blue') +
  geom_smooth(aes(color = strata_label)) +
  scale_color_viridis_d(name = '') + 
  facet_wrap(~ facet_label) +
  xlab('Date') + ylab('% Streets w Traffic Congestion') + 
  theme_bw() +
  theme(text = element_text(size = 10))
fig3

# 4d Save plot
tiff(paste0(figure_path, 'fig3_TrafficTimeseriesPlot.tiff'),
     units = "cm", width = 18, height = 9, res = 300)
fig3
dev.off()

####******************************************
#### 5: Prepare Model Results Forest Plot #### 
####******************************************

# 5a Clean strata names for plotting
mod_results1 <- mod_results %>% 
  filter(str_detect(model_identifier, 'includeRecovery')) %>% 
  filter(str_detect(model_identifier, 'propMaroonRed')) %>% 
  mutate(mod_label = as.character(model_identifier)) %>% 
  separate(col = mod_label, into = c('mod_label', NA, NA), sep = '_') %>% 
  mutate(strata_type = case_when(str_detect(model_identifier, 'ejiQ') ~ 'EJI',
                                 str_detect(model_identifier, 'ice') ~ 'ICE',
                                 str_detect(model_identifier, 'Ebm') ~ 'EBM',
                                 str_detect(model_identifier, 'Svm') ~ 'SVM',
                                 str_detect(model_identifier, 'Hvm') ~ 'HVM'),
         facet_type = case_when(str_detect(model_identifier, 'ice|ejiQ') ~ 'Main Analyses',
                                str_detect(model_identifier, 'Ebm|Svm|Hvm') ~ 'EJI Modules'), 
         mod_label = str_replace(mod_label, 'eji', 'EJI'),
         mod_label = str_replace(mod_label, 'EJIEbm', 'Env. Burden'),
         mod_label = str_replace(mod_label, 'EJIHvm', 'Health Vuln.'),
         mod_label = str_replace(mod_label, 'EJISvm', 'Social Vuln.'),
         mod_label = str_replace(mod_label, 'Q', ' Q'),
         mod_label = str_replace(mod_label, 'ice', 'ICE'),
         mod_label = str_replace(mod_label, 'HhincomeBw', ''),
         mod_label = factor(mod_label, 
                            levels = c('ICE Q1', 'ICE Q2', 'ICE Q3', 'ICE Q4', 'ICE Q5',
                                       'EJI Q1', 'EJI Q2', 'EJI Q3', 'EJI Q4', 'EJI Q5',
                                       'Env. Burden Q1', 'Env. Burden Q2',
                                       'Env. Burden Q3',
                                       'Health Vuln. Q1', 'Health Vuln. Q2',
                                       'Health Vuln. Q3',
                                       'Social Vuln. Q1', 'Social Vuln. Q2',
                                       'Social Vuln. Q3')),
         mod_label_ejiFlipped = case_when(
          mod_label == 'EJI Q1' ~ 'EJI Q5',
          mod_label == 'EJI Q2' ~ 'EJI Q4',
          mod_label == 'EJI Q4' ~ 'EJI Q2',
          mod_label == 'EJI Q5' ~ 'EJI Q1',
          mod_label == 'Env. Burden Q1' ~ 'Env. Burden Q3',
          mod_label == 'Env. Burden Q3' ~ 'Env. Burden Q1',
          mod_label == 'Health Vuln. Q1' ~ 'Health Vuln. Q3',
          mod_label == 'Health Vuln. Q3' ~ 'Health Vuln. Q1',
          mod_label == 'Social Vuln. Q1' ~ 'Social Vuln. Q3',
          mod_label == 'Social Vuln. Q3' ~ 'Social Vuln. Q1',
          TRUE ~ as.character(mod_label))) %>% 
  arrange(mod_label)

# 5b Add an 'order' variable to use for plotting the y axis. This variable will leave
#    two spaces between each strata on the y axis
mod_results1$order = seq(1,57,by=3)

# 5c Pivot so we can plot both coefficients 
mod_results1 <- mod_results1 %>%  
  pivot_longer(cols = coef_pause:uci_pauseEnd, names_sep = '_',
               names_to = c('limit', 'names')) %>% 
  pivot_wider(names_from = limit, values_from = value, names_repair = 'check_unique') 

# 5d Set up color palettes
forest_plot_color_palette <- c('dodgerblue', 'dodgerblue1', 
                               'dodgerblue2', 'dodgerblue3',
                               'dodgerblue4', 'red', 'red1',
                               'red2', 'red3', 'red4')
forest_plot_color_palette2 <- c('goldenrod1', 'goldenrod2', 
                                'goldenrod3', 'darkorange', 'darkorange1', 'darkorange2', 
                                'orangered', 'orangered2', 'orangered3')

# 5e Create forest plot of model results for main analyses
fig4_a <- mod_results1 %>% 
  filter(facet_type == 'Main Analyses') %>%
  ggplot(aes(x = coef, y = order, xmin = lci, xmax = uci,
             color = names, shape = names)) +
  #geom_point() +
  geom_pointrange(fatten = 1) +
  geom_vline(aes(xintercept = 0), linetype = 'solid') +
  scale_color_manual(values = c('black', 'blue')) +
  xlim(c(-8,1)) +
  scale_y_continuous(breaks = mod_results$order, 
                     labels = mod_results$mod_label_ejiFlipped,
                     limits = c(1,29)) +
  geom_hline(aes(yintercept = 14.5), linetype = 'dashed', color = 'gray60') +
  annotate('text', x = -6, y = 29, label = 'A: Main Analyses', size = 9/.pt) +
  xlab("Effect Estimate") + ylab("Strata") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.position = 'none')
fig4_a

# 5f Create forest plot of model results for EJI modules
fig4_b <- mod_results1 %>% 
  filter(facet_type == 'EJI Modules') %>% 
  ggplot(aes(x = coef, y = order, xmin = lci, xmax = uci,
             color = names, shape = names)) +
  #geom_point() +
  geom_pointrange(fatten = 1) +
  geom_vline(aes(xintercept = 0), linetype = 'solid') +
  scale_color_manual(values = c('black', 'blue')) +
  xlim(c(-8,1)) +
  scale_y_continuous(breaks = mod_results$order, 
                     labels = mod_results$mod_label_ejiFlipped,
                     limits = c(30,56)) +
  geom_hline(aes(yintercept = 38.5), linetype = 'dashed', color = 'gray60') +
  geom_hline(aes(yintercept = 47.5), linetype = 'dashed', color = 'gray60') +
  annotate('text', x = -6, y = 56, label = 'B: EJI Modules', size = 9/.pt) +
  xlab("Effect Estimate") + ylab("Strata") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.position = 'none')
fig4_b

# 5g Combine and save plot
tiff(paste0(figure_path, 'fig4_ModResultsPlot_propMaroonRed_includeRecoveryRelabeled.tiff'),
     units = "cm", width = 16, height = 9, res = 300)
plot_grid(fig4_a, fig4_b, rel_widths = c(1,1))
dev.off()


####*******************************************
#### 6: Prepare Map of EJI Module Tertiles #### 
####*******************************************

# 6a Prepare chloropleth map of EJI tertiles
ejiModuleTertsMap <- 
  fullData %>% 
  dplyr::select(poly_id, eji_ebm_3, eji_svm_3, eji_hvm_3, date) %>% 
  rename(EBM = eji_ebm_3, SVM = eji_svm_3, HVM = eji_hvm_3) %>% 
  filter(date == '2020-03-09') %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% 
  pivot_longer(cols = EBM:HVM,
               names_to = 'module',
               values_to = 'tertile') %>% 
  mutate(tertile = factor(tertile, levels = c('Q3', 'Q2', 'Q1'))) %>% 
  ggplot(aes(geometry = geometry, fill = tertile)) + 
  geom_sf(lwd = 0.2) + 
  facet_wrap(~module) +
  scale_fill_viridis(name = 'Environmental \n Justice Index',
                     option = 'viridis',
                     discrete = T,
                     labels = c('Q3 (High Burden)', 'Q2',
                                'Q1 (Low Burden)', 'Not Enough Data')) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
ejiModuleTertsMap

# 6b Save plot
tiff(paste0(figure_path, 'figX_ejiModuleChlorMap.tiff'),
     units = "in", width = 10, height = 7, res = 300)
ejiModuleTertsMap
dev.off()
