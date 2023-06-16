# Tables & Figures for Manuscript
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 03/30/2023

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Prepare Descriptive Table for EJI/ICE Strata CTs
# 2: Prepare Choropleth Traffic Pre/Post Pause Map
# 3: Prepare Choropleth EJI / ICE Map
# 4: Prepare Time Series Plot
# 5: Prepare Model Results Grouped Bar Chart
# 6: Prepare Model Results Bar Chart w Rush Hour Stratification

####**************
#### N: Notes ####
####**************

# Na Description
# In this script we prepare tables and figures for the main body of the 
# manuscript. 

# A note about ICE and EJI quantiles
#       All quantiles are calculated such that Q1 corresponds to the lower values,
#       and Q5 corresponds to the higher values. However, for ICE higher values 
#       correspond to the most privileged group, while lower values correspond to
#       the most disadvantaged group. In contrast for EJI, higher values correspond
#       to the most burdened group, while lower values correspond to the least
#       burdened group. For display purposes only, (3_01) the quantiles of EJI will
#       be reversed so that Q1 corresponds to the most burdened group.

####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages & passwords
source(paste0(project.folder, 'packages.R'))

# 0c Set up filepath(s)
data_path <- paste0(project.folder, 'data/processed_data/')
census_data_path <- paste0(project.folder, 'data/raw_data/census_data/')
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

# 1a Load and tidy NYC median household income data
nyc_medIncome <- read_csv(paste0(census_data_path, 
                                 "nhgis_med-income_tract_15-19.csv")) %>% 
  filter(GISJOIN != "GIS Join Match Code") %>% 
  dplyr::select(GISJOIN, STATEA, COUNTYA, TRACTA, ALW1E001, ALW1M001) %>% 
  rename(fips_code = GISJOIN, state_code = STATEA, county_code = COUNTYA, tract_code = TRACTA,
         med_hhincome_est = ALW1E001, med_hhincome_moe = ALW1M001) %>% 
  filter(state_code == "36") %>% 
  filter(county_code == "005" | county_code == "047" | county_code == "061" | 
           county_code == "081" | county_code == "085") %>% 
  mutate(med_hhincome_est = as.numeric(med_hhincome_est),
         med_hhincome_moe = as.numeric(med_hhincome_moe),
         poly_id = paste0(state_code, county_code, tract_code)) %>% 
  dplyr::select(poly_id, med_hhincome_est)

# 1b Merge household income data with full dataset
fullData_wHH <- fullData %>% left_join(nyc_medIncome, by = 'poly_id')

# 1c Nest by strata
# 1c.i ICE HH Income and BW Race, 5 quantiles
fullDataS <- fullData_wHH %>% group_by(ice_hhincome_bw_5) %>% nest() %>% 
  rename(strata = ice_hhincome_bw_5) %>% 
  mutate(strata = case_when(
    strata == 'Q1' ~ 'iceHhincomeBwQ1', strata == 'Q2' ~ 'iceHhincomeBwQ2',
    strata == 'Q3' ~ 'iceHhincomeBwQ3', strata == 'Q4' ~ 'iceHhincomeBwQ4',
    strata == 'Q5' ~ 'iceHhincomeBwQ5'))
# 1c.ii EJI, 5 quantiles
fullDataS.2 <- fullData_wHH %>% group_by(eji_5) %>% nest() %>% 
  rename(strata = eji_5) %>% 
  mutate(strata = case_when(
    strata == 'Q1' ~ 'ejiQ1', strata == 'Q2' ~ 'ejiQ2',
    strata == 'Q3' ~ 'ejiQ3', strata == 'Q4' ~ 'ejiQ4',
    strata == 'Q5' ~ 'ejiQ5'))

# 1d Bind together
fullDataS <- fullDataS %>% 
  bind_rows(fullDataS.2) %>% 
  filter(!is.na(strata))

# 1e Set strata to factor with assigned levels
fullDataS <- fullDataS %>% 
  mutate(strata = factor(strata, levels = c('iceHhincomeBwQ1', 'iceHhincomeBwQ2',
                                            'iceHhincomeBwQ3', 'iceHhincomeBwQ4',
                                             'iceHhincomeBwQ5', 'ejiQ1', 'ejiQ2',
                                             'ejiQ3', 'ejiQ4', 'ejiQ5'))) %>% 
  arrange(strata)

# 1f Clean environment
rm(fullDataS.2, fullData_wHH)

# 1g Calculate column values for mean and SD (or median and IQR) of census vars
#    population, perc white, perc black
t1_census <- fullDataS %>% 
  mutate(num_CTs = map(.x = data, ~ n_distinct(.x$poly_id)),
         mean_pop = map(.x = data, ~ mean(.x$all_race_est, na.rm = T)),
         sd_pop = map(.x = data, ~ sd(.x$all_race_est, na.rm = T)),
         mean_pwhite = map(.x = data, ~ (mean(.x$nhwhite_race_est/.x$all_race_est, na.rm = T)*100)),
         sd_pwhite = map(.x = data, ~ (sd(.x$nhwhite_race_est/.x$all_race_est, na.rm = T)*100)),
         mean_pblack = map(.x = data, ~ (mean(.x$nhblack_race_est/.x$all_race_est, na.rm = T)*100)),
         sd_pblack = map(.x = data, ~ (sd(.x$nhblack_race_est/.x$all_race_est, na.rm = T)*100)),
         med_hhincome = map(.x = data, ~ median(.x$med_hhincome_est, na.rm = T)),
         iqr_hhincome = map(.x = data, ~ IQR(.x$med_hhincome_est, na.rm = T))) %>% 
  dplyr::select(-data) %>% 
  mutate(across(where(is.list), ~ as.numeric(.x)),
         across(where(is.numeric), ~ round(.x, digits = 1)),
         mean_pop = round(mean_pop, digits = 0),
         sd_pop = round(sd_pop, digits = 0)) %>% 
  mutate(strata_label_ejiFlipped = case_when(
    strata == 'ejiQ1' ~ 'ejiQ5',
    strata == 'ejiQ2' ~ 'ejiQ4',
    strata == 'ejiQ4' ~ 'ejiQ2',
    strata == 'ejiQ5' ~ 'ejiQ1',
    TRUE ~ as.character(strata))) %>% 
  dplyr::select(strata, strata_label_ejiFlipped, everything()) %>% 
  arrange(strata_label_ejiFlipped)

# 1h Calculate column values for traffic pre and post pause
# 1h.i Make function to calculate mean and sd traf values pre and post pause
aggTrafByPause = function(df){
  df <- df %>% mutate(pause_combo = case_when(
    pause == 0 & pause_end == 0 ~ 'pre-pause',
    pause == 1 & pause_end == 0 ~ 'pause',
    pause == 0 & pause_end == 1 ~ 'recovery'
  )) %>% 
    group_by(pause_combo) %>% 
    summarise(mean_propMaroonRed = round(mean(prop_maroon_red, na.rm = T), digits = 1),
              sd_propMaroonRed = round(sd(prop_maroon_red, na.rm = T), digits = 1))
}
# 1h.ii Run function and convert df to table-like format
t1_traf <- fullDataS %>% 
  mutate(agg_traf = map(.x = data, ~ aggTrafByPause(.x))) %>% 
  dplyr::select(-data) %>% unnest(agg_traf) %>% 
  pivot_wider(names_from = pause_combo,
              values_from = c(mean_propMaroonRed, sd_propMaroonRed)) %>% 
  janitor::clean_names() %>% 
  mutate(strata_label_ejiFlipped = case_when(
    strata == 'ejiQ1' ~ 'ejiQ5',
    strata == 'ejiQ2' ~ 'ejiQ4',
    strata == 'ejiQ4' ~ 'ejiQ2',
    strata == 'ejiQ5' ~ 'ejiQ1',
    TRUE ~ as.character(strata))) %>% 
  dplyr::select(strata, strata_label_ejiFlipped, 
                mean_prop_maroon_red_pre_pause, sd_prop_maroon_red_pre_pause,
                mean_prop_maroon_red_pause, sd_prop_maroon_red_pause,
                mean_prop_maroon_red_recovery, sd_prop_maroon_red_recovery) %>% 
  arrange(strata_label_ejiFlipped)

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

# Two-panel choropleth map showing ICE as one panel and EJI as the other
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
                     option = 'rocket',
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
                     option = 'mako',
                     discrete = T,
                     labels = c('Q1 (High Burden)', 'Q2', 'Q3', 'Q4', 
                                'Q5 (Low Burden)', 'Not Enough Data')) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 10))
fig2_b

# 2d Combine panels into one plot and save
tiff(paste0(figure_path, 'fig2_IceEjiChoroMap.tiff'),
     units = "cm", width = 16, height = 9, res = 300)
patchwork_fig2 <- fig2_a + fig2_b
patchwork_fig2 + plot_annotation(tag_levels = 'A')
dev.off()

####*********************************
#### 4: Prepare Time Series Plot #### 
####*********************************

# Two panel time series plot with EJI and ICE quintiles specified in color

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

# 4c Create 10-color palette that combines the first five discrete colors from
#    rocket and the first five discrete colors from cividis, so that ICE and EJI
#    always uses the same color scheme
ice_palette <- as.character(rocket(5, alpha = 1, begin = 0, end = 1, direction = 1))
eji_palette <- as.character(mako(5, alpha = 1, begin = 0, end = 1, direction = 1))
combo_palette <- c(ice_palette, eji_palette)

line_list <- c('dashed', 'solid', 'solid', 'solid', 'solid',
               'dashed', 'solid', 'solid', 'solid', 'solid')

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
  geom_smooth(aes(color = strata_label, linetype = strata_label)) +
  scale_color_manual(values = combo_palette, name = '') + 
  scale_linetype_manual(values = line_list, name = '') +
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

####************************************************
#### 5: Prepare Model Results Grouped Bar Chart #### 
####************************************************

# Multi-panel grouped bar chart showing model results for main ICE and EJI
# analyses and secondary EJI module analyses. Each metric or domain is a different
# facet and effect estimates for Pause and Recovery period are differentiated by color.

# 5a Clean strata names & add any needed vars for plotting
mod_results1 <- mod_results %>% 
  filter(!str_detect(model_identifier, 'rh')) %>% 
  filter(!str_detect(model_identifier,'propGreen')) %>% 
  filter(!str_detect(model_identifier, 'main')) %>% 
  filter(!str_detect(model_identifier, 'Sens|sens')) %>% 
  filter(!str_detect(model_identifier, 'ModsAdjusted')) %>% 
  mutate(mod_label = as.character(model_identifier)) %>% 
  separate(col = mod_label, into = c('mod_label', NA, NA), sep = '_') %>% 
  mutate(Index = case_when(str_detect(model_identifier, 'ejiQ') ~ 'EJI',
                                 str_detect(model_identifier, 'ice') ~ 'ICE',
                                 str_detect(model_identifier, 'Ebm') ~ 'EBM',
                                 str_detect(model_identifier, 'Svm') ~ 'SVM',
                                 str_detect(model_identifier, 'Hvm') ~ 'HVM'),
         Index = factor(Index, levels = c('ICE', 'EJI', 'EBM', 'HVM', 'SVM')),
         Analysis = case_when(str_detect(model_identifier, 'ice|ejiQ') ~ 'Main Analyses',
                              str_detect(model_identifier, 'Ebm|Svm|Hvm') ~ 'EJI Modules'), 
         mod_label = str_replace(mod_label, 'eji', 'EJI'),
         mod_label = str_replace(mod_label, 'EJIEbmQ', 'Env. Burden T'),
         mod_label = str_replace(mod_label, 'EJIHvmQ', 'Health Vuln. T'),
         mod_label = str_replace(mod_label, 'EJISvmQ', 'Social Vuln. T'),
         mod_label = str_replace(mod_label, 'Q', ' Q'),
         mod_label = str_replace(mod_label, 'ice', 'ICE'),
         mod_label = str_replace(mod_label, 'HhincomeBw', ''),
         mod_label = factor(mod_label, 
                            levels = c('EJI Q1', 'EJI Q2', 'EJI Q3', 'EJI Q4', 'EJI Q5', 
                                       'ICE Q5', 'ICE Q4', 'ICE Q3', 'ICE Q2', 'ICE Q1',
                                       'Env. Burden T3', 'Env. Burden T2',
                                       'Env. Burden T1',
                                       'Health Vuln. T3', 'Health Vuln. T2',
                                       'Health Vuln. T1',
                                       'Social Vuln. T3', 'Social Vuln. T2',
                                       'Social Vuln. T1')),
         mod_label_ejiFlipped = case_when(
          mod_label == 'EJI Q1' ~ 'EJI Q5',
          mod_label == 'EJI Q2' ~ 'EJI Q4',
          mod_label == 'EJI Q4' ~ 'EJI Q2',
          mod_label == 'EJI Q5' ~ 'EJI Q1',
          mod_label == 'Env. Burden T1' ~ 'Env. Burden T3',
          mod_label == 'Env. Burden T3' ~ 'Env. Burden T1',
          mod_label == 'Health Vuln. T1' ~ 'Health Vuln. T3',
          mod_label == 'Health Vuln. T3' ~ 'Health Vuln. T1',
          mod_label == 'Social Vuln. T1' ~ 'Social Vuln. T3',
          mod_label == 'Social Vuln. T3' ~ 'Social Vuln. T1',
          TRUE ~ as.character(mod_label)),
         quintile_label = case_when(
           str_detect(mod_label_ejiFlipped, 'Q1') ~ 'Q1', str_detect(mod_label_ejiFlipped, 'Q2') ~ 'Q2',
           str_detect(mod_label_ejiFlipped, 'Q3') ~ 'Q3', str_detect(mod_label_ejiFlipped, 'Q4') ~ 'Q4',
           str_detect(mod_label_ejiFlipped, 'Q5') ~ 'Q5'),
         quintile_label = factor(quintile_label, levels = c('Q5', 'Q4', 'Q3', 'Q2', 'Q1')),
         tertile_label = case_when(
           str_detect(mod_label_ejiFlipped, 'T1') ~ 'T1',
           str_detect(mod_label_ejiFlipped, 'T2') ~ 'T2', str_detect(mod_label_ejiFlipped, 'T3') ~ 'T3'),
         tertile_label = factor(tertile_label, levels = c('T3', 'T2', 'T1'))) %>% 
  arrange(mod_label)

# 5b Pivot so we can plot both coefficients 
mod_results1 <- mod_results1 %>%  
  pivot_longer(cols = coef_pause:uci_pauseEnd, names_sep = '_',
               names_to = c('limit', 'Policy')) %>% 
  pivot_wider(names_from = limit, values_from = value, names_repair = 'check_unique') %>% 
  mutate(Policy = ifelse(Policy == 'pause', 'Pause', 'Recovery'))

# 5c Create grouped bar chart of model results for main analyses (ICE & EJI)
fig4_a <- mod_results1 %>% 
  filter(Analysis == 'Main Analyses') %>%
  ggplot(aes(y = quintile_label, x = coef, xmin = lci, xmax = uci,
             fill = Policy, group = Policy)) +
  geom_col(position = "dodge", width = .8) +
  geom_errorbar(position = "dodge", size = 1) +
  geom_vline(aes(xintercept = 0), linetype = 'solid') +
  scale_fill_manual(values = cividis(n = 2)) +  
  xlim(c(-8,0.5)) +
  facet_wrap(~Index, ncol = 1) +
  xlab("") + ylab("") + 
  theme_bw() +
  theme(text = element_text(size = 9.5), legend.position = 'top',
        axis.text.y = element_text(size = 10))
fig4_a

# 5d Create bar chart of model results for EJI sub-modules
fig4_b <- mod_results1 %>% 
  filter(Analysis == 'EJI Modules') %>%
  ggplot(aes(y = tertile_label, x = coef, xmin = lci, xmax = uci,
             fill = Policy, group = Policy)) +
  geom_col(position = "dodge", width = .8) +
  geom_errorbar(position = "dodge", size = 1) +
  geom_vline(aes(xintercept = 0), linetype = 'solid') +
  scale_fill_manual(values = cividis(n = 2)) +
  xlim(c(-8, 0.5)) +
  facet_wrap(~Index, ncol = 1) +
  xlab("") + ylab("") + 
  theme_bw() +
  theme(text = element_text(size = 9.5), legend.position = 'none',
        axis.text.y = element_text(size = 10))
fig4_b

# 5e Create x and y label variables for use in Grobs below
xlab = 'Decrease in % Traffic Congestion from Pre-Pause Trend'
ylab = 'Strata'
a_label = 'A'
b_label = 'B'

# 5f Combine and save plot
tiff(paste0(figure_path, 'fig4_ModResultsPlot_Main.tiff'),
     units = "cm", width = 16, height = 9, res = 300)
patchwork_fig4 <- fig4_a + fig4_b
patchwork_fig4
grid::grid.draw(grid::textGrob(xlab, x = 0.5,  y = 0.05, gp = grid::gpar(fontsize = 10)))
grid::grid.draw(grid::textGrob(ylab, x = 0.03,  rot = 90, gp = grid::gpar(fontsize = 10)))
grid::grid.draw(grid::textGrob(a_label, x = 0.105,  y = 0.84, gp = grid::gpar(fontsize = 11)))
grid::grid.draw(grid::textGrob(b_label, x = 0.585,  y = 0.84, gp = grid::gpar(fontsize = 11)))
dev.off()

####*******************************************************************
#### 6: Prepare Model Results Bar Chart w Rush Hour Stratification #### 
####*******************************************************************

# 6a Clean strata names for plotting
mod_results2 <- mod_results %>% 
  filter(str_detect(model_identifier, 'rh')) %>% 
  mutate(mod_label = as.character(model_identifier)) %>% 
  separate(col = mod_label, into = c('mod_label', NA, NA), sep = '_') %>% 
  mutate(rh_indicator = case_when(str_detect(model_identifier, 'rh0') ~ 'Not Rush Hour',
                                str_detect(model_identifier, 'rh1') ~ 'Rush Hour'),
         rh_indicator = factor(rh_indicator, levels = c('Rush Hour', 'Not Rush Hour')),
         Index = case_when(str_detect(model_identifier, 'ejiQ') ~ 'EJI',
                           str_detect(model_identifier, 'ice') ~ 'ICE'),
         Index = factor(Index, levels = c("ICE", 'EJI')),
         mod_label = str_replace(mod_label, 'eji', 'EJI'),
         mod_label = str_replace(mod_label, 'Q', ' Q'),
         mod_label = str_replace(mod_label, 'ice', 'ICE'),
         mod_label = str_replace(mod_label, 'HhincomeBw', ''),
         mod_label = str_replace(mod_label, 'rh0|rh1', ''),
         mod_label = factor(mod_label, 
                            levels = c('EJI Q1', 'EJI Q2', 'EJI Q3', 'EJI Q4', 'EJI Q5', 
                                       'ICE Q5', 'ICE Q4', 'ICE Q3', 'ICE Q2', 'ICE Q1')),
         mod_label_ejiFlipped = case_when(
           mod_label == 'EJI Q1' ~ 'EJI Q5',
           mod_label == 'EJI Q2' ~ 'EJI Q4',
           mod_label == 'EJI Q4' ~ 'EJI Q2',
           mod_label == 'EJI Q5' ~ 'EJI Q1',
           TRUE ~ as.character(mod_label)),
         quintile_label = case_when(
           str_detect(mod_label_ejiFlipped, 'Q1') ~ 'Q1', str_detect(mod_label_ejiFlipped, 'Q2') ~ 'Q2',
           str_detect(mod_label_ejiFlipped, 'Q3') ~ 'Q3', str_detect(mod_label_ejiFlipped, 'Q4') ~ 'Q4',
           str_detect(mod_label_ejiFlipped, 'Q5') ~ 'Q5'),
         quintile_label = factor(quintile_label, levels = c('Q5', 'Q4', 'Q3', 'Q2', 'Q1'))) 

# 6b Pivot so we can plot both coefficients 
mod_results2 <- mod_results2 %>% 
  pivot_longer(cols = coef_pause:uci_pauseEnd, names_sep = '_',
               names_to = c('limit', 'Policy')) %>% 
  pivot_wider(names_from = limit, values_from = value, names_repair = 'check_unique') %>% 
  mutate(Policy = ifelse(Policy == 'pause', 'Pause', 'Recovery'))
  
# 6c Create grouped bar chart for rush hour time points
fig5_a <- mod_results2 %>% 
  filter(Index == 'ICE') %>%
  ggplot(aes(y = quintile_label, x = coef, xmin = lci, xmax = uci,
             fill = Policy, group = Policy)) +
  geom_col(position = "dodge", width = .8) +
  geom_errorbar(position = "dodge", size = 1) +
  geom_vline(aes(xintercept = 0), linetype = 'solid') +
  scale_fill_manual(values = cividis(n = 2)) +
  xlim(c(-12,0.5)) +
  facet_wrap(~rh_indicator, ncol = 1) +
  xlab("") + ylab("") + 
  theme_bw() +
  theme(text = element_text(size = 9.5), legend.position = 'top',
         axis.text.y = element_text(size = 10))
fig5_a

# 6d Create grouped bar chart for rush hour time points
fig5_b <- mod_results2 %>% 
  filter(Index == 'EJI') %>%
  ggplot(aes(y = quintile_label, x = coef, xmin = lci, xmax = uci,
             fill = Policy, group = Policy)) +
  geom_col(position = "dodge", width = .8) +
  geom_errorbar(position = "dodge", size = 1) +
  geom_vline(aes(xintercept = 0), linetype = 'solid') +
  scale_fill_manual(values = cividis(n = 2)) +
  xlim(c(-12,0.5)) +
  facet_wrap(~rh_indicator, ncol = 1) +
  xlab("") + ylab("") + 
  theme_bw() +
  theme(text = element_text(size = 9.5), legend.position = 'none',
        axis.text.y = element_text(size = 10))
fig5_b

# 6e Create labelling variables for use in Grobs below
xlab = 'Decrease in % Traffic Congestion from Pre-Pause Trend'
ylab = 'Strata'
ice_label = 'A: ICE'
eji_label = 'B: EJI'

# 6f Combine and save plot
tiff(paste0(figure_path, 'fig5_ModResultsPlot_RushHour.tiff'),
     units = "cm", width = 16, height = 9, res = 300)
fig5_a + fig5_b
grid::grid.draw(grid::textGrob(xlab, x = 0.5,  y = 0.05, gp = grid::gpar(fontsize = 10)))
grid::grid.draw(grid::textGrob(ylab, x = 0.03,  rot = 90, gp = grid::gpar(fontsize = 10)))
grid::grid.draw(grid::textGrob(ice_label, x = 0.13,  y = 0.84, gp = grid::gpar(fontsize = 11)))
grid::grid.draw(grid::textGrob(eji_label, x = 0.61,  y = 0.84, gp = grid::gpar(fontsize = 11)))
dev.off()







