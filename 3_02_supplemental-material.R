# Supplemental Material for Manuscript
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 08/11/2023

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Prepare Map of EJI Module Tertiles
# 2: Plot: Sens Analyses Changing Tensor Term Knots
# 3: Plot: Sens Analyses Remove Traffic Vars from EJI
# 4: Plot: Sens Analyses Adjust EJI Mods for Each Other
# 5: Table: 5x5 Showing Census Tract Overlap Between EJI/ICE Quintiles
# 6: Plot: Traffic Congestion Pre/Post Pause Cut into Deciles
# 7: Plot: Histograms of CT/Date Traffic Correlation Tensor Term Knots
# 8: Plot: Histogram of Dates with Missing Hours
# 9: Evaluate Num Days Missing for Each CT

####**************
#### N: Notes ####
####**************

# Na Description
# In this script we create supplemental plots / tables. 

# A note about ICE and EJI quantiles
#       All quantiles are calculated such that Q1 corresponds to the lower values,
#       and Q5 corresponds to the higher values. However, for ICE higher values 
#       correspond to the most privileged group, while lower values correspond to
#       the most disadvantaged group. In contrast for EJI, higher values correspond
#       to the most burdened group, while lower values correspond to the least
#       burdened group. For display purposes only, (3_02) the quantiles of EJI will
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
model_path <- paste0(project.folder, 'outputs/models/')
figure_path <- paste0(project.folder, 'outputs/figures/')
polygons_of_interest_path = here::here('data', 'nyc_census_tracts', 'nycgeo_census_tracts.shp')
raw_data_path <- paste0(project.folder, 'data/raw_data/')

# 0d Load data
fullData <- read_rds(paste0(data_path, 'full_dataset_wcovars_daily.rds'))
tracts_sf <- st_read(polygons_of_interest_path)
mod_results <- read_csv(paste0(model_path, 'model_results_table.csv'))
load(here::here('data', 'needed_for_gt_to_polygons_function', 'gt_extent.RData'))
inset <- jpeg::readJPEG(paste0(figure_path, 'nyc_inset.jpeg'), native = T)
no2 <- read_csv(paste0(raw_data_path, 'NYCCAS_NO2_summer-winter-annual.csv'))
nyc_eji_ebm <- read_fst(paste0(data_path, 'eji_nyc_ebm.fst'))

####*******************************************
#### 1: Prepare Map of EJI Module Tertiles #### 
####*******************************************

# 1a Prepare choropleth map of EJI tertiles
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
                     option = 'cividis',
                     discrete = T,
                     labels = c('T1 (Greater Burden)', 'T2',
                                'T3 (Lesser Burden)', 'Not Enough Data')) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
ejiModuleTertsMap

# 1b Save plot
tiff(paste0(figure_path, 'figX_ejiModuleChlorMap.tiff'),
     units = "cm", width = 16, height = 9, res = 300)
ejiModuleTertsMap
dev.off()

####*******************************************************
#### 2: Plot: Sens Analyses Changing Tensor Term Knots #### 
####*******************************************************

# 2a Clean strata names for plotting
mod_results3 <- mod_results %>% 
  filter(!str_detect(model_identifier, 'rh')) %>% 
  filter(!str_detect(model_identifier,'propGreen')) %>% 
  filter(!str_detect(model_identifier, 'Ebm|Svm|Hvm')) %>%  
  filter(!str_detect(model_identifier, 'main')) %>% 
  filter(!str_detect(model_identifier, 'Sens')) %>% 
  filter(!str_detect(model_identifier, 'sensKnots4.2')) %>% 
  mutate(mod_label = as.character(model_identifier)) %>% 
  separate(col = mod_label, into = c('mod_label', NA, 'knots'), sep = '_') %>% 
  mutate(knots = ifelse(knots == 'includeRecovery', 'sensKnots8.4', knots),
         facet_type = case_when(str_detect(knots, 'sensKnots11.5') ~ 'Lat 11, Lon 5',
                                str_detect(knots, 'sensKnots16.8') ~ 'Lat 16, Lon 8',
                                str_detect(knots, 'sensKnots8.4') ~ 'Lat 8, Lon 4 (Main)'),
         mod_label = str_replace(mod_label, 'eji', 'EJI'),
         mod_label = str_replace(mod_label, 'Q', ' Q'),
         mod_label = str_replace(mod_label, 'ice', 'ICE'),
         mod_label = str_replace(mod_label, 'HhincomeBw', ''),
         mod_label = str_replace(mod_label, 'sensKnots4.2|sensKnots11.5|sensKnots16.8', ''),
         mod_label = factor(mod_label, 
                            levels = c('EJI Q1', 'EJI Q2', 'EJI Q3', 'EJI Q4', 'EJI Q5',
                                       'ICE Q5', 'ICE Q4', 'ICE Q3', 'ICE Q2', 'ICE Q1')),
         mod_label_ejiFlipped = case_when(
           mod_label == 'EJI Q1' ~ 'EJI Q5',
           mod_label == 'EJI Q2' ~ 'EJI Q4',
           mod_label == 'EJI Q4' ~ 'EJI Q2',
           mod_label == 'EJI Q5' ~ 'EJI Q1',
           TRUE ~ as.character(mod_label))) %>% 
  arrange(mod_label)

# 2b Add an 'order' variable to use for plotting the y axis. 
mod_results3$order = c(-0.2,0.2,2.8,3.2,5.8,6.2,8.8,9.2,11.8,12.2,
                       14.8,15.2,17.8,18.2,20.8,21.2,23.5,24,24.5,26.8,27.2)

# 2c Pivot so we can plot both coefficients 
mod_results3 <- mod_results3 %>%  
  pivot_longer(cols = coef_pause:uci_pauseEnd, names_sep = '_',
               names_to = c('limit', 'Policy')) %>% 
  pivot_wider(names_from = limit, values_from = value, names_repair = 'check_unique') %>% 
  mutate(Policy = case_when(
    Policy == 'pause' ~ 'NY on Pause',
    Policy == 'pauseEnd' ~ 'Reopening',
    TRUE ~ as.character(Policy)
  ))

# 2d Create vectors for y-axis breaks and labels
breaks4sensKnots <- c(0,3,6,9,12,15,18,21,24,27)
labels4sensKnots <- c('EJI Q5', 'EJI Q4', 'EJI Q3', 'EJI Q2', 'EJI Q1',
                      'ICE Q5', 'ICE Q4', 'ICE Q3', 'ICE Q2', 'ICE Q1')

# 2e Create forest plot of models with different knot specifications
sensKnots <- mod_results3 %>% 
  ggplot(aes(x = coef, y = order, xmin = lci, xmax = uci,
             color = facet_type, shape = Policy)) +
  geom_pointrange(fatten = 2) +
  geom_vline(aes(xintercept = 0), linetype = 'solid') +
  xlim(c(-8,1)) +
  scale_y_continuous(breaks = breaks4sensKnots, 
                     labels = labels4sensKnots,
                     limits = c(-1,28)) +
  guides(color = guide_legend(title = 'Tensor Term Knots')) +
  geom_hline(aes(yintercept = 13.5), linetype = 'dashed', color = 'gray60') +
  xlab("Decrease in % Traffic Congestion from Pre-Pause Trend") + ylab("Strata") + 
  theme_bw() +
  theme(text = element_text(size = 10))
sensKnots

# 2f Save plot
tiff(paste0(figure_path, 'figX_sensKnotsPlot.tiff'),
     units = "cm", width = 16, height = 9, res = 300)
sensKnots
dev.off()

####*********************************************************
#### 3: Plot: Sens Analyses Remove Traffic Vars from EJI #### 
####*********************************************************

# 3a Clean strata names for plotting
mod_results4 <- mod_results %>% 
  filter(!str_detect(model_identifier, 'rh')) %>% 
  filter(!str_detect(model_identifier,'propGreen')) %>% 
  filter(!str_detect(model_identifier, 'Ebm|Svm|Hvm')) %>%  
  filter(!str_detect(model_identifier, 'main')) %>% 
  filter(!str_detect(model_identifier, 'iceHhincomeBw')) %>% 
  filter(!str_detect(model_identifier, 'sensKnots')) %>% 
  mutate(mod_label = as.character(model_identifier)) %>% 
  separate(col = mod_label, into = c('mod_label', NA, NA), sep = '_') %>% 
  mutate(facet_type = case_when(str_detect(mod_label, 'Sens') ~ 'No Traffic Vars',
                                TRUE ~ 'Main Analysis'),
         mod_label = str_replace(mod_label, 'eji', 'EJI'),
         mod_label = str_replace(mod_label, 'Q', ' Q'),
         mod_label = str_replace(mod_label, 'Sens', ''),
         mod_label = factor(mod_label, 
                            levels = c('EJI Q1', 'EJI Q2', 'EJI Q3', 'EJI Q4', 'EJI Q5')),
         mod_label_ejiFlipped = case_when(
           mod_label == 'EJI Q1' ~ 'EJI Q5',
           mod_label == 'EJI Q2' ~ 'EJI Q4',
           mod_label == 'EJI Q4' ~ 'EJI Q2',
           mod_label == 'EJI Q5' ~ 'EJI Q1',
           TRUE ~ as.character(mod_label))) %>% 
  arrange(mod_label)

# 3b Add an 'order' variable to use for plotting the y axis. 
mod_results4$order = c(-0.5,0.5,2.5,3.5,5.5,6.5,8.5,9.5,11.5,12.5)

# 3c Pivot so we can plot both coefficients 
mod_results4 <- mod_results4 %>%  
  pivot_longer(cols = coef_pause:uci_pauseEnd, names_sep = '_',
               names_to = c('limit', 'Policy')) %>% 
  pivot_wider(names_from = limit, values_from = value, names_repair = 'check_unique') %>% 
  mutate(Policy = case_when(
    Policy == 'pause' ~ 'NY on Pause',
    Policy == 'pauseEnd' ~ 'Reopening',
    TRUE ~ as.character(Policy)
  ))

# 3d Create vectors for y-axis breaks and labels
breaks4sensEJItraf <- c(0,3,6,9,12)
labels4sensEJItraf <- c('Q5 (Lesser Burden)', 'Q4', 'Q3', 'Q2', 'Q1 (Greater Burden)')

# 3e Create forest plot of rush hour results
sensEJItraf <- mod_results4 %>% 
  ggplot(aes(x = coef, y = order, xmin = lci, xmax = uci,
             color = facet_type, shape = Policy)) +
  geom_pointrange(fatten = 2) +
  geom_vline(aes(xintercept = 0), linetype = 'solid') +
  xlim(c(-8,1)) +
  scale_y_continuous(breaks = breaks4sensEJItraf, 
                     labels = labels4sensEJItraf,
                     limits = c(-1,13)) +
  guides(color = guide_legend(title = 'Model')) +
  xlab("Decrease in % Traffic Congestion from Pre-Pause Trend") + ylab("Strata") + 
  theme_bw() +
  theme(text = element_text(size = 10))
sensEJItraf

# 3f Combine and save plot
tiff(paste0(figure_path, 'figX_sensEJInoTrafPlot.tiff'),
     units = "cm", width = 16, height = 9, res = 300)
sensEJItraf
dev.off()

####***********************************************************
#### 4: Plot: Sens Analyses Adjust EJI Mods for Each Other #### 
####***********************************************************

# 4a Clean strata names for plotting
mod_results5 <- mod_results %>% 
  filter(!str_detect(model_identifier, 'rh')) %>% 
  filter(!str_detect(model_identifier,'propGreen')) %>% 
  filter(!str_detect(model_identifier, 'Sens')) %>%  
  filter(!str_detect(model_identifier, 'main')) %>% 
  filter(!str_detect(model_identifier, 'iceHhincomeBw')) %>% 
  filter(!str_detect(model_identifier, 'sensKnots')) %>% 
  filter(!str_detect(model_identifier, 'ejiQ')) %>% 
  mutate(mod_label = as.character(model_identifier)) %>% 
  separate(col = mod_label, into = c('mod_label', NA, 'analysis'), sep = '_') %>% 
  mutate(facet_type = case_when(str_detect(analysis, 'ejiModsAdjusted') ~ 'Adjusted',
                                TRUE ~ 'Main Analysis'),
         mod_label = str_replace(mod_label, 'eji', ''),
         mod_label = str_replace(mod_label, 'Q', ' T'),
         mod_label = str_replace(mod_label, 'Ebm', 'EBM'),
         mod_label = str_replace(mod_label, 'Svm', 'SVM'),
         mod_label = str_replace(mod_label, 'Hvm', 'HVM'),
         mod_label = factor(mod_label, 
                            levels = c('EBM T1', 'EBM T2', 'EBM T3', 
                                       'SVM T1', 'SVM T2', 'SVM T3',
                                       'HVM T1', 'HVM T2', 'HVM T3')),
         mod_label_ejiFlipped = case_when(
           mod_label == 'EBM T1' ~ 'EBM T3',
           mod_label == 'EBM T3' ~ 'EBM T1',
           mod_label == 'SVM T1' ~ 'SVM T3',
           mod_label == 'SVM T3' ~ 'SVM T1',
           mod_label == 'HVM T1' ~ 'HVM T3',
           mod_label == 'HVM T3' ~ 'HVM T1',
           TRUE ~ as.character(mod_label))) %>% 
  arrange(mod_label)

# 4b Add an 'order' variable to use for plotting the y axis. 
mod_results5$order = c(-0.5,0.5,2.5,3.5,5.5,6.5,8.5,9.5,11.5,12.5,
                       14.5,15.5,17.5,18.5,20.5,21.5,23.5,24.5)

# 4c Pivot so we can plot both coefficients 
mod_results5 <- mod_results5 %>%  
  pivot_longer(cols = coef_pause:uci_pauseEnd, names_sep = '_',
               names_to = c('limit', 'Policy')) %>% 
  pivot_wider(names_from = limit, values_from = value, names_repair = 'check_unique') %>% 
  mutate(Policy = case_when(
    Policy == 'pause' ~ 'NY on Pause',
    Policy == 'pauseEnd' ~ 'Reopening',
    TRUE ~ as.character(Policy)
  ))

# 4d Create vectors for y-axis breaks and labels
breaks4ejiModsAdjusted <- c(0,3,6,9,12,15,18,21,24)
labels4ejiModsAdjusted <- c('EBM T3', 'EBM T2', 'EBM T1', 'SVM T3', 'SVM T2', 'SVM T1',
                            'HVM T3', 'HVM T2', 'HVM T1')

# 4e Create forest plot of EJI mod results
ejiModsAdjusted <- mod_results5 %>% 
  ggplot(aes(x = coef, y = order, xmin = lci, xmax = uci,
             color = facet_type, shape = Policy)) +
  geom_pointrange(fatten = 2) +
  geom_vline(aes(xintercept = 0), linetype = 'solid') +
  geom_hline(aes(yintercept = 7), linetype = 'dashed', color = 'gray60') +
  geom_hline(aes(yintercept = 16), linetype = 'dashed', color = 'gray60') +
  xlim(c(-8,1)) +
  scale_y_continuous(breaks = breaks4ejiModsAdjusted, 
                     labels = labels4ejiModsAdjusted,
                     limits = c(-1,25)) +
  guides(color = guide_legend(title = 'Model')) +
  xlab("Decrease in % Traffic Congestion from Pre-Pause Trend") + ylab("Strata") + 
  theme_bw() +
  theme(text = element_text(size = 10))
ejiModsAdjusted

# 3f Combine and save plot
tiff(paste0(figure_path, 'figX_sensEjiModsAdjusted.tiff'),
     units = "cm", width = 16, height = 9, res = 300)
ejiModsAdjusted
dev.off()

####**************************************************************************
#### 5: Table: 5x5 Showing Census Tract Overlap Between EJI/ICE Quintiles #### 
####**************************************************************************

# 5a Summarise to unique census tract observations and flip EJI labels
#    Note: Census tracts do not change quintiles of EJI or ICE over
#          the study period, so we can take the first observation for each
#          census tract
table5x5 <- fullData %>% group_by(poly_id) %>% 
  summarise(eji_5 = first(eji_5),
            ice_hhincome_bw_5 = first(ice_hhincome_bw_5)) %>% 
  mutate(eji_5_relabeled = case_when(
           eji_5 == 'Q1' ~ 'Q5', eji_5 == 'Q2' ~ 'Q4', eji_5 == 'Q3' ~ 'Q3',
           eji_5 == 'Q4' ~ 'Q2', eji_5 == 'Q5' ~ 'Q1', TRUE ~ 'NA'),
         eji_5_relabeled = factor(eji_5_relabeled, levels = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5')),
         ice_hhincome_bw_5 = factor(ice_hhincome_bw_5, levels = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5')))

# 5b Run 5x5 table
table(table5x5$eji_5_relabeled, table5x5$ice_hhincome_bw_5, useNA = 'always')

# 5c Correlation between EJI and ICE
#    Note: -.88 (note that the scales run in opposite directions for EJI & ICE)
# 5c.i Set up dataset
cor_df <- fullData %>% group_by(poly_id) %>% 
  summarise(rpl_eji = first(rpl_eji),
            ice_hhincome_bw = first(ice_hhincome_bw)) 
# 5c.ii Run correlation
cor(cor_df$rpl_eji, cor_df$ice_hhincome_bw, method = c('pearson'), use = 'complete.obs')

####*****************************************************************
#### 6: Plot: Traffic Congestion Pre/Post Pause Cut into Deciles #### 
####*****************************************************************

# Two-panel choropleth map showing traffic on a day before and a day after
# implementation of NY on Pause, split into deciles

# 6a Determine min and max of two dates for plotting
#    Note: min = 0; max = 34.1
fullData %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% ungroup() %>% 
  filter(date == '2020-03-09') %>% 
  summarise(max = max(prop_maroon_red, na.rm = T), min = min(prop_maroon_red, na.rm = T))
fullData %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% ungroup() %>% 
  filter(date == '2020-03-23') %>% 
  summarise(max = max(prop_maroon_red, na.rm = T), min = min(prop_maroon_red, na.rm = T))

# 6b Calculate deciles
deciles_cong = quantile(fullData$prop_maroon_red, na.rm = T, probs = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1))

# 6c Assign new 
deciles_df <- fullData %>% 
  mutate(
    cong_10 = factor(case_when(
      prop_maroon_red < deciles_cong[2] ~ 'D1 (Low)',
      prop_maroon_red >= deciles_cong[2] & prop_maroon_red < deciles_cong[3] ~ 'D2',
      prop_maroon_red >= deciles_cong[3] & prop_maroon_red < deciles_cong[4] ~ 'D3',
      prop_maroon_red >= deciles_cong[4] & prop_maroon_red < deciles_cong[5] ~ 'D4',
      prop_maroon_red >= deciles_cong[5] & prop_maroon_red < deciles_cong[6] ~ 'D5',
      prop_maroon_red >= deciles_cong[6] & prop_maroon_red < deciles_cong[7] ~ 'D6',
      prop_maroon_red >= deciles_cong[7] & prop_maroon_red < deciles_cong[8] ~ 'D7',
      prop_maroon_red >= deciles_cong[8] & prop_maroon_red < deciles_cong[9] ~ 'D8',
      prop_maroon_red >= deciles_cong[9] & prop_maroon_red < deciles_cong[10] ~ 'D9',
      prop_maroon_red >= deciles_cong[10] ~ 'D10 (High)'), 
      levels = c('D1 (Low)', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7', 'D8', 'D9', 'D10 (High)')))

# 6d Create panel A - Monday before Pause was implemented
dec_prePost_map_a <- 
  deciles_df %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% 
  filter(date == '2020-03-09') %>% 
  ggplot(aes(geometry = geometry, fill = cong_10)) + 
  geom_sf(lwd = 0.2) + 
  annotate('text', x = 988000, y = 255000, label = 'A: 3/09/2020', size = 10/.pt,
           fontface = 2) +
  scale_fill_viridis_d(name = '', 
                       option = 'viridis') +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 10),
        legend.position = 'none')
dec_prePost_map_a

# 6e Create panel B - Monday after Pause was implemented
dec_prePost_map_b <- 
  deciles_df %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% 
  filter(date == '2020-03-23') %>% 
  ggplot(aes(geometry = geometry, fill = cong_10)) + 
  geom_sf(lwd = 0.2) + 
  annotate('text', x = 988000, y = 255000, label = 'B: 3/23/2020', size = 10/.pt,
           fontface = 2) +
  scale_fill_viridis_d(name = 'Decile of Traffic\nCongestion', 
                       option = 'viridis') +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 10))
dec_prePost_map_b

# 6f Combine panels into one plot and save
tiff(paste0(figure_path, 'figX_DecilePrePostPauseChoroMap.tiff'),
     units = "cm", width = 16, height = 9, res = 300)
dec_prePost_map_a + dec_prePost_map_b
dev.off()

####**************************************************************************
#### 7: Plot: Histograms of CT/Date Traffic Correlation Tensor Term Knots #### 
####**************************************************************************

# These histograms were run to show the variation in correlations between
# traffic congestion (values) in census tracts (columns) [rows = unique dates],
# as a way of assessing spatial and temporal autocorrelation.

# This was prompted because when running the sensitivity analyses changing
# knot values for the tensor product, no changes occurred to any effect estimates
# or standard errors (although R2 values changed, so they were different models)

# We run the correlations and histograms for one of most clustered strata (ICE Q1)
# for both the data and the residuals of the model with the lowest tensor term knot
# values

# This is a sanity check, and is not included in the supplemental material.

#***************** Original data

# 7a Prepare dataframe for correlations
corDF_iceQ1 <- fullData %>% filter(ice_hhincome_bw_5 == 'Q1') %>% 
  dplyr::select(date, poly_id, prop_maroon_red) %>% 
  pivot_wider(names_from = poly_id,
              values_from = prop_maroon_red) %>% 
  dplyr::select(-date)

# 7b Run correlations
cor_iceQ1 <- cor(corDF_iceQ1, use = 'complete.obs')

# 7c Histogram of correlations
ggplot() +
  geom_histogram(aes(x = cor_iceQ1))
  
#***************** Residuals from sens model with knots of 4 (Lat) and 2 (Lon)

# 7d Load model
mod4hist <- read_rds(paste0(model_path, 'iceHhincomeBwQ1_propMaroonRed_sensKnots4.2.rds'))

# 7e Pull residuals and add to dataframe for correlations  
corDF_iceQ1_resids <- fullData %>% filter(ice_hhincome_bw_5 == 'Q1') %>% 
  ungroup() %>% 
  dplyr::select(date, poly_id) %>% 
  mutate(resids = mod4hist$gam$residuals) %>% 
  pivot_wider(names_from = poly_id,
              values_from = resids) %>% 
  dplyr::select(-date)

# 7f Run correlations
cor_iceQ1_resids <- cor(corDF_iceQ1_resids, use = 'complete.obs')

# 7g Histogram of correlations
ggplot() +
  geom_histogram(aes(x = cor_iceQ1_resids))

#***************** Residuals from sens model with knots of 11 (Lat) and 5 (Lon)

# 7h Load model
mod4hist2 <- read_rds(paste0(model_path, 'iceHhincomeBwQ1_propMaroonRed_sensKnots11.5.rds'))

# 7i Pull residuals and add to dataframe for correlations  
corDF_iceQ1_resids2 <- fullData %>% filter(ice_hhincome_bw_5 == 'Q1') %>% 
  ungroup() %>% 
  dplyr::select(date, poly_id) %>% 
  mutate(resids = mod4hist2$gam$residuals) %>% 
  pivot_wider(names_from = poly_id,
              values_from = resids) %>% 
  dplyr::select(-date)

# 7j Run correlations
cor_iceQ1_resids2 <- cor(corDF_iceQ1_resids2, use = 'complete.obs')

# 7k Histogram of correlations
ggplot() +
  geom_histogram(aes(x = cor_iceQ1_resids2))
  
####****************************************************
#### 8: Plot: Histogram of Dates with Missing Hours #### 
####****************************************************

# 8a Load hourly dataset
traf <- read_fst(paste0(data_path, 'gt18-20_2010CTs.fst'))
  
# 8b Aggregate to daily
traf_daily <- traf %>% 
  mutate(date = date(captured_datetime),
         no_image = as.numeric(ifelse(no_image == 'no image', 1, 0))) %>% 
  group_by(poly_id, date) %>% 
  summarise(speed_reduct_fact = mean(speed_reduct_fact, na.rm = T),
            green_gray_85 = mean(green_gray_85, na.rm = T),
            gt_pixcount_streets = mean(gt_pixcount_streets, na.rm = T),
            prop_maroon_red = mean(prop_maroon_red, na.rm = T),
            prop_green = mean(prop_green, na.rm = T),
            gt_pixcount_maroon = mean(gt_pixcount_maroon, na.rm = T),
            gt_pixcount_red = mean(gt_pixcount_red, na.rm = T),
            gt_pixcount_orange = mean(gt_pixcount_orange, na.rm = T),
            gt_pixcount_green = mean(gt_pixcount_green, na.rm = T),
            gt_pixcount_gray = mean(gt_pixcount_gray, na.rm = T),
            gt_pixcount_construction = mean(gt_pixcount_construction, na.rm = T),
            gt_pixcount_emergency = mean(gt_pixcount_emergency, na.rm = T),
            gt_pixcount_notsampled = mean(gt_pixcount_notsampled, na.rm = T),
            gt_pixcount_background = mean(gt_pixcount_background, na.rm = T),
            gt_pixcount_tot = mean(gt_pixcount_tot, na.rm = T),
            no_image = sum(no_image),
            gt_pixcount_notstreets = mean(gt_pixcount_notstreets, na.rm = T),
            ice_gt = mean(ice_gt, na.rm = T))

# 8c Restrict to observations with NA poly_id (these were missing at least 1 hour)
#    Note: All these obs will have an NA poly_id
#          Obs with no_image == 0 are not actually dates missing 0 hours 
#            because since summarizing was done on both date and poly_id, all
#            the missing hours were grouped into the NA poly_id
missing_hour_df <- traf_daily %>% filter(is.na(poly_id)) %>% 
  group_by(no_image) %>% 
  summarise(count = n())

# 8d Create histogram of dates missing hours
missing_hour_hist <- missing_hour_df %>% 
  ggplot(aes(x = no_image, y = count)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1,24,1),
                     limits = c(0,25)) +
  xlab("Number of Hours Missing Traffic Congestion Data") + ylab("Count of Dates") + 
  theme_bw() +
  theme(text = element_text(size = 10))
missing_hour_hist

# 8e Combine and save plot
tiff(paste0(figure_path, 'figX_missingHourBarChart.tiff'),
     units = "cm", width = 16, height = 9, res = 300)
missing_hour_hist
dev.off()


####**********************************************
#### 9: Evaluate Num Days Missing for Each CT #### 
####**********************************************

# Not a figure, done for analytic purposes.

# 9a Load hourly dataset
traf <- read_fst(paste0(data_path, 'gt18-20_2010CTs.fst'))

# 9b Aggregate to daily
traf_daily <- traf %>% 
  mutate(date = date(captured_datetime),
         no_image = as.numeric(ifelse(no_image == 'no image', 1, 0)),
         missing_CCC_data = ifelse(gt_pixcount_notsampled == gt_pixcount_tot, 1, 0)) %>% 
  group_by(poly_id, date) %>% 
  summarise(speed_reduct_fact = mean(speed_reduct_fact, na.rm = T),
            green_gray_85 = mean(green_gray_85, na.rm = T),
            gt_pixcount_streets = mean(gt_pixcount_streets, na.rm = T),
            prop_maroon_red = mean(prop_maroon_red, na.rm = T),
            prop_green = mean(prop_green, na.rm = T),
            gt_pixcount_maroon = mean(gt_pixcount_maroon, na.rm = T),
            gt_pixcount_red = mean(gt_pixcount_red, na.rm = T),
            gt_pixcount_orange = mean(gt_pixcount_orange, na.rm = T),
            gt_pixcount_green = mean(gt_pixcount_green, na.rm = T),
            gt_pixcount_gray = mean(gt_pixcount_gray, na.rm = T),
            gt_pixcount_construction = mean(gt_pixcount_construction, na.rm = T),
            gt_pixcount_emergency = mean(gt_pixcount_emergency, na.rm = T),
            gt_pixcount_notsampled = mean(gt_pixcount_notsampled, na.rm = T),
            gt_pixcount_background = mean(gt_pixcount_background, na.rm = T),
            gt_pixcount_tot = mean(gt_pixcount_tot, na.rm = T),
            no_image = sum(no_image),
            gt_pixcount_notstreets = mean(gt_pixcount_notstreets, na.rm = T),
            ice_gt = mean(ice_gt, na.rm = T),
            num_hours_missing_CCC_data = sum(missing_CCC_data, na.rm = T))

# 9c Review number of date/CT obs that have had missing CCC data
table(traf_daily$num_hours_missing_CCC_data)

# 9c Count number observations and number observations missing congestion data by CT
traf_daily_byCT <- traf_daily %>% 
  group_by(poly_id) %>% 
  summarise(dates = n())

# 9d Review counts
table(traf_daily_byCT$dates)

# 9e Evaluate the not sampled pixcounts
evalaute_notsampled <- fullData %>% 
  mutate(NS_not_0 = ifelse(gt_pixcount_notsampled > 4, 1, 0)) %>% 
  group_by(poly_id) %>% 
  summarise(count_NS_not_0 = sum(NS_not_0), 
            count_obs = n(),
            min_NS = min(gt_pixcount_notsampled), 
            med_NS = median(gt_pixcount_notsampled), 
            max_NS = max(gt_pixcount_notsampled), 
            gt_pixcount_tot = mean(gt_pixcount_tot)) %>% 
  mutate(max_percent_of_total = max_NS/gt_pixcount_tot*100,
         NS_not_0_percent_of_total = count_NS_not_0/count_obs*100) %>% 
  dplyr::select(poly_id, count_NS_not_0, count_obs, NS_not_0_percent_of_total, 
                everything())


####****************************************************
#### 10: Table: Change in NO2 by Community District #### 
####****************************************************

# We will use puma names in the census tract shapefile and community district
# names in the no2 data to join all data together. Puma names contain
# community district names, although Bronx 1 & 2, Bronx 3 & 6, 
# Manhattan 1 & 2, and Manhattan 4 & 5 are merged. This will be more accurate
# than overlaying a shapefile, because in 2010, census tracts did not nest
# within community districts and there would have been some overlap

# 10a Tidy no2 data variable names
no2 <- no2 %>% janitor::clean_names()

# 10b Keep only needed time periods, geography, and vars in NO2 dataset
#     Need CD geography, summer 2019 and summer 2020 time periods, and mean 
#       no2 concentration (ppb)
no2 <- no2 %>% filter(geo_type == 'CD') %>% 
  filter(time_period == 'Summer 2019' | time_period == 'Summer 2020') %>% 
  dplyr::select(-geo_rank, -x10th_percentile_ppb, -x90th_percentile_ppb,
                -geo_type) 

# 10c Calculate 2019-2020 summer change in NO2 by community 
#     district & keep only needed variables
no2 <- no2 %>% pivot_wider(names_from = time_period, values_from = mean_ppb) %>% 
  janitor::clean_names() 
no2 <- no2 %>% mutate(no2_change = summer_2020 - summer_2019) %>% 
  dplyr::select(-summer_2020, -summer_2019)

# 10d Alter geo_id to create new ids for the community districts that are
#     joint (Bronx 1 & 2, Bronx 3 & 6, Manhattan 1 & 2, and Manhattan 4 & 5)
no2 <- no2 %>% mutate(geo_id = case_when(
  geo_id == 201 | geo_id == 202 ~ '201_202',
  geo_id == 101 | geo_id == 102 ~ '101_102',
  geo_id == 203 | geo_id == 206 ~ '203_206',
  geo_id == 104 | geo_id == 105 ~ '104_105',
  TRUE ~ as.character(geo_id)
))

# 10e Average NO2 concentrations and merge community district names for 
#     combined community districts 
#     (Bronx 1 & 2, Bronx 3 & 6, Manhattan 1 & 2, and Manhattan 4 & 5)
no2 <- no2 %>% mutate(no2_change = case_when(
  geo_id == '201_202' ~ mean(c(-1.5, -1)),
  geo_id == '101_102' ~ mean(c(-6.6, -6.3)),
  geo_id == '203_206' ~ mean(c(-0.4, 0.3)),
  geo_id == '104_105' ~ mean(c(-5.7, -5.2)),
  TRUE ~ no2_change
))
no2 <- no2 %>% mutate(geography = case_when(
  geography == 'Financial District (CD1)' ~ 'Financial District (CD1) & Greenwich Village and Soho (CD2)',
  geography == 'Clinton and Chelsea (CD4)' ~ 'Clinton and Chelsea (CD4) & Midtown (CD5)',
  geography == 'Mott Haven and Melrose (CD1)' ~ 'Mott Haven and Melrose (CD1) & Hunts Point and Longwood (CD2)',
  geography == 'Morrisania and Crotona (CD3)' ~ 'Morrisania and Crotona (CD3) & Belmont and East Tremont (CD6)',
  TRUE ~ geography
)) %>% 
  filter(!geography == 'Greenwich Village and Soho (CD2)') %>% 
  filter(!geography == 'Midtown (CD5)') %>% 
  filter(!geography == 'Hunts Point and Longwood (CD2)') %>% 
  filter(!geography == 'Belmont and East Tremont (CD6)')

# 10f Pull polygon ids, and ICE / EJI quintiles from full dataset
ct_ice_eji <- fullData %>% dplyr::select(poly_id,ice_hhincome_bw_5, eji_5) %>% 
  distinct()

# 10g Identify community districts with 100% of census tracts in study area 
n_cts_cds <- as.data.frame(tracts_sf) %>% group_by(puma_nm) %>% 
  summarise(n_cts_full = n())

# 10g Merge ICE / EJI quintile data with census tract shapefile and count of
#     census tracts in each community district
tracts_sf_ice_eji <- ct_ice_eji %>% 
  left_join(tracts_sf, by = c('poly_id' = 'geoid')) %>% 
  left_join(n_cts_cds, by = 'puma_nm')

# 10i Convert to dataframe and make new community district id variable that
#     matches geo_id from no2 data
tracts_ice_eji <- as.data.frame(tracts_sf_ice_eji) %>% 
  mutate(geo_id2 = gsub("\\D", "", puma_nm),
         geo_id3 = str_pad(geo_id2, width = 2, side = c('left'), pad = '0'),
         geo_id4 = case_when(
           brgh_nm == 'Bronx' ~ paste0('2', geo_id3),
           brgh_nm == 'Brooklyn' ~ paste0('3', geo_id3),
           brgh_nm == 'Manhattan' ~ paste0('1', geo_id3),
           brgh_nm == 'Queens' ~ paste0('4', geo_id3)
         ))
tracts_ice_eji <- tracts_ice_eji %>% 
  mutate(geo_id = case_when(
    brgh_nm == 'Bronx' & str_detect(puma_nm, '1 & 2') ~ '201_202',
    brgh_nm == 'Bronx' & str_detect(puma_nm, '3 & 6') ~ '203_206',
    brgh_nm == 'Manhattan' & str_detect(puma_nm, '1 & 2') ~ '101_102',
    brgh_nm == 'Manhattan' & str_detect(puma_nm, '4 & 5') ~ '104_105',
    TRUE ~ geo_id4
  ))

# 10j Merge no2 and EJI / ICE data and keep only needed vars
cd_no2_seg <- tracts_ice_eji %>% 
  left_join(no2, by = 'geo_id') %>% 
  dplyr::select(geo_id, geography, no2_change, ice_hhincome_bw_5, eji_5,
                n_cts_full)

# 10k Aggregate EJI and ICE to community district level using mode
# 10k.i Create mode function b/c base R does not have one
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
# 10k.ii
cd_no2_seg <- cd_no2_seg %>% 
  group_by(geo_id, geography, no2_change, n_cts_full) %>% 
  summarise(ice_hhincome_bw_5_mode = Mode(ice_hhincome_bw_5, na.rm = TRUE),
            eji_5_mode = Mode(eji_5, na.rm = TRUE),
            n_cts = n()) %>% ungroup()

# 10l Flag community districts with less than 50% of census tracts
cd_no2_seg <- cd_no2_seg %>% 
  mutate(include = ifelse((n_cts/n_cts_full)*100 > 50, 1, 0))

# 10m Format as table and write to clipboard
suptab_no2 <- cd_no2_seg %>% 
  ungroup() %>% 
  filter(include == 1) %>% 
  mutate(borrough = case_when(
    geo_id == '204' | geo_id == '205' ~ 'Bronx',
    geo_id == '302' ~ 'Brooklyn',
    geo_id == '201_202' ~ 'Bronx',
    TRUE ~ 'Manhattan')) %>% 
  mutate(eji_5_modeFlipped = case_when(
    eji_5_mode == 'Q1' ~ 'Q5',
    eji_5_mode == 'Q2' ~ 'Q4',
    eji_5_mode == 'Q3' ~ 'Q3',
    eji_5_mode == 'Q4' ~ 'Q2',
    eji_5_mode == 'Q5' ~ 'Q1',
    TRUE ~ NA)) %>% 
  dplyr::select(borrough, geography, ice_hhincome_bw_5_mode, 
                eji_5_modeFlipped, no2_change) %>% 
  arrange(no2_change)
clipr::write_clip(suptab_no2)


####***********************************************
#### 11: Table: EBM Module domains and metrics #### 
####***********************************************

# 11a Restrict to census tracts in study area
#     Should end with 436 census tracts (9 are missing EJI data)
cts <- fullData$poly_id %>% unique()
nyc_eji_ebm_study_area <- nyc_eji_ebm %>% 
  filter(geoid %in% cts) %>% 
  na.omit()

# 11b Calculate quantiles
quants_eji_ebm = quantile(nyc_eji_ebm_study_area$rpl_ebm, na.rm = T, probs = c(0, .33, .66, 1))

# 11c Assign new stratified variables
#     Tertiles are flipped!! T3 = least burden, T1 = most burden
nyc_eji_ebm_study_area <- nyc_eji_ebm_study_area %>% 
  mutate(
    eji_ebm_3 = factor(case_when(
      rpl_ebm < quants_eji_ebm[2] ~ 'T3',
      rpl_ebm >= quants_eji_ebm[2] & rpl_ebm < quants_eji_ebm[3] ~ 'T2',
      rpl_ebm >= quants_eji_ebm[3] ~ 'T1')))

# 11d Run tables to confirm missing obs counts match those above
table(nyc_eji_ebm_study_area$eji_ebm_3, useNA = 'always')

# 11e Pivot longer
ebm_long <- nyc_eji_ebm_study_area %>% 
  pivot_longer(e_ozone_epl:e_impwtr_epl, names_to = 'domain', values_to = 'epl') 

# 11f Calculate mean and sd of each epl grouped by tertile and domain
ebm_long <- ebm_long %>% 
  group_by(eji_ebm_3, domain) %>% 
  mutate(mean_epl = round(mean(epl), digits = 2),
         sd_epl = round(sd(epl), digits = 2)) %>% 
  ungroup()

# 11g Pivot wider so that each tertile is a column
ebm_wide <- ebm_long %>% 
  mutate(mean_sd_epl = paste0(mean_epl, ' (', sd_epl, ')')) %>% 
  dplyr::select(eji_ebm_3, domain, mean_sd_epl) %>% 
  distinct() %>% 
  pivot_wider(names_from = 'eji_ebm_3', values_from = 'mean_sd_epl') %>% 
  dplyr::select(domain, T1, T2, T3)

# 11h Tidy domain names
ebm_wide <- ebm_wide %>% 
  mutate(domain = str_replace(domain, 'e_', ''),
         domain = str_replace(domain, '_epl', '')) %>% 
  mutate(domain = case_when(
    domain == 'ozone' ~ 'Annual Mean Days Above Ozone Regulatory Standard',
    domain == 'pm' ~ 'Annual Mean Days Above PM2.5 Regulatory Standard',
    domain == 'dslpm' ~ 'Ambient Diesel PM Concentration',
    domain == 'totcr' ~ 'Probability of Cancer in Lifetime',
    domain == 'npl' ~ 'Proportion of Area within 1-mile of EPA National Priority List Site',
    domain == 'tri' ~ 'Proportion of Area within 1-mile of EPA Toxic Release Inventory Site',
    domain == 'tsd' ~ 'Proportion of Area within 1-mile of EPA Treatment, Storage, & Disposal Site',
    domain == 'rmp' ~ 'Proportion of Area within 1-mile of EPA Risk Management Plan Site',
    domain == 'coal' ~ 'Proportion of Area within 1-mile of Coal Mine',
    domain == 'lead' ~ 'Proportion of Area within 1-mile of Lead Mine',
    domain == 'park' ~ 'Proportion of Area within 1-mile of Green Space',
    domain == 'houage' ~ 'Percentage of Houses Built pre-1980',
    domain == 'wlkind' ~ 'Relative Walkability Rank',
    domain == 'rail' ~ 'Proportion of Area within 1-mile of Railroad',
    domain == 'road' ~ 'Proportion of Area within 1-mile of High-Volume Road/Highway',
    domain == 'airprt' ~ 'Proportion of Area within 1-mile of Airport',
    domain == 'impwtr' ~ 'Impaired Water Bodies',
    TRUE ~ domain
  ))

# 11i Add domain groupings
suptab_ebm <- ebm_wide %>% 
  mutate(group = c('Air Pollution', 'Air Pollution', 'Air Pollution',
                   'Air Pollution', 'Potentially Hazardous & Toxic Sites', 
                   'Potentially Hazardous & Toxic Sites', 
                   'Potentially Hazardous & Toxic Sites', 
                   'Potentially Hazardous & Toxic Sites', 
                   'Potentially Hazardous & Toxic Sites', 
                   'Potentially Hazardous & Toxic Sites', 'Built Environment', 
                   'Built Environment', 'Built Environment', 
                   'Transportation Infrastructure', 'Transportation Infrastructure', 
                   'Transportation Infrastructure', 'Water Pollution')) %>% 
  dplyr::select(group, everything())
clipr::write_clip(suptab_ebm)
