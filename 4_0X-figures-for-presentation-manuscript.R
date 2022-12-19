# Create Figures for Manuscript and Presentations
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 02/06/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Chloropleth map of census ice vars
# 2: Maps comparing GT Raster to GT Polygons
# 3: Pre-post NY on PAUSE map
# 4: Stratified model results maps

# for each census ice var: exposure-response curve

####**************
#### N: Notes ####
####**************

# Na Description
# 


####********************
#### 0: Preparation #### 
####********************

# 0a Load Packages
packages <- c("tidyverse", "fst", "nycgeo", "sf", "scales", "lubridate",
              "raster", "rgdal", "terra", "here", "rasterVis", "ggmap")
lapply(packages, library, character.only = TRUE)

# 0b Load data
tracts <- nycgeo::nyc_boundaries(geography = "tract")
ice_census <- read_fst(path = "./data/ice_census_vars.fst")
ice_gt <- fst::read_fst(here::here('data', 'ice_gt_jantomar2020.fst'))
gt_polyids <- fst::read_fst(here::here('data', 'gt_polyids.fst'))
mod_results <- readRDS("outputs/results_stratified_models.rds")
mod_results_interaction <- readRDS("outputs/results_interaction_models.rds")


####***********************************************
#### 1: Create chloropleth maps for census ICE #### 
####***********************************************

# 1a Create id var to merge, convert to long, tidy ice var names for plotting
#    Notes: the fips_code variable in ice_census has extra zeroes compared to
#           the geoid variable in tracts. Will recreate the id variable in tracts
#           in the ice_census dataframe by pasting the state, county, and tract
#           ids together
ice_census <- ice_census %>% mutate(geoid = paste0(state_code, county_code, tract_code)) %>% 
  pivot_longer(ice_race:ice_hhincome_race, names_to = "ice_var", values_to = "ice_value") %>% 
  mutate(ice_var = case_when(
    ice_var == "ice_race" ~ "Race/Ethnicity",
    ice_var == "ice_hhincome" ~ "Household Income",
    ice_var == "ice_hhincome_race" ~ "Race/Income Combined"))

# 1b Merge ICE census vars to census tracts geometry file
ice_census_chloropleth <- tracts %>% full_join(ice_census, by = "geoid")
  
# 1c Create faceted chloropleth map -- all of NYC
ice_census_chloropleth_map <- ice_census_chloropleth %>% 
  filter(!is.na(ice_var)) %>% 
  ggplot() +
  geom_sf(aes(fill = ice_value), lwd = 0) +
  scale_fill_viridis_c(name = "", 
                       option = "inferno",
                       breaks = c(-1, 1),
                       labels = c("Syst. \nDisadvantaged \n(-1)", 
                                  "Syst. \nPriveleged \n(+1)")) +
  facet_grid(~ice_var) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
  
ice_census_chloropleth_map

# 1d Create faceted chloropleth map -- gt capture area only 
ice_census_chloropleth_gtcapture_map <- ice_census_chloropleth %>% 
  filter(!is.na(ice_var)) %>% filter(geoid %in% gt_polyids$poly_id) %>% 
  ggplot() +
  geom_sf(aes(fill = ice_value), lwd = 0) +
  scale_fill_viridis_c(name = "", 
                       option = "inferno",
                       breaks = c(-.74, 1),
                       labels = c("Syst. \nDisadvantaged \n(-0.74)", 
                                  "Syst. \nPriveleged \n(+1)")) +
  facet_grid(~ice_var) +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 16))
  
ice_census_chloropleth_gtcapture_map

# 1e Save maps
# 1e.i Save census chloropleth for all nyc
tiff("./figures/ice_census_chloropleth_map.tiff",
     units = "in", width = 12, height = 7, res = 300)
ice_census_chloropleth_map
dev.off()
# 1e.ii Save census chloropleth for gt capture area only
tiff("./figures/ice_census_chloropleth_gtcapture_map.tiff",
     units = "in", width = 12, height = 7, res = 300)
ice_census_chloropleth_gtcapture_map
dev.off()


####***************************************************************
#### 2: Create maps comparing GT raster to GT ICE for Polygons #### 
####***************************************************************

# 2a Plot Google Traffic raster 
# 2a.i Read in Google Traffic image and convert to raster
gtraster <- png::readPNG(here::here('data', 'CCC_03_17_20__06:30.png'))
gtraster <- raster(gtraster) 
# 2a.ii Set extent of raster
load(here::here('data', 'gt_extent.RData'))
extent(gtraster) <- c(gt_extent[1], gt_extent[2], gt_extent[3], gt_extent[4])
# 2a.iii Convert pixel values back to true color and make factor
gtraster <- raster::calc(gtraster, fun = function(x){x*256})
gtraster <- raster::calc(gtraster, fun = function(x){as.factor(x)})
# 2a.iv Plot and save Google Traffic raster
tiff("./figures/raster_031720_0630.tiff",
     units = "in", width = 8, height = 7, res = 300)
plot(gtraster, 
     col = c('firebrick4', 'brown1', 'orange', 'green', 'grey60', 'grey94', 'grey94', 'grey94', 'white'))
dev.off()

# 2b Plot Google Traffic polygon
# 2b.i Restrict to same date as Google Traffic image used for raster plot
ice_gt_031720_0630 <- ice_gt %>% filter(captured_datetime == ymd_hms("2020-03-17 06:30:00"))
# 2b.ii Merge gt data to census tracts geometry file
tracts_ice_gt_031720_0630 <- tracts %>% 
  right_join(ice_gt_031720_0630, by = c("geoid" = "poly_id"))
# 2b.iii Determine min and max mean gt_ice values for plot legend
min_ice_gt_031720_0630 <- min(tracts_ice_gt_031720_0630$ice_gt, na.rm = T)
max_ice_gt_031720_0630 <- max(tracts_ice_gt_031720_0630$ice_gt, na.rm = T)
# 2b.iii Plot chloropleth of ice_gt fro 03-17-20 6:30
polygon_031720_0630 <- tracts_ice_gt_031720_0630 %>% 
  filter(!is.na(ice_gt)) %>% 
  ggplot() + geom_sf(aes(fill = ice_gt), lwd = 0) + 
  scale_fill_gradient2(name = "Traffic \n Congestion",
                       breaks = c(min_ice_gt_031720_0630, max_ice_gt_031720_0630),
                       labels = c("Medium", "Free-flowing"),
                       low = "red4", mid = "darkorange2", high = "green", 
                       midpoint = 0, space = "Lab", na.value = "grey50", 
                       guide = "colourbar", aesthetics = "fill") +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 20))
# 2b.iv Save
tiff("./figures/polygons_031720_0630.tiff",
     units = "in", width = 8, height = 7, res = 300)
polygon_031720_0630
dev.off()


####****************************************
#### 3: Create pre-post NY on PAUSE map #### 
####****************************************

# 3a Restrict to two dates before and after NY on PAUSE
#    Note: we choose the same hour and day of week
ice_gt_prepost <- ice_gt %>% 
  filter(captured_datetime == ymd_hms("2020-03-10 09:30:00") |
           captured_datetime == ymd_hms("2020-03-24 09:30:00"))

# 3b Merge gt data to census tracts geometry file
tracts_ice_gt_prepost <- tracts %>% 
  right_join(ice_gt_prepost, by = c("geoid" = "poly_id"))

# 3c Determine min and max mean gt_ice values for plot legend
min_ice_gt_prepost <- min(tracts_ice_gt_prepost$ice_gt, na.rm = T)
max_ice_gt_prepost <- max(tracts_ice_gt_prepost$ice_gt, na.rm = T)

# 3d Plot faceted chloropleth
prepost_plot <- tracts_ice_gt_prepost %>% 
  filter(!is.na(ice_gt)) %>% filter(!is.na(captured_datetime)) %>% 
  ggplot() + geom_sf(aes(fill = ice_gt), lwd = 0) + 
  facet_wrap(~as.Date(captured_datetime)) +
  scale_fill_gradient2(name = "Traffic \n Congestion",
                       breaks = c(min_ice_gt_prepost, max_ice_gt_prepost),
                       labels = c("Congestion (-.5)", "Free-flowing (+.9)"),
                       low = "red", mid = "darkorange2", high = "green", 
                       midpoint = 0, space = "Lab", na.value = "grey50", 
                       guide = "colourbar", aesthetics = "fill") +
  theme_void() +
  theme(panel.grid = element_line(color = "transparent"),
        text = element_text(size = 20))

prepost_plot

# 3e Save
tiff("./figures/prepost_polygons_map.tiff",
     units = "in", width = 12, height = 7, res = 300)
prepost_plot
dev.off()


####**************************************
#### 4: Create stratified model plots #### 
####**************************************

# Create color palette
cbPalette <- c("#D3D3D3", "#E69F00")

# 4a Race plot
# 4a.i Prepare race_low dataframe
mod_results_race_low <- mod_results %>% 
  dplyr::select(strata, poly_id) %>% 
  filter(strata == "race_low") %>% 
  unnest(cols = poly_id) %>% 
  full_join(tracts, by = c("poly_id" = "geoid")) %>% 
  filter(poly_id %in% gt_polyids$poly_id) %>%   
  mutate(color = ifelse(strata == "race_low", "yes", "no"),
         color = ifelse(is.na(color), "no", color),
         facet = c("Race ICE: 20th Percentile")) 
# 4a.ii Prepare race_high dataframe
mod_results_race_high <- mod_results %>% 
  dplyr::select(strata, poly_id) %>% 
  filter(strata == "race_high") %>% 
  unnest(cols = poly_id) %>% 
  full_join(tracts, by = c("poly_id" = "geoid")) %>% 
  filter(poly_id %in% gt_polyids$poly_id) %>%   
  mutate(color = ifelse(strata == "race_high", "yes", "no"),
         color = ifelse(is.na(color), "no", color),
         facet = c("Race ICE: 80th Percentile")) 
# 4a.iii Bind dataframes together
mod_results_race <- mod_results_race_low %>% 
  bind_rows(mod_results_race_high)
# 4a.iv Create annotation dataframe
labels_race <- mod_results %>% filter(strata == "race_low" | strata == "race_high") %>% 
  mutate(beta = round(beta, digits = 2),
         lci = round(lci, digits = 2),
         uci = round(uci, digits = 2),
         facet = case_when(strata == "race_low" ~ "Race ICE: 20th Percentile",
                            strata == "race_high" ~ "Race ICE: 80th Percentile"),
         effects = paste0(beta, " 95% CI: ", lci, ", ", uci),
         facet = factor(facet)) %>% 
  dplyr::select(facet, effects)
# 4a.iii Create plot
mod_race_plot <- mod_results_race %>% 
  ggplot() + geom_sf(aes(geometry = geometry, fill = color), lwd = 0) + 
  facet_wrap(~facet) +
  scale_fill_manual(values=cbPalette) +
  geom_label(data = labels_race, aes(label = effects),
             x = Inf, y = -Inf, hjust=1, vjust=0,
             inherit.aes = FALSE) +
  theme_void() +
  theme(text = element_text(size = 16),
        legend.position = "none")
mod_race_plot
# 4a.iv Save plot
tiff("./figures/strat_results_race_map.tiff",
     units = "in", width = 12, height = 7, res = 300)
mod_race_plot
dev.off()

# 4b Hhincome plot
# 4b.i Prepare hhincom_low dataframe
mod_results_hhincome_low <- mod_results %>% 
  dplyr::select(strata, poly_id) %>% 
  filter(strata == "income_low") %>% 
  unnest(cols = poly_id) %>% 
  full_join(tracts, by = c("poly_id" = "geoid")) %>% 
  filter(poly_id %in% gt_polyids$poly_id) %>%   
  mutate(color = ifelse(strata == "income_low", "yes", "no"),
         color = ifelse(is.na(color), "no", color),
         facet = c("HH Income ICE: 20th Percentile")) 
# 4b.ii Prepare hhincome_high dataframe
mod_results_hhincome_high <- mod_results %>% 
  dplyr::select(strata, poly_id) %>% 
  filter(strata == "income_high") %>% 
  unnest(cols = poly_id) %>% 
  full_join(tracts, by = c("poly_id" = "geoid")) %>% 
  filter(poly_id %in% gt_polyids$poly_id) %>%   
  mutate(color = ifelse(strata == "income_high", "yes", "no"),
         color = ifelse(is.na(color), "no", color),
         facet = c("HH Income ICE: 80th Percentile")) 
# 4b.iii Bind dataframes together
mod_results_income <- mod_results_hhincome_low %>% 
  bind_rows(mod_results_hhincome_high)
# 4b.iv Create annotation dataframe
labels_income <- mod_results %>% filter(strata == "income_low" | strata == "income_high") %>% 
  mutate(beta = round(beta, digits = 2),
         lci = round(lci, digits = 2),
         uci = round(uci, digits = 2),
         facet = case_when(strata == "income_low" ~ "HH Income ICE: 20th Percentile",
                           strata == "income_high" ~ "HH Income ICE: 80th Percentile"),
         effects = paste0(beta, " 95% CI: ", lci, ", ", uci),
         facet = factor(facet)) %>% 
  dplyr::select(facet, effects)
# 4b.iii Create plot
mod_income_plot <- mod_results_income %>% 
  ggplot() + geom_sf(aes(geometry = geometry, fill = color), lwd = 0) + 
  facet_wrap(~facet) +
  scale_fill_manual(values=cbPalette) +
  geom_label(data = labels_income, aes(label = effects),
             x = Inf, y = -Inf, hjust=1, vjust=0,
             inherit.aes = FALSE) +
  theme_void() +
  theme(text = element_text(size = 16),
        legend.position = "none")
mod_income_plot
# 4b.iv Save plot
tiff("./figures/strat_results_income_map.tiff",
     units = "in", width = 12, height = 7, res = 300)
mod_income_plot
dev.off()

# 4c Combined income race plot
# 4c.i Prepare comb_low dataframe
mod_results_comb_low <- mod_results %>% 
  dplyr::select(strata, poly_id) %>% 
  filter(strata == "comb_low") %>% 
  unnest(cols = poly_id) %>% 
  full_join(tracts, by = c("poly_id" = "geoid")) %>% 
  filter(poly_id %in% gt_polyids$poly_id) %>%   
  mutate(color = ifelse(strata == "comb_low", "yes", "no"),
         color = ifelse(is.na(color), "no", color),
         facet = c("Comb. ICE: 20th Percentile")) 
# 4c.ii Prepare comb_high dataframe
mod_results_comb_high <- mod_results %>% 
  dplyr::select(strata, poly_id) %>% 
  filter(strata == "comb_high") %>% 
  unnest(cols = poly_id) %>% 
  full_join(tracts, by = c("poly_id" = "geoid")) %>% 
  filter(poly_id %in% gt_polyids$poly_id) %>%   
  mutate(color = ifelse(strata == "comb_high", "yes", "no"),
         color = ifelse(is.na(color), "no", color),
         facet = c("Comb. ICE: 80th Percentile")) 
# 4c.iii Bind dataframes together
mod_results_comb <- mod_results_comb_low %>% 
  bind_rows(mod_results_comb_high)
# 4c.iv Create annotation dataframe
labels_comb <- mod_results %>% filter(strata == "comb_low" | strata == "comb_high") %>% 
  mutate(beta = round(beta, digits = 2),
         lci = round(lci, digits = 2),
         uci = round(uci, digits = 2),
         facet = case_when(strata == "comb_low" ~ "Comb. ICE: 20th Percentile",
                           strata == "comb_high" ~ "Comb. ICE: 80th Percentile"),
         effects = paste0(beta, " 95% CI: ", lci, ", ", uci),
         facet = factor(facet)) %>% 
  dplyr::select(facet, effects)
# 4c.iii Create plot
mod_comb_plot <- mod_results_comb %>% 
  ggplot() + geom_sf(aes(geometry = geometry, fill = color), lwd = 0) + 
  facet_wrap(~facet) +
  scale_fill_manual(values=cbPalette) +
  geom_label(data = labels_comb, aes(label = effects),
             x = Inf, y = -Inf, hjust=1, vjust=0,
             inherit.aes = FALSE) +
  theme_void() +
  theme(text = element_text(size = 16),
        legend.position = "none")
mod_comb_plot
# 4c.iv Save plot
tiff("./figures/strat_results_comb_map.tiff",
     units = "in", width = 12, height = 7, res = 300)
mod_comb_plot
dev.off()


####*************************************
#### 5: Google traffic coverage area #### 
####*************************************

# 5a Create variable indicating if census tract is in GT capture area
gt_polyids <- gt_polyids %>% filter(!is.na(poly_id)) %>% 
  mutate(capture_area = "yes")

# 5b Merge gt data to census tracts geometry file
tracts_gt <- tracts %>% full_join(gt_polyids, by = c("geoid" = "poly_id"))

# 5c Create plot and save
tiff("./figures/gt_capture_area.tiff",
     units = "in", width = 12, height = 8, res = 300)
tracts_gt %>% 
  ggplot() + 
  geom_sf(aes(fill = capture_area)) + 
  theme_void() + 
  theme(legend.position = "none")
dev.off()


####**************************
#### 6: Interaction Plots #### 
####**************************

# 6a Create variables for plot
#    Need a variable for effect modifier values, the effect of the intervention
#    at those values of the effect modifier, the standard error for those effects,
#    and the upper and lower confidence intervals 
interaction_plot_df <- mod_results_interaction %>% 
  slice(rep(1:n(), each = 21)) %>% # replicate rows
  mutate(
    ice_value = rep(seq(from = -1, to = 1, by = .1), 3),
    intervention_effect = beta_intervention + (beta_interaction*ice_value),
    intervention_effect_se = sqrt(var_intervention + 
                                    (ice_value^2 * var_interaction) +
                                    (2 * ice_value * covar_intervention_interaction)),
    lci = intervention_effect - 1.96 * intervention_effect_se,
    uci = intervention_effect + 1.96 * intervention_effect_se
  )

# 6b Create plot
tiff("./figures/interaction_plot.tiff",
     units = "in", width = 12, height = 8, res = 300)
interaction_plot_df %>% 
  ggplot(aes(x = ice_value, y = intervention_effect)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2) +
  facet_grid(~model) +
  ylab("NY on PAUSE Effect Estimate") + xlab("ICE Value") + 
  theme_bw(base_size = 16)
dev.off()



