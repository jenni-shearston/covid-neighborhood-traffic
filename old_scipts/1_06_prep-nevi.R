# Prepare NEVI Index
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 11/10/2022

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


####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages & passwords
source(paste0(project.folder, 'packages.R'))
source(paste0(project.folder, 'passwords.R'))

# 0c Set up filepath(s)
data_path <- paste0(project.folder, 'data/')

# 0b Load data
# 0b.i Load census tract sf file
tracts_sf <- nycgeo::nyc_boundaries(geography = "tract")
# 0b.iii Load census tracts in traffic data
traf <- read_fst(paste0(data_path, 'google_traffic_jantomar2020.fst')) %>% 
  dplyr::select(poly_id) %>% filter(!is.na(poly_id)) %>% 
  mutate(poly_id = as.character(poly_id)) %>% distinct()
# 0b.ii Load NEVI data
nevi <- read_rds(paste0(data_path, 'nevi_tract_final.rds')) %>% 
  filter(Tract_FIPS %in% traf$poly_id)

# 0d Review distribution of NEVI variables
nevi %>% ggplot(aes(x = nevi)) + geom_histogram()
nevi %>% ggplot(aes(x = nevi_cluster)) + geom_histogram(stat = 'count')
nevi %>% ggplot(aes(x = score_demo)) + geom_histogram()
nevi %>% ggplot(aes(x = score_economic)) + geom_histogram()
nevi %>% ggplot(aes(x = score_residential)) + geom_histogram()
nevi %>% ggplot(aes(x = score_healthstatus)) + geom_histogram()


####*******************************************
#### 1:  #### 
####*******************************************


# How are the NEVI score and the the clusters interpreted? 
#   low sore = low vulnerability; 2 low clusters (1 & 2), 
#   1 med cluster (3); 3 high clusters (4-6)
#   clusters: hierarchical clustering; uneven numbers of ct in each cluster
#     gap statistic
#   for full nevi score, could treat the 6 clusters as the strata, although
#     we probably dont have cluster 5 in our area
# Is race/ethnicity data incorporated? 
#   it is not included because the variables were driving the clusters
# How might I go about making categories of the NEVI score or of specific 
#   indices to use in stratification?

# currently submitting to journals (just got rejected, are currently revising and
# then will submit again to a new journal)
# plan to put it on a pre-print server soon, probably ok w siting pre-print

# let's talk about authorship / who from nevi team should be authors
# what expertise / interpretation can they help with




