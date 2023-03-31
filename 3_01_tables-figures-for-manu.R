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

# the google traffic data are snapshots in time (reconfirm w Markus after getting full
# hourly dataset). For air pollution data, the time stamp is the sample begin time
# https://aqs.epa.gov/aqsweb/documents/about_aqs_data.html#sample-vs-sample-measurement


####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages & passwords
source(paste0(project.folder, 'packages.R'))
source(paste0(project.folder, 'passwords.R'))

# 0c Set up filepath(s)
data_path <- paste0(project.folder, 'data/processed_data/')
model_path <- paste0(project.folder, 'outputs/models/')

# 0d Load data
fullData <- read_rds(paste0(data_path, 'full_dataset_wcovars_daily.rds'))



# table 1: cols = pop, prepause traf, postpause traf, perc white, perc black, median hhincome
#          rows = 5 strata for ICE and 5 strata for EJI
# fig 1: traffic pre and post pause by census tract (2-panel map)
# fig 2: 2-panel chloropleth w ICE and EJI
# fig 3: time series of traffic
# fig 4: forrest plot of strata model results
# sup table 1: coefs and cis for fig 3
# fig 5 (if different from total, if not, goes in supp): forrest plot of eji modules
# sup table 2: coefs and cis for fig 4





