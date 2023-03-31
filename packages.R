# Packages to Load
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 11/01/2022

# List of packages to use
list.of.packages = c('broom',
                     'corrplot',
                     'data.table',
                     'doParallel',
                     'dlnm',
                     'egg',
                     'fst',
                     'furrr',
                     'future',
                     'gamm4',
                     'here',
                     'Hmisc',
                     'httr',
                     'jsonlite',
                     'lubridate',
                     'magrittr',
                     'parallel',
                     'progress',
                     'progressr',
                     'purrr',
                     'rvest',
                     'sf',
                     'splines',
                     'stringr',
                     'survival',
                     'tidycensus',
                     'tidyr',
                     'tidyverse', 
                     'viridis',
                     'wesanderson'
)

# Check if packages in list are installed, and if not, install 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
lapply(list.of.packages, require, character.only = TRUE)


