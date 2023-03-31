# Function: make_captured_datetime_vector
# Project: Acquisition and Analysis of Crowd-Sourced Traffic Data at Varying Spatial Scales
# Script Authors: Jenni A. Shearston and Sebastian T. Rowland
# Updated: 03/02/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 1: Function to Make Vector of Captured Datetimes for Timeseries

####**************
#### N: Notes #### 
####**************

# This function creates a vector of datetimes to be included in the time series.
# Users specify either a base_date (begin date) and an end_date, or 
# a base_date and sampling strategy. The sampling strategy involves specifying
# a quantity, unit, and direction (e.g., '3 days forward'), which when combined 
# with the base_date, can be used to calculate an end_date.

####*********************************************************************
#### 1: Function to Make Vector of Captured Datetimes for Timeseries #### 
####*********************************************************************

# BEGIN FUNCTION 
make_captured_datetime_vector <- function(base_date = '2020/01/10 00:30',
                                          end_date = 'none', 
                                          timezone = 'America/Eastern',
                                          sampling_quantity_units_direction = 'none') {

  # Instead of entering an end_date, you can specify sampling_quantity_units_direction
  # This allows a user to specify an end_date based on a quantity of time units, e.g.: '3 days forward'
  # The parameter is specified with an underscore between the quantity, unit, and direction
  # sampling_quantity_units_direction = "3_weeks_forward"
 
  # 1a Catch some errors
  if (end_date == 'none' & sampling_quantity_units_direction == 'none') stop('pick a way to sample')
  if (end_date != 'none' & sampling_quantity_units_direction != 'none') stop('pick one way to sample')
  
  # 1b Change the base_date to POSIXct format
  base_date <- base_date %>% 
    lubridate::parse_date_time('ymd HM', tz = timezone) 
 
  # 1c Change the end_date to POSIXct format
  if (end_date !='none') {
    end_date <- end_date %>% 
      lubridate::parse_date_time('ymd HM', tz = timezone) 
  }
  
  # 1d Get end_date if end_date is defined by sampling_quantity_units_direction
  if (sampling_quantity_units_direction != 'none') {
    # 1d.i Separate each parameter from sampling_quantity_units_direction
    quantity <- stringr::str_split_fixed(sampling_quantity_units_direction, '_', 3)[1] %>% as.numeric()
    sampling_units <- stringr::str_split_fixed(sampling_quantity_units_direction, '_', 3)[2]
    direction <- stringr::str_split_fixed(sampling_quantity_units_direction, '_', 3)[3]
    
    # 1d.ii Error messages
    if (is.na(quantity)) stop('quantity must be a number (e.g. 3 not three)')
    if (!sampling_units %in% c('hours', 'days', 'weeks', 'months', 'years')) stop('pick an allowed sampling unit: hours, days, weeks, months, years')
    if (!direction %in% c('forward', 'backward')) stop('pick an allowed sampling direction: forward, backward')
    
    # 1d.iii Create sampling_duration
    if (sampling_units == 'hours') {
      sampling_duration <- lubridate::duration(hours = quantity)
    } else if (sampling_units == 'days') {
      sampling_duration <- lubridate::duration(days = quantity)
    } else if (sampling_units == 'weeks') {
      sampling_duration <- lubridate::duration(weeks = quantity)
    } else if (sampling_units == 'months') {
      sampling_duration <- lubridate::duration(months = quantity)
    } else if (sampling_units == 'years') {
      sampling_duration <- lubridate::duration(years = quantity)
    }
    
    # 1d.iv Calculate end_date from base_date and sampling_duration
    if(direction == 'forward') {
      end_date = base_date + sampling_duration
    } else if(direction == 'backwards') {
      end_date = base_date - sampling_duration
    }
  }
  
  # 1e Get sampling times
  datetime_vector <- seq(base_date, end_date, 60*60) 
  
  # 1f return vector
  return(datetime_vector)
}
                            
# END FUNCTION


              