# Function: reformat_captured_datetime_vector for set of captured datetimes
# Project: Acquisition and Analysis of Crowd-Sourced Traffic Data at Varying Spatial Scales
# Script Authors: Jenni A. Shearston and Sebastian T. Rowland
# Updated: 03/02/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 1: Function to Reformat the captured_datetime_vector as gt_image_cat Filenames

####**************
#### N: Notes #### 
####**************

# This function reformats the captured_datetime_vector to reflect the filenames 
# of gt_image_cat files. Recall that all datetime information for a processed gt_image_cat
# is included in the filename. In order to loop over all the filenames we want to include 
# in our timeseries, we must convert our vector of datetimes to a vector of specially 
# formatted datetimes (e.g., '2020-01-10 00:30:00 UTC' to 'CCC_01_10_20__00:30.png')

####************************************************************************************
#### 1: Function to Reformat the captured_datetime_vector as gt_image_cat Filenames #### 
####************************************************************************************

# BEGIN FUNCTION 
reformat_captured_datetime_vector <- function(captured_datetime_vector = datetimes_of_interest, 
                                              gt_dir = gt_dir) {
  
  # 1a Get datetime elements 
  dt_year <- lubridate::year(captured_datetime_vector) 
  dt_month <- lubridate::month(captured_datetime_vector) 
  dt_day <- lubridate::day(captured_datetime_vector) 
  dt_hour <- lubridate::hour(captured_datetime_vector) 
  dt_minute <- lubridate::minute(captured_datetime_vector) 
  
  # 1b Find an example gt_image_cat filename from your folder 
  filename_example <- list.files(path = gt_dir)[1]
  
  # 1c Create punctuation list 
  punctuation_list <- stringr::str_split(filename_example, '[0-9][0-9]')[[1]]
  
  # 1d Reformat captured_datetime_vector to reflect filenames of gt_image_cat files
  filenames <- paste0(punctuation_list[1], two_digit_pad(dt_month), 
                      punctuation_list[2], two_digit_pad(dt_day), 
                      punctuation_list[3], str_sub(dt_year, 3, 4), 
                      punctuation_list[4], two_digit_pad(dt_hour), 
                      punctuation_list[5], two_digit_pad(dt_minute), 
                      punctuation_list[6])
  
  # 1e Return formatted filenames
  return(filenames)
  
}
  
# END FUNCTION 





