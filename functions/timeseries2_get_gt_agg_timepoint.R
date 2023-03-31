# Function: Obtain gt_agg_timeseries for a captured_datetime
# Project: Acquisition and Analysis of Crowd-Sourced Traffic Data at Varying Spatial Scales
# Script Authors: Sebastian T. Rowland and Jenni A. Shearston 
# Updated: 03/02/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 1: Function to create the gt_agg_timeseries for a captured_datetime

####**************
#### N: Notes #### 
####**************

# This function aggregates gt_cat values (e.g., values from gt_image_cat files)
# by unique polygon ids for a single inputted captured_datetime.

####***************************************************************************
#### 1: Function to create the gt_agg_timeseries for one captured_datetime #### 
####***************************************************************************

# BEGIN FUNCTION 
get_gt_agg_timepoint <- function(captured_datetime_filename, 
                                 poly_matrix,
                                 gt_dir, 
                                 available_files) {
  
  # Example value 
  #captured_datetime_filename = 'CCC_03_24_20__21:30.png'

  # 1a Pull datetime from filename
  captured_datetime <- captured_datetime_filename %>% 
    stringr::str_remove_all('.png') %>%
    stringr::str_remove_all('[A-z]') %>%
    stringr::str_replace('[[:punct:]]', '') 
  
  # 1b If the gt_image_cat is available:
  if (captured_datetime_filename %in% available_files) {
    
    # 1b.i Read the gt_image_cat.png as a matrix
    gt_matrix_cat <- png::readPNG(here::here(gt_dir, captured_datetime_filename))
    
    # 1b.ii Recover original gt values
    #    Note: Because gt_matrix_cat was read into R as a png, the values
    #          of each pixel were divided by 255 (the max value for a png from 0-255),  
    #          so that all values were between 0 and 1.
    gt_matrix_cat <- round(gt_matrix_cat * 255, 1)
    
    # 1b.iii Create the gt_agg_timeseries
    #    Note: First, we combine the poly_matrix and gt_matrix_cat matrices
    #          into a dataframe based on pixel position - in each matrix the pixel 
    #          location is indexed in the same way - rows for North-South and  
    #          columns for East-West. 
    #          Second, we perform a grouped summarize by poly_id to count the number
    #          of pixels of each type for each poly_id. For example:
    #          gt_cat == 2 evaluates whether the pixel value is equal to 2 (red color code), 
    #          yielding a 1 if the pixel value is 2 and a 0 if not. Thus the 
    #          sum is a sum of true/false statements and yields the number of
    #          pixels of that type, in that polygon.
    #          Finally, a variable for captured_datetime is added.
    gt_agg_timepoint <- 
      data.frame(poly_id = as.vector(poly_matrix),
                 gt_cat = as.vector(gt_matrix_cat)) %>% 
      dplyr::group_by(poly_id) %>% 
      dplyr::summarize(gt_pixcount_maroon       = sum(gt_cat == 1),
                       gt_pixcount_red          = sum(gt_cat == 2),
                       gt_pixcount_orange       = sum(gt_cat == 3),
                       gt_pixcount_green        = sum(gt_cat == 4),
                       gt_pixcount_gray         = sum(gt_cat == 5),
                       gt_pixcount_construction = sum(gt_cat == 6),
                       gt_pixcount_emergency    = sum(gt_cat == 7),
                       gt_pixcount_notsampled   = sum(gt_cat == 8),
                       gt_pixcount_background   = sum(gt_cat == 255),
                       gt_pixcount_tot          = n()) %>% 
      dplyr::mutate(captured_datetime = captured_datetime) %>% 
      dplyr::select(captured_datetime, poly_id, everything())
    
    # 1b.iv Remove NA poly_ids (eg, the poly_id representing pixels 
    # not within polygons_of_interest)
    gt_agg_timepoint <- gt_agg_timepoint %>% 
      dplyr::filter(!is.na(poly_id))
    
  # 1c If the gt_image_cat is NOT available:
  } else {
    
    # 1c.i Generate NA if the gt_image_cat file for the captured_datetime is not available
    gt_agg_timepoint <- data.frame(poly_id = NA) %>% 
      mutate(gt_pixcount_maroon          = NA,
             gt_pixcount_red             = NA,
             gt_pixcount_orange          = NA,
             gt_pixcount_green           = NA,
             gt_pixcount_gray            = NA,
             gt_pixcount_construction    = NA,
             gt_pixcount_emergency       = NA,
             gt_pixcount_notsampled      = NA,
             gt_pixcount_background      = NA,
             gt_pixcount_tot             = NA) %>% 
      mutate(captured_datetime = captured_datetime)%>% 
      dplyr::select(captured_datetime, poly_id, everything())
      
  }
  
  # 1d Return dataframe of aggregated timeseries 
  return(gt_agg_timepoint)
  
}

# END FUNCTION 


