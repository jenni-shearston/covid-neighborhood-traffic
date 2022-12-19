# Function: Obtain gt_agg_timeseries for a captured_datetime
# Aggregating Google Traffic Data to Polygons
# Sebastian T. Rowland and Jenni A. Shearston 
# Updated 11/05/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 1: Function to create the gt_agg_timeseries for a captured_datetime


####**************
#### N: Notes #### 
####**************

# This function aggregates gt_cat values (e.g., values from gt_image_cat files)
# by unique polygon ids for an inputted captured_datetime.


####***************************************************************************
#### 1: Function to create the gt_agg_timeseries for one captured_datetime #### 
####***************************************************************************

get_gt_agg_timepoint <- function(captured_datetime_filename, poly_matrix 
                                 #gt_matrix_cat
                                 ) {
  
  # example value 
  #captured_datetime <- '01_01_18__02_00'

  # 1a Pull datetime from filename
  captured_datetime <- captured_datetime_filename %>% 
    str_remove_all('.png') %>%
    str_remove_all('[A-z]') %>%
    str_replace('[[:punct:]]', '')
  
  # 1b determine the available gt_image_cats 
  available_files <- list.files(path = '/Users/jennishearston/Dropbox/CLEAN/2020/')
  
  # 1c if the gt_image_cat is available:
  if (sum(captured_datetime_filename == available_files) == 1) {
    
    # 1c.i read the gt_image_cat.png as a matrix
    gt_matrix_cat <- png::readPNG(paste0('/Users/jennishearston/Dropbox/CLEAN/2020/', 
                                         captured_datetime_filename))
    
    # 1c.ii recover original gt values
    #    Note: because gt_matrix_cat was read into R as a png, the values
    #          of each pixel were divided by 256 (the max value for a png), so that 
    #          all values were between 0 and 1
    gt_matrix_cat <- round(gt_matrix_cat * 256, 1)
    
    # 1c.iii create the gt_agg_timeseries
    #    Note: first, we combine the poly_matrix and gt_matrix_cat matrices
    #          into a dataframe based on pixel position - in each matrix the pixel 
    #          location is indexed in the same way - rows for North-South and  
    #          columns for East-West. 
    #          second, we perform a grouped summarize by poly_id to count the number
    #          of pixels of each type for each poly_id. 
    #          gt_cat == 2 evaluates whether the pixel value is equal to 2 (red color code), 
    #          yielding a 1 if the pixel value is 2 and a 0 if not. Thus the 
    #          sum is a sum of true/false statements and yields the number of
    #          pixels of that type, in that polygon
    #          finally, a variable for captured_datetime is added
    gt_agg_timepoint <- 
      data.frame(poly_id = as.vector(poly_matrix),
                 gt_cat = as.vector(gt_matrix_cat)) %>% 
      group_by(poly_id) %>% 
      summarize(gt_pixcount_maroon       = sum(gt_cat == 1),
                gt_pixcount_red          = sum(gt_cat == 2),
                gt_pixcount_orange       = sum(gt_cat == 3),
                gt_pixcount_green        = sum(gt_cat == 4),
                gt_pixcount_gray         = sum(gt_cat == 5),
                gt_pixcount_construction = sum(gt_cat == 6),
                gt_pixcount_emergency    = sum(gt_cat == 7),
                gt_pixcount_notsampled   = sum(gt_cat == 8),
                gt_pixcount_background   = sum(gt_cat == 9),
                gt_pixcount_tot          = n()) %>% 
      mutate(captured_datetime = captured_datetime)
    
    # 1c.iv remove na poly_ids (eg, the poly_id representing pixels 
    # not within polygons_of_interest)
    gt_agg_timepoint <- gt_agg_timepoint %>% 
      filter(!is.na(poly_id))
    
  # 1d if the gt_image_cat is NOT available:
  } else if (sum(captured_datetime_filename == available_files) == 0) {
    
    # 1d.i generate NA if the gt_image_cat file for the captured_datetime is not available
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
      mutate(captured_datetime = captured_datetime)
      
  }
  
  # 1e return dataframe of aggregated timeseries 
  return(gt_agg_timepoint)
}
