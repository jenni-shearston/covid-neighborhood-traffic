# Function: get_gt_agg_timeseries for set of captured datetimes
# Project: Acquisition and Analysis of Crowd-Sourced Traffic Data at Varying Spatial Scales
# Script Authors: Jenni A. Shearston and Sebastian T. Rowland
# Updated: 03/02/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 1: Function to Create Google Traffic timeseries for Unique Polygons

####**************
#### N: Notes #### 
####**************

# This function loops function 0_02_get_gt_agg_timepoint over all gt_image_cat 
# filenames included in the captured_datetime_vector, creating a timeseries of
# Google Traffic data aggregated by unique polygon ids (poly_id) in the 
# poly_matrix file 

####*************************************************************************
#### 1: Function to Create Google Traffic timeseries for Unique Polygons #### 
####*************************************************************************

# BEGIN FUNCTION 
get_gt_agg_timeseries <- function(captured_datetime_vector_filename = datetimes_of_interest, 
                                  gt_agg_timeseries_output_path = gt_agg_timeseries_output_path,
                                  gt_dir = gt_dir,
                                  method = 'parallel',
                                  poly_matrix = poly_matrix) {
  
  # 1a Determine the available gt_image_cats 
  available_files <- list.files(gt_dir)
  
  # OPTION 1
  
  # Use forloop 
  if (method == 'forloop') {
  
    # 1b Initialize a dataframe to fill
    gt_agg_timeseries <- data.frame(captured_datetime = NA,
                                    poly_id = NA,
                                    gt_pixcount_maroon = NA,
                                    gt_pixcount_red= NA,
                                    gt_pixcount_orange = NA,
                                    gt_pixcount_green = NA,
                                    gt_pixcount_gray= NA,
                                    gt_pixcount_construction = NA,
                                    gt_pixcount_emergency = NA,
                                    gt_pixcount_notsampled = NA,
                                    gt_pixcount_background = NA,
                                    gt_pixcount_tot = NA)

    # 1c Collect the timeseries in a loop over each gt_image_cat filename
    for (i in 1:length(captured_datetime_vector_filename)) {
      gt_agg_timeseries <- gt_agg_timeseries %>%
        dplyr::bind_rows(get_gt_agg_timepoint(
          captured_datetime_vector_filename[i],
          poly_matrix,
          gt_dir, 
          available_files))
      if (i%%50 == 0) {print(captured_datetime_vector_filename[i])}
    }
  
  # OPTION 2
  # Use parallelization
  } else if (method == 'parallel') {
    
    # 1b Set up parallelization
    # 1b.i Get the number of cores
    #      Note: We subtract one to reserve a core for other tasks
    n.cores <- parallel::detectCores() - 1
    # 1b.ii Create the cluster
    my.cluster <- parallel::makeCluster(
      n.cores, 
      type = "FORK")
    # 1b.iii Register it to be used by %dopar%
    doParallel::registerDoParallel(cl = my.cluster)
  
    #1c Collect the timeseries in parallel over each gt_image_cat filename
    gt_agg_timeseries <- 
      foreach(
        i = 1:length(captured_datetime_vector_filename),
        .combine = 'rbind'
      ) %dopar% {
        get_gt_agg_timepoint(captured_datetime_vector_filename[i],
                             poly_matrix,
                             gt_dir, 
                             available_files)
        }
     stopCluster(my.cluster)
  }
  
  # FOR EITHER OPTION
  
  # 1d Save out gt_agg_timeseries
  gt_agg_timeseries %>% 
    dplyr::filter(!is.na(captured_datetime)) %>%
    fst::write_fst(gt_agg_timeseries_output_path)

}

# END FUNCTION 




