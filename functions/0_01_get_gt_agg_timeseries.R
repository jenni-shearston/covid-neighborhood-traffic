# Function: get_gt_agg_timeseries for set of captured datetimes
# Aggregating Google Traffic Data to Polygons
# Sebastian T. Rowland and Jenni A. Shearston 
# Updated 11/05/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Read polygons_of_interest
# 2: Load gt_geo_projected
# 3: Georeference a gt_image_cat
# 4: Intersect polygons_of_interest with gt_raster_unprojected
# 5: Create gt_agg_timeseries and save 


####**************
#### N: Notes #### 
####**************

# 


####********************
#### 0: Preparation #### 
####********************

# 0a Load Packages
# packages <- c("tidyverse", "raster", "rgdal", "terra", "sf", 'here', 'tictoc')
# lapply(packages, library, character.only = TRUE)


####**********************************
#### 1: Read polygons_of_interest #### 
####**********************************


# BEGIN FUNCTION 
get_gt_agg_timeseries <- function( #polygons_of_interest, 
                                   #poly_id_var, 
                                   captured_datetime_vector_filename, 
                                   dir_output, 
                                   name_output) {
  # 1a set values for the function's arguments 
  # these will become the default values of the arguments once it is a function
  #dir_poly_of_interest <- "nyc_census_tracts" 
  #name_poly_of_interest <- "nycgeo_census_tracts.shp" 
  #poly_id_var <- 'geoid'
  # 
  
  # 1a read in polygons_of_interest
  #polygons_of_interest <- sf::st_read(here::here("data", "polygons_of_interest", 
   #                                          dir_poly_of_interest,
    #                                         name_poly_of_interest))
  
  # 1b plot to confirm success
 # plot(polygons_of_interest, main = "polygons_of_interest")
  
 #  # 1d Rename the poly_id and remove non-essential variables 
 #  polygons_of_interest <- polygons_of_interest %>% 
 #    dplyr::rename(poly_id = !!poly_id_var) %>% 
 #    dplyr::select(poly_id)
 #  
 #  # 1d make poly_id numeric
 #  polygons_of_interest <- polygons_of_interest %>% 
 #    dplyr::mutate(poly_id = as.numeric(poly_id))
 #  
 #  
 #  ####******************************
 #  #### 2: Load gt_geo_projected #### 
 #  ####******************************
 #  
 #  # 2a read in gt_geo_projected
 #  gt_geo_projected <- raster::raster(here::here("data", "gt_geo_projected.tif"))
 #  
 #  # 2b plot to confirm success
 #  #gt_geo_projected_plot <- raster::calc(gt_geo_projected, fun = function(x){as.factor(x)})
 #  #plot(gt_geo_projected_plot, main = "gt_geo_projected", 
 #  #     col = c('firebrick4', 'brown1',  'orange', 'green', 'grey60', 'grey94', 'grey94', 'grey94', 'white'))
 #  
 #  # 2c high five your colleague 
 #  
 #  # 2d extract the extent (min and max lat and long) of gt_geo_projected 
 #  #    Note: we use gt_geo_projected's extent because we worked to get it right in QGIS
 #  gt_extent <- raster::extent(gt_geo_projected)
 #  
 #  ####************************************
 #  #### 3: Georeference a gt_image_cat #### 
 #  ####************************************
 #  
 #  # 3a read the gt_image_cat.png as a matrix
 #  # the actual datetime of this image is arbitrary
 #  gt_matrix_cat <- png::readPNG(here::here('data', 'CCC_01_01_18__02_00.png'))
 #  
 #  # 3b convert gt_matrix_unprojected to raster
 #  gt_raster_unprojected <- raster::raster(gt_matrix_cat) 
 #  
 #  # 3c change the extent of gt_raster_unprojected to reflect the extent of the gt_extent 
 #  #    Note: now we are converting it to lat long 
 #  #          here we georeference gt_raster_unprojected based on the location of gt_geo_projected
 #  #          which was georeferenced in QGIS with 4 points
 #  raster::extent(gt_raster_unprojected) <- c(gt_extent[1], gt_extent[2], gt_extent[3], gt_extent[4])
 #  
 #  # 3e plot 
 #  # 3e.i recover original gt values
 #  #    Note: because gt_raster_unprojected was read into R as a png, the values
 #  #          of each pixel were divided by 256 (the max value for a png), so that 
 #  #          all values were between 0 and 1
 #  #gt_raster_unprojected_plot <- raster::calc(gt_raster_unprojected, fun = function(x){x*256})
 #  # 3e.ii covert values to factor so legend is categorical
 #  #gt_raster_unprojected_plot <- raster::calc(gt_raster_unprojected, fun = function(x){as.factor(x)})
 #  # 3e.iii make the plot
 # # plot(gt_raster_unprojected_plot, main = "gt_raster_unprojected", 
 #  #     col = c('firebrick4', 'brown1', 'orange', 'green', 'grey60', 'grey94', 'grey94', 'grey94', 'white'))
 #  
 #  
 #  ####******************************************************************
 #  #### 4: Intersect polygons_of_interest with gt_raster_unprojected #### 
 #  ####******************************************************************
 #  
 #  # 4a convert polygons_of_interest CRS to WGS84 
 #  # Note: this is important because gt_raster_projected has a CRS of WGS84 which 
 #  #       uses lat/long and not decimal degrees or feet 
 #  #       because gt_raster_unprojected has been assigned the extent of 
 #  #       gt_raster_projected, which is in lat/long, the polygons_of_interest
 #  #       file must have a CRS in lat/long in order for the rasterize
 #  #       command to clip the shapefile to the gt_raster_unprojected's extent
 #  polygons_of_interest_wgs84 <- sf::st_transform(polygons_of_interest, "WGS84")
 #  
 #  # 4b convert polygons_of_interest to a raster with the dimensions and 
 #  # resolution of gt_raster_unprojected (~ 30s to 2min)
 #  # the resulting raster will only have cells within the area defined by the 
 #  # overlap of gt_raster_unprojected and the polygons of interest 
 #  # The raster will have one column - the id of the polygon the cell belongs to
 #  tic('completed rasterization')
 #  poly_gt_crosswalk <- raster::rasterize(polygons_of_interest_wgs84, gt_raster_unprojected, field = "poly_id")
 #  
 #  # 4c plot 
 #  #plot(poly_gt_crosswalk, main = "poly_gt_crosswalk")
 #  
 #  # 4d convert to matrix
 #  poly_matrix <- raster::as.matrix(poly_gt_crosswalk)
 #  toc()
  
  ####******************************************
  #### 5: Create gt_agg_timeseries and save #### 
  ####******************************************
  
 # 5a collect the polygon-aggregation of pixel color codes for each of the 
  #    captured_datetimes of interest
  #    this is our map command; if this is slow we can also try pmap()
  # 5b.i initialize a dataframe to fill
  gt_agg_timeseries <- data.frame(poly_id = NA, 
                                  gt_pixcount_maroon = NA, 
                                  gt_pixcount_red= NA, 
                                  gt_pixcount_orange = NA, 
                                  gt_pixcount_green = NA, 
                                  gt_pixcount_gray= NA, 
                                  gt_pixcount_construction = NA, 
                                  gt_pixcount_emergency = NA, 
                                  gt_pixcount_notsampled = NA, 
                                  gt_pixcount_background = NA, 
                                  gt_pixcount_tot = NA, 
                                  captured_datetime = NA)
  
  # 5b.ii collect the gt_agg in a loop
  for (i in 1:length(captured_datetime_vector_filename)) {
    gt_agg_timeseries <- gt_agg_timeseries %>% 
      dplyr::bind_rows(get_gt_agg_timepoint(
        captured_datetime_vector_filename[i], 
        poly_matrix
        #gt_matrix_cat
        ))
    if (i%%50 ==0) {print(captured_datetime_vector_filename[i])}
  }

  # 5c Save out gt_agg_timeseries
  gt_agg_timeseries %>% 
    dplyr::filter(!is.na(captured_datetime)) %>%
    fst::write_fst(here::here(dir_output, 
                         paste0(name_output, '.fst')))

}

