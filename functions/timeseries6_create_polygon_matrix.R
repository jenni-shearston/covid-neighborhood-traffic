# Function: create_polygon_matrix for a polygons_of_interest shapefile
# Project: Acquisition and Analysis of Crowd-Sourced Traffic Data at Varying Spatial Scales
# Script Authors: Sebastian T. Rowland and Jenni A. Shearston 
# Updated: 03/02/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 1: Function to Convert polygons_of_interest to Matrix of gt_image_cat Dimensions

####**************
#### N: Notes #### 
####**************

# This function converts a polygon shapefile (polygons_of_interest) to a matrix with
# the dimensions and resolution of a matrix of a traffic image (gt_image_cat)

####**************************************************************************************
#### 1: Function to Convert polygons_of_interest to Matrix of gt_image_cat Dimensions #### 
####**************************************************************************************

# BEGIN FUNCTION 

create_polygon_matrix <- function(polygons_of_interest_path = polygons_of_interest_path,
                                  poly_id_var = poly_id_var,
                                  gt_geo_projected_path = gt_geo_projected_path,
                                  gt_image_cat_path = gt_image_cat_path,
                                  poly_matrix_output_path = poly_matrix_output_path
                                    ) {
  
  
  ####**************************************
  #### 2: Prepare Shapefile of Interest #### 
  ####**************************************
  
  # 2a Bring in shapefile of interest
  polygons_of_interest <- st_read(polygons_of_interest_path)
  
  ####*******************************************
  #### 3: Prepare Polygon and Raster Inputs #### 
  ####*******************************************
  
  # 3a Rename the poly_id and remove non-essential variables 
  polygons_of_interest <- polygons_of_interest %>% 
    dplyr::rename(poly_id = !!poly_id_var) %>% 
    dplyr::select(poly_id)
  
  # 3b Make poly_id numeric
  polygons_of_interest <- polygons_of_interest %>% 
    dplyr::mutate(poly_id = as.numeric(poly_id))
  
  # 3c Read in gt_geo_projected
  #    Note: 'gt_geo_projected' must be a projected using WGS84
  gt_geo_projected <- raster::raster(gt_geo_projected_path)
  
  # 3d Extract the extent (min and max lat and long) of gt_geo_projected 
  #    Note: We use gt_geo_projected's extent because we have carefully 
  #          georeferenced it
  gt_extent <- raster::extent(gt_geo_projected)
  
  # 3e Read a gt_image_cat.png as a matrix
  #    Note: The actual datetime of this image is arbitrary. 
  gt_matrix_cat <- png::readPNG(gt_image_cat_path)
  
  # 3f Convert gt_matrix_unprojected to raster
  gt_raster_unprojected <- raster::raster(gt_matrix_cat) 
  
  # 3g Change the extent of gt_raster_unprojected to reflect the extent of the gt_extent 
  #    Note: Now we are converting it to lat long 
  #          Here we georeference gt_raster_unprojected based on the location of 
  #          gt_geo_projected which was georeferenced carefully
  raster::extent(gt_raster_unprojected) <- c(gt_extent[1], gt_extent[2], gt_extent[3], gt_extent[4])
  
  # 3h Convert polygons_of_interest CRS to WGS84 
  #    Note: This is important because gt_raster_projected has a CRS of WGS84 which 
  #          uses lat/long and not decimal degrees or feet 
  #          Because gt_raster_unprojected has been assigned the extent of 
  #          gt_raster_projected, which is in lat/long, the polygons_of_interest
  #          file must have a CRS in lat/long in order for the rasterize
  #          command to clip the shapefile to the gt_raster_unprojected's extent
  polygons_of_interest_wgs84 <- sf::st_transform(polygons_of_interest, "WGS84")
  
  # 3i Convert polygons_of_interest to a raster with the dimensions and 
  #    resolution of gt_raster_unprojected (~ 30s to 2min)
  #    Note: The resulting raster will only have cells within the area defined by the 
  #          overlap of gt_raster_unprojected and the polygons of interest 
  #          The raster will have one column - the id of the polygon the cell belongs to
  poly_gt_crosswalk <- raster::rasterize(polygons_of_interest_wgs84, 
                                         gt_raster_unprojected, field = "poly_id")
  
  # 3j Convert to matrix
  poly_matrix <- raster::as.matrix(poly_gt_crosswalk)
  
  # 3k Save matrix
  poly_matrix %>% write_rds(poly_matrix_output_path)
  
  # 3l Return matrix
  return(poly_matrix)
  
  # 3m Remove no longer needed files and run garbage collection to 
  #    return memory 
  #    Note: This is critical to conserve memory
  rm(poly_gt_crosswalk, polygons_of_interest_wgs84, 
     gt_raster_unprojected, gt_extent, gt_geo_projected,
     gt_matrix_cat)
  gc()
  
}

# END FUNCTION



