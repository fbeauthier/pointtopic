# Wrote a function that finds the proportion of postcodes from parts of old boundaries that make up new boundaries
# this function can be used to translate population-related metrics from old boundaries to new boundaries

# required packages
library(tidyverse)
library(rgdal)  
library(terra)
library(raster) 
library(sf)
library(st)
library(shapefiles)

# the function requires 3 inputs: old boundaries, new boundaries and postcodes, all as .shp
# load tables
# old boundaries
old <- st_read(" ") # insert file path to old boundaries .shp
# new boundaries
new <- st_read(" ") # insert file path to new boundaries .shp
# postcodes
postcodes <- st_read(" ") # insert file path to postcodes .shp (points)

# define the dataframe columns needed: boundary code column as strings
# e.g. old_code_col = "SOA_CODE"
# insert old boundaries code column
old_code_col = " "
# insert new boundaries code column
new_code_col = " "
# insert postcodes code column
pcd_col = " "

translate_geo <- function(old, new, postcodes){
  # make sure CRS for all three datasets are equal
  sf_use_s2(FALSE)
  old <- st_transform(old, crs = 4326)
  new <- st_transform(new, crs = 4326)
  postcodes <- st_transform(postcodes, crs = 4326)
  
  # get number of postcodes in new boundaries
  # st_join postcodes in new boundaries
  new_w_postcodes <- st_join(new[new_code_col], postcodes[pcd_col])
  # sum of postcodes in each new boundary
  new_postcode_count <- new_w_postcodes %>%
    as.data.frame() %>%  # convert to regular st dataframe (easier for simple calculations)
    dplyr::select(1,2) %>%  # select only code and postcode cols by index
    group_by_at(1) %>%  # group by area code
    summarise(sum_new_postcodes = n()) %>%
    ungroup()

  # intersection of old boundaries and new boundaries using st_intersection() function
  # returns the intersection polygon for each old boundary part within a new boundary
  new_w_old <- st_intersection(new[new_code_col], old[old_code_col])
  # modify geometry to uniform type: simple polygons
  new_w_old$geometry <- st_cast(new_w_old$geometry, "POLYGON")
  # save shapefile if wanted
  #st_write(new_w_old, "[filepath/filename].shp")

  # left-join postcodes found in each intersection area
  print(st_crs(new_w_old) == st_crs(postcodes)) # check Coordinate Reference System matches
  new_w_old_postcodes <- st_join(new_w_old, postcodes[pcd_col])

  # count number of postcodes in each intersection area (old boundaries parts within new boundary)
  new_w_old_postcodes <- new_w_old_postcodes %>%
    as.data.frame() %>%  # convert to regular st dataframe (easier for simple calculations)
    dplyr::select(1, 2, 3) %>%  # select new boundary, old boundary and postcode columns by index
    group_by(across(c(1,2))) %>%   # group by new boundary code, old boundary code
    summarise(old_in_new_postcodes = n()) %>%
    ungroup()

  # create new table of proportion of postcodes in old boundaries parts as total postcodes new boundary
  proportions <- new_postcode_count %>%
    left_join(y = new_w_old_postcodes, by = new_code_col) %>%
    mutate(prop = old_in_new_postcodes/sum_new_postcodes)
  
  return(proportions)
}

# try function
translate_geo(old, new, postcodes)
# proportions <- translate_geo(old, new, postcodes)

# then all we need to do is multiply [metric] * "proportion of postcodes in that SA as total postcodes in the DZ"
# and the translation is complete.