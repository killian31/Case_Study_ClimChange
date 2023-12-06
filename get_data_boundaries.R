library(sf)
library(nasapower)
library(tidyr)
library(dplyr)
library(ggplot2)

# GeoDATA <- read_sf("./world-administrative-boundaries.geojson")
# GeoDATA <- st_transform(GeoDATA, crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
# GeoDATA.WGS84 <- st_transform(GeoDATA, crs = '+proj=longlat +datum=WGS84')
# Ukraine <- GeoDATA.WGS84[GeoDATA.WGS84$iso3 == "UKR", ]
# Get bounding box
# bbox_list <- as.list(st_bbox(Ukraine))
# xmin <- bbox_list$xmin
# ymin <- bbox_list$ymin
# xmax <- bbox_list$xmax
# ymax <- bbox_list$ymax

# compute_smaller_bbox <- function(xmin, ymin, xmax, ymax, target_area = 5) {
#   # Calculate the original bounding box dimensions
#   width <- xmax - xmin
#   height <- ymax - ymin
#   
#   # Calculate the area of the original bounding box
#   original_area <- width * height
#   
#   # If the original area is already less than or equal to the target area, return the original bounding box
#   if (original_area <= target_area) {
#     return(c(xmin, ymin, xmax, ymax))
#   }
#   
#   # Calculate the target width and height based on the API's constraints
#   target_width <- sqrt(target_area)  # Assuming the area is in square degrees
#   target_height <- target_width
#   
#   # Calculate the center coordinates
#   center_x <- (xmin + xmax) / 2
#   center_y <- (ymin + ymax) / 2
#   
#   # Calculate the new bounding box coordinates
#   new_xmin <- center_x - target_width / 2
#   new_ymin <- center_y - target_height / 2
#   new_xmax <- center_x + target_width / 2
#   new_ymax <- center_y + target_height / 2
#   
#   # Return the new bounding box
#   return(c(new_xmin, new_ymin, new_xmax, new_ymax))
# }

centered_bounding_box <- function(xmin, ymin, xmax, ymax) {
  # Calculate current width and height
  width <- xmax - xmin
  height <- ymax - ymin
  
  # Calculate centered bounding box with a maximum width and height of 4.5
  new_width <- min(width, 4.5)
  new_height <- min(height, 4.5)
  
  # Calculate new xmin, ymin, xmax, ymax
  new_xmin <- (xmin + xmax - new_width) / 2
  new_ymin <- (ymin + ymax - new_height) / 2
  new_xmax <- new_xmin + new_width
  new_ymax <- new_ymin + new_height
  
  # Return the centered bounding box
  return(c(new_xmin, new_ymin, new_xmax, new_ymax))
}

get_yearly_data <- function(year, iso3 = "UKR", geodata_file = "./world-administrative-boundaries.geojson") {
  GeoDATA <- read_sf(geodata_file)
  GeoDATA <- st_transform(GeoDATA, crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
  GeoDATA.WGS84 <- st_transform(GeoDATA, crs = '+proj=longlat +datum=WGS84')
  country <- GeoDATA.WGS84[GeoDATA.WGS84$iso3 == iso3, ]
  
  bbox_list <- as.list(st_bbox(country))
  xmin <- bbox_list$xmin
  ymin <- bbox_list$ymin
  xmax <- bbox_list$xmax
  ymax <- bbox_list$ymax
  
  smaller_bbox <- centered_bounding_box(xmin, ymin, xmax, ymax)
  
  daily_single <- get_power(
    community = "ag",
    pars = c("T2M_MIN", "T2M_MAX", "T2M"),
    lonlat = smaller_bbox,
    temporal_api = "daily",
    dates = c(paste(year,"-01-01", sep=""), paste(year,"12-31", sep=""))
  )
  mean_daily <- daily_single %>%
    group_by(YEAR, MM, DD) %>%
    summarise(
      LAT = mean(LAT),
      LON = mean(LON),
      T2M_MIN = mean(T2M_MIN),
      T2M_MAX = mean(T2M_MAX),
      T2M = mean(T2M)
    ) %>%
    ungroup()
  return(mean_daily)
}
