library(sf)
GeoDATA <- read_sf("./world-administrative-boundaries.geojson")
GeoDATA <- st_transform(GeoDATA, crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
GeoDATA.WGS84 <- st_transform(GeoDATA, crs = '+proj=longlat +datum=WGS84')
Ukraine <- GeoDATA.WGS84[GeoDATA.WGS84$iso3 == "UKR", ]
# Get bounding box
bbox_list <- as.list(st_bbox(Ukraine))
xmin <- bbox_list$xmin
ymin <- bbox_list$ymin
xmax <- bbox_list$xmax
ymax <- bbox_list$ymax
# normalize the bbox for the area to be no more than 100 points
compute_smaller_bbox <- function(xmin, ymin, xmax, ymax, target_area = 5) {
  # Calculate the original bounding box dimensions
  width <- xmax - xmin
  height <- ymax - ymin
  
  # Calculate the area of the original bounding box
  original_area <- width * height
  
  # If the original area is already less than or equal to the target area, return the original bounding box
  if (original_area <= target_area) {
    return(c(xmin, ymin, xmax, ymax))
  }
  
  # Calculate the target width and height based on the API's constraints
  target_width <- sqrt(target_area)  # Assuming the area is in square degrees
  target_height <- target_width
  
  # Calculate the center coordinates
  center_x <- (xmin + xmax) / 2
  center_y <- (ymin + ymax) / 2
  
  # Calculate the new bounding box coordinates
  new_xmin <- center_x - target_width / 2
  new_ymin <- center_y - target_height / 2
  new_xmax <- center_x + target_width / 2
  new_ymax <- center_y + target_height / 2
  
  # Return the new bounding box
  return(c(new_xmin, new_ymin, new_xmax, new_ymax))
}
original_bbox = c(xmin, ymin, xmax, ymax)
area_o = (xmax - xmin) * (ymax - ymin)
smaller_bbox <- compute_smaller_bbox(xmin, ymin, xmax, ymax)
area_s = (smaller_bbox[3] - smaller_bbox[1]) * (smaller_bbox[4] - smaller_bbox[2])
area_s <= 100
daily_single_2022 <- get_power(
  community = "ag",
  pars = c("T2M_MIN", "T2M_MAX", "T2M"),
  lonlat = smaller_bbox,
  temporal_api = "daily",
  dates = c("2022-01-01", "2022-12-31")
)
