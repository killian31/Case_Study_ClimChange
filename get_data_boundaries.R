library(sf)
library(nasapower)
library(tidyr)
library(dplyr)
library(ggplot2)

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

BBOX <- function(GEO, CODE) {
  AREA <- GEO %>% filter(iso3 == CODE) #Get Geo for chosen ISO3
  AREA_BBOX <- st_bbox(AREA) #Get Bounding Box
  #AREA and BBOX Calculation in KM2
  AREA_KM2 <- format(x = round(as.numeric(gsub("\\[m\\^2\\]", "", st_area(AREA) / 1e6)), 2), trim = TRUE, scientific = FALSE, big.mark = " ")
  BBOX_KM2 <- format(x = round(as.numeric(gsub("\\[m\\^2\\]", "", st_area(st_as_sfc(AREA_BBOX)) / 1e6)), 2), trim = TRUE, scientific = FALSE, big.mark = " ")
  #Map AREA and BBOX
  AREA.M <- plot(AREA$geometry, main = paste(paste(AREA$Residence, " (", AREA_KM2, "km^2)", sep = ""), "\n", paste("BBOX", " (", BBOX_KM2, "km^2)", sep = "")))
  AREA.M <- AREA.M + rect(AREA_BBOX[1], AREA_BBOX[2], AREA_BBOX[3], AREA_BBOX[4], col="transparent", border="red")
  return(AREA_BBOX)
}

get_yearly_data <- function(iso3, buffer_km = 10, geodata_file = "./world-administrative-boundaries.geojson") {
  years <- 1990:2020
  
  GeoDATA <- read_sf(geodata_file)
  GeoDATA <- st_transform(GeoDATA, crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
  GeoDATA.WGS84 <- st_transform(GeoDATA, crs = '+proj=longlat +datum=WGS84')
  country <- GeoDATA.WGS84[GeoDATA.WGS84$iso3 == iso3, ]
  
  bbox_list <- unname(unlist(as.list(st_bbox(country))))
  
  # xmin <- bbox_list$xmin
  # ymin <- bbox_list$ymin
  # xmax <- bbox_list$xmax
  # ymax <- bbox_list$ymax
  # 
  # smaller_bbox <- centered_bounding_box(xmin, ymin, xmax, ymax)
  PRM <- query_parameters(community = "ag", temporal_api = "daily")
  
  COUNTRY <- data.frame(
    LAT = numeric(0),
    LON = numeric(0),
    YEAR = numeric(0),
    MM = numeric(0),
    DD = numeric(0),
    DOY = numeric(0),
    YYYYMMDD = as.Date(character(0), format = "%Y%m%d"),
    T2M = numeric(0),
    T2M_MIN = numeric(0),
    T2M_MAX = numeric(0),
    T2M_RANGE = numeric(0)
  )  
  TIME_0 <- Sys.time()
  
  for (YEAR in years) {
    TIME <- c(paste0(YEAR, "-01-01"), paste0(YEAR, "-12-31"))
    
    CLIMATE <- get_power(
      community = "ag",
      pars = c("T2M", "T2M_MIN", "T2M_MAX", "T2M_RANGE"),
      temporal_api = "daily",
      lonlat = bbox_list, #c(9.53357, 46.40750, 17.16639, 49.01875),
      dates = TIME,
      time_standard = "UTC"
    )
    
    COUNTRY <- rbind(COUNTRY, CLIMATE)
    
    TIME_1 <- Sys.time()
    TIME <- difftime(TIME_1, TIME_0, units = "mins")
    
    cat("YEAR:", YEAR, "| TIME:", round(TIME, 2), "MIN")
  }
  
  # daily_single <- get_power(
  #   community = "ag",
  #   pars = c("T2M_MIN", "T2M_MAX", "T2M", "T2M_RANGE"),
  #   lonlat = bbox_list,
  #   temporal_api = "daily",
  #   dates = c(paste(year,"-01-01", sep=""), paste(year,"12-31", sep=""))
  # )
  #Spatial Points
  COUNTRY <- st_as_sf(COUNTRY, coords = c("LON", "LAT"), crs = '+proj=longlat +datum=WGS84')
  
  #Keep Unique Spatial Points 
  COUNTRY_SPU <- COUNTRY[!duplicated(COUNTRY$geometry), ]
  
  AREA <- country
  COUNTRY_SPU_within_AREA <- st_intersection(AREA, COUNTRY_SPU)
  COUNTRY_SPU_within_AREA <- AUSTRIA_SPU_within_AREA[, names(COUNTRY_SPU)]
  
  
  AREA_10km <- st_buffer(AREA, dist = buffer_km * 1000)
  AREA_10km_SMOOTH <- st_simplify(AREA_10km, dTolerance = 5000)
  #Get Spatial Points Located within Smooth 10km Buffer Boundaries
  COUNTRY_SPU_within_SMOOTH_AREA <- st_intersection(AREA_10km_SMOOTH, COUNTRY_SPU)
  COUNTRY_SPU_within_SMOOTH_AREA <- COUNTRY_SPU_within_SMOOTH_AREA[, names(COUNTRY_SPU)]
  
  #Get Spatial Points in Smooth 10km Buffer Boundaries and !in AREA Boundaries
  COUNTRY_SPU_wSA <- st_difference(COUNTRY_SPU_within_SMOOTH_AREA, AREA)
  #Remove Row(s) !in AREA_10km_SMOOTH
  COUNTRY <- COUNTRY %>% filter(geometry %in% COUNTRY_SPU_within_SMOOTH_AREA$geometry)

  
  COUNTRY_NGeo <- data.frame(COUNTRY %>% st_drop_geometry())

  mean_daily <- COUNTRY_NGeo %>%
    group_by(YEAR, MM, DD) %>%
    summarise(
      T2M_MIN = min(T2M_MIN),
      T2M_MAX = max(T2M_MAX),
      T2M = mean(T2M),
      T2M_RANGE = mean(T2M_RANGE)
    ) %>%
    ungroup()
  return(mean_daily)
}
