library(sf)
library(nasapower)
library(tidyr)
library(dplyr)
library(ggplot2)

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

original_bbox = c(xmin, ymin, xmax, ymax)
smaller_bbox <- centered_bounding_box(xmin, ymin, xmax, ymax)

daily_single_2022 <- get_power(
  community = "ag",
  pars = c("T2M_MIN", "T2M_MAX", "T2M"),
  lonlat = smaller_bbox,
  temporal_api = "daily",
  dates = c("2022-01-01", "2022-12-31")
)
colnames(daily_single_2022)

calculate_statistics <- function(data, variable) {
  stats <- summary(data[[variable]])
  quantiles <- quantile(data[[variable]], c(0.05, 0.10, 0.5, 0.90, 0.95))
  return(c(Minimum = stats[1], Mean = stats[3], Maximum = stats[6], Quantiles = quantiles))
}

# Function to get climate data for a specific variable and perform calculations
get_and_analyze_climate_data <- function(community, variable, smaller_bbox, year) {
  # Get climate data
  daily_data <- get_power(
    community = community,
    pars = variable,
    lonlat = smaller_bbox,
    temporal_api = "daily",
    dates = c(paste(year, "-01-01"), paste(year, "-12-31"))
  )
  
  # Calculate summary statistics
  statistics <- calculate_statistics(daily_data, variable)
  
  # Return results
  return(data.frame(Year = year, Variable = variable, statistics))
}

community <- "ag"
variables <- c("T2M_MIN", "T2M_MAX", "T2M")
year <- 2022

final_results <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(final_results) <- c("Year", "Variable", "Minimum", "Mean", "Maximum", "Quantile_0.05", "Quantile_0.10", "Quantile_0.5", "Quantile_0.90", "Quantile_0.95")

# Loop through each variable
for (variable in variables) {
  # Get and analyze climate data for the variable
  variable_results <- get_and_analyze_climate_data(community, variable, smaller_bbox, year)
  
  # Append results to the final data frame
  final_results <- rbind(final_results, variable_results)
}

# Convert results to a tibble for easier manipulation
final_results_tibble <- as_tibble(final_results)

# Print or further analyze the final results
print(final_results_tibble)

# Plotting the results for each variable
ggplot(final_results_tibble, aes(x = Variable, y = statistics, fill = Variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Temperature in 2022",
       x = "Variable",
       y = "Mean Temperature") +
  theme_minimal()
