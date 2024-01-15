library(progress)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)

# Load data
# data_UKR <- read.csv("data/temperature_daily_grid_UKR.csv", header = TRUE, sep = ",")
# data_ESP <- read.csv("data/temperature_daily_grid_ESP.csv", header = TRUE, sep = ",")
# data_POL <- read.csv("data/temperature_daily_grid_POL.csv", header = TRUE, sep = ",")
# data_PRT <- read.csv("data/temperature_daily_grid_PRT.csv", header = TRUE, sep = ",")
# 
# # convert kelvin to celsius
# data_UKR <- data_UKR %>%
#   mutate_at(vars(-1:-3), ~. - 273.15)
# 
# data_ESP <- data_ESP %>%
#   mutate_at(vars(-1:-3), ~. - 273.15)
# 
# data_POL <- data_POL %>%
#   mutate_at(vars(-1:-3), ~. - 273.15)
# 
# data_PRT <- data_PRT %>%
#   mutate_at(vars(-1:-3), ~. - 273.15)

plot_density_by_year <- function(data, variable, country_name, line_size = 0.3) {
  # Ensure the date column is in Date format
  data$date <- as.Date(data$date)
  
  # Extract the year from the date
  data <- data %>%
    mutate(year = as.numeric(format(date, "%Y")))
  
  # Filter data for years 1990 to 2020
  data <- data %>%
    filter(year >= 1990, year <= 2020)
  
  # Create a color palette
  color_palette <- colorRampPalette(c("blue", "red"))(2021 - 1990 + 1)
  
  # Plotting
  ggplot(data, aes(x = !!sym(variable), group = year, color = as.factor(year))) +
    geom_density(linewidth = line_size) +
    scale_color_manual(values = color_palette) +
    labs(title = paste("Density of", variable, "in", country_name, "by year (1990-2020)"),
         x = variable,
         y = "Density") +
    theme_minimal()
}

# Example usage
# plot_density_by_year(data_UKR, "temperature_mean", "Ukraine")
# plot_density_by_year(data_UKR, "temperature_min", "Ukraine")
# plot_density_by_year(data_UKR, "temperature_max", "Ukraine")
