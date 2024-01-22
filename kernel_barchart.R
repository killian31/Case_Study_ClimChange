library(progress)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(plotly)
library(ggridges)
library(viridis)

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

plot_3d_density_by_year <- function(data, variable, country_name) {
  # Ensure the date column is in Date format
  data$date <- as.Date(data$date)
  
  # Extract the year from the date as numeric
  data <- data %>%
    mutate(year = as.numeric(format(date, "%Y")))
  
  # Filter data for years 1990 to 2020
  filtered_data <- data %>%
    filter(year >= 1990, year <= 2020)
  
  # Convert year back to factor for plotting
  filtered_data$year <- as.factor(filtered_data$year)
  
  # Plotting
  ggplot(filtered_data, aes(x = !!sym(variable), y = `year`, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
    scale_x_continuous(expand = c(0.01, 0)) +
    scale_y_discrete(expand = c(0.01, 0)) +
    scale_fill_viridis(name = "Temp. [C]", option = "C") +
    labs(title = paste('Temperatures in', country_name),
         subtitle = paste(variable, '(Celsius) by year between 1990 and 2020')) +
    theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
}

# Example usage
# plot_density_by_year(data_UKR, "temperature_mean", "Ukraine")
# plot_density_by_year(data_UKR, "temperature_min", "Ukraine")
# plot_density_by_year(data_UKR, "temperature_max", "Ukraine")

# plot_3d_density_by_year(data_UKR, "temperature_mean", "Ukraine")
# plot_3d_density_by_year(data_PRT, "temperature_max", "Portugal")

