library(progress)

files <- c('UKR', 'ESP', 'POL', 'PRT')
data_UKR <- read.csv("data/temperature_daily_grid_UKR.csv", header = TRUE, sep = ",")
data_ESP <- read.csv("data/temperature_daily_grid_ESP.csv", header = TRUE, sep = ",")
data_POL <- read.csv("data/temperature_daily_grid_POL.csv", header = TRUE, sep = ",")
data_PRT <- read.csv("data/temperature_daily_grid_PRT.csv", header = TRUE, sep = ",")

# convert kelvin to celsius
all_data <- list(data_ukr, data_esp, data_pol, data_prt)
for (i in 1:length(all_data)) {
  all_data[[i]]$temperature_mean <- all_data[[i]]$temperature_mean - 273.15
  all_data[[i]]$temperature_min <- all_data[[i]]$temperature_min - 273.15
  all_data[[i]]$temperature_max <- all_data[[i]]$temperature_max - 273.15
}

drawYearlyBoxplots <- function(data, temp_variable, file_name; country) {
  # Check if the temperature variable is valid
  if (!(temp_variable %in% c("temperature_mean", "temperature_min", "temperature_max"))) {
    stop("Invalid temperature variable. Choose from 'temperature_mean', 'temperature_min', 'temperature_max'.")
  }
  
  # Extract year from the date column
  data$year <- as.integer(format(as.Date(data$date), "%Y"))
  
  # Prepare the data: create a subset with only the relevant temperature variable and the year
  data_for_plot <- data[c("year", temp_variable)]
  
  # Renaming the temperature column to a generic name for plotting
  names(data_for_plot)[names(data_for_plot) == temp_variable] <- "Temperature"
  
  # Open a JPEG device
  jpeg(file_name, width = 1600, height = 900) # You can adjust the size as needed
  
  # Using boxplot to plot the data
  boxplot(Temperature ~ year, data = data_for_plot, 
          xlab = "Year", ylab = "Temperature", 
          main = paste("Yearly Boxplots of", temp_variable, "for", country),
          las = 2, # Rotates x-axis labels
          col = rainbow(length(unique(data_for_plot$year))) # Optional: Adds color
  )
  
  # Close the device
  dev.off()
}



calculateYearlyStatistics <- function(data, start_year, end_year, temp_variable) {
  # Check if the temperature variable is valid
  if (!(temp_variable %in% c("temperature_mean", "temperature_min", "temperature_max"))) {
    stop("Invalid temperature variable. Choose from 'temperature_mean', 'temperature_min', 'temperature_max'.")
  }
  # Initialize an empty data frame to store results
  results <- data.frame(Year = integer(), Minimum = numeric(), Mean = numeric(), Maximum = numeric(),
                        Quantile_0.05 = numeric(), Quantile_0.10 = numeric(), Median = numeric(),
                        Quantile_0.90 = numeric(), Quantile_0.95 = numeric())
  
  # Total number of years for progress calculation
  total_years <- end_year - start_year + 1
  
  # Set up progress bar
  pb <- txtProgressBar(min = 0, max = total_years, style = 3)
  # Loop through the years
  for (year in start_year:end_year) {
    # Filter data for the given year
    data_year <- subset(data, format(as.Date(date), "%Y") == as.character(year))
    # Extract the relevant temperature data
    temp_data <- data_year[[temp_variable]]
    # Calculate empirical characteristics
    min_val <- min(temp_data, na.rm = TRUE)
    mean_val <- mean(temp_data, na.rm = TRUE)
    max_val <- max(temp_data, na.rm = TRUE)
    quantiles <- quantile(temp_data, probs = c(0.05, 0.10, 0.50, 0.90, 0.95), na.rm = TRUE)
    # Combine the results into a row
    row <- data.frame(Year = year, Minimum = min_val, Mean = mean_val, Maximum = max_val,
                      Quantile_0.05 = quantiles[1], Quantile_0.10 = quantiles[2], Median = quantiles[3],
                      Quantile_0.90 = quantiles[4], Quantile_0.95 = quantiles[5])
    # Append the row to the results data frame
    results <- rbind(results, row)
    # Update progress bar
    setTxtProgressBar(pb, year - start_year + 1)
  }
  
  # Close the progress bar
  close(pb)
  
  return(results)
}

drawYearlyBoxplots(data_UKR, "temperature_mean", "visualizations/UKR/boxplot_mean_UKR.jpg", "Ukraine")
drawYearlyBoxplots(data_UKR, "temperature_min", "visualizations/UKR/boxplot_min_UKR.jpg", "Ukraine")
drawYearlyBoxplots(data_UKR, "temperature_max", "visualizations/UKR/boxplot_max_UKR.jpg", "Ukraine")

drawYearlyBoxplots(data_ESP, "temperature_mean", "visualizations/ESP/boxplot_mean_ESP.jpg", "Spain")
drawYearlyBoxplots(data_ESP, "temperature_min", "visualizations/ESP/boxplot_min_ESP.jpg", "Spain")
drawYearlyBoxplots(data_ESP, "temperature_max", "visualizations/ESP/boxplot_max_ESP.jpg", "Spain")

drawYearlyBoxplots(data_POL, "temperature_mean", "visualizations/POL/boxplot_mean_POL.jpg", "Poland")
drawYearlyBoxplots(data_POL, "temperature_min", "visualizations/POL/boxplot_min_POL.jpg", "Poland")
drawYearlyBoxplots(data_POL, "temperature_max", "visualizations/POL/boxplot_max_POL.jpg", "Poland"))

drawYearlyBoxplots(data_PRT, "temperature_mean", "visualizations/PRT/boxplot_mean_PRT.jpg", "Portugal")
drawYearlyBoxplots(data_PRT, "temperature_min", "visualizations/PRT/boxplot_min_PRT.jpg", "Portugal")
drawYearlyBoxplots(data_PRT, "temperature_max", "visualizations/PRT/boxplot_max_PRT.jpg", "Portugal")

final_dataset_mean_UKR <- calculateYearlyStatistics(data_UKR, 1990, 2020, "temperature_mean")
write.csv(final_dataset_mean_UKR, "output_data/UKR/aggregate_mean_UKR.csv", row.names=FALSE)
final_dataset_min_UKR <- calculateYearlyStatistics(data_UKR, 1990, 2020, "temperature_min")
write.csv(final_dataset_min_UKR, "output_data/UKR/aggregate_min_UKR.csv", row.names=FALSE)
final_dataset_max_UKR <- calculateYearlyStatistics(data_UKR, 1990, 2020, "temperature_max")
write.csv(final_dataset_max_UKR, "output_data/UKR/aggregate_max_UKR.csv", row.names=FALSE)

final_dataset_mean_ESP <- calculateYearlyStatistics(data_ESP, 1990, 2020, "temperature_mean")
write.csv(final_dataset_mean_ESP, "output_data/ESP/aggregate_mean_ESP.csv", row.names=FALSE)
final_dataset_min_ESP <- calculateYearlyStatistics(data_ESP, 1990, 2020, "temperature_min")
write.csv(final_dataset_min_ESP, "output_data/ESP/aggregate_min_ESP.csv", row.names=FALSE)
final_dataset_max_ESP <- calculateYearlyStatistics(data_ESP, 1990, 2020, "temperature_max")
write.csv(final_dataset_max_ESP, "output_data/ESP/aggregate_max_ESP.csv", row.names=FALSE)

final_dataset_mean_POL <- calculateYearlyStatistics(data_POL, 1990, 2020, "temperature_mean")
write.csv(final_dataset_mean_POL, "output_data/POL/aggregate_mean_POL.csv", row.names=FALSE)
final_dataset_min_POL <- calculateYearlyStatistics(data_POL, 1990, 2020, "temperature_min")
write.csv(final_dataset_min_POL, "output_data/POL/aggregate_min_POL.csv", row.names=FALSE)
final_dataset_max_POL <- calculateYearlyStatistics(data_POL, 1990, 2020, "temperature_max")
write.csv(final_dataset_max_POL, "output_data/POL/aggregate_max_POL.csv", row.names=FALSE)

final_dataset_mean_PRT <- calculateYearlyStatistics(data_PRT, 1990, 2020, "temperature_mean")
write.csv(final_dataset_mean_PRT, "output_data/PRT/aggregate_mean_PRT.csv", row.names=FALSE)
final_dataset_min_PRT <- calculateYearlyStatistics(data_PRT, 1990, 2020, "temperature_min")
write.csv(final_dataset_min_PRT, "output_data/PRT/aggregate_min_PRT.csv", row.names=FALSE)
final_dataset_max_PRT <- calculateYearlyStatistics(data_PRT, 1990, 2020, "temperature_max")
write.csv(final_dataset_max_PRT, "output_data/PRT/aggregate_max_PRT.csv", row.names=FALSE)
