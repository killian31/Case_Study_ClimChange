library(progress)

data = read.csv("data/temperature_daily_grid_UKR.csv", header = TRUE, sep = ",")
# show first temporal rows and last
head(data)
tail(data)

drawYearlyBoxplots <- function(data, temp_variable, file_name) {
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
          main = paste("Yearly Boxplots of", temp_variable),
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

drawYearlyBoxplots(data, "temperature_mean", "boxplot_mean.jpg")
drawYearlyBoxplots(data, "temperature_min", "boxplot_min.jpg")
drawYearlyBoxplots(data, "temperature_max", "boxplot_max.jpg")

final_dataset_mean <- calculateYearlyStatistics(data, 1990, 2020, "temperature_mean")
write.csv(final_dataset_mean, "aggregate_mean.csv", row.names=FALSE)
final_dataset_min <- calculateYearlyStatistics(data, 1990, 2020, "temperature_min")
write.csv(final_dataset_min, "aggregate_min.csv", row.names=FALSE)
final_dataset_max <- calculateYearlyStatistics(data, 1990, 2020, "temperature_max")
write.csv(final_dataset_max, "aggregate_max.csv", row.names=FALSE)

