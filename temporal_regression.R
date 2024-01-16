# Load data

# ESP
data_ESP_max <- read.csv("output_data/ESP/aggregate_max_ESP.csv", header = TRUE, sep = ",")
data_ESP_mean <- read.csv("output_data/ESP/aggregate_mean_ESP.csv", header = TRUE, sep = ",")
data_ESP_min <- read.csv("output_data/ESP/aggregate_min_ESP.csv", header = TRUE, sep = ",")

# POL
data_POL_max <- read.csv("output_data/POL/aggregate_max_POL.csv", header = TRUE, sep = ",")
data_POL_mean <- read.csv("output_data/POL/aggregate_mean_POL.csv", header = TRUE, sep = ",")
data_POL_min <- read.csv("output_data/POL/aggregate_min_POL.csv", header = TRUE, sep = ",")

# PRT
data_PRT_max <- read.csv("output_data/PRT/aggregate_max_PRT.csv", header = TRUE, sep = ",")
data_PRT_mean <- read.csv("output_data/PRT/aggregate_mean_PRT.csv", header = TRUE, sep = ",")
data_PRT_min <- read.csv("output_data/PRT/aggregate_min_PRT.csv", header = TRUE, sep = ",")

# UKR
data_UKR_max <- read.csv("output_data/UKR/aggregate_max_UKR.csv", header = TRUE, sep = ",")
data_UKR_mean <- read.csv("output_data/UKR/aggregate_mean_UKR.csv", header = TRUE, sep = ",")
data_UKR_min <- read.csv("output_data/UKR/aggregate_min_UKR.csv", header = TRUE, sep = ",")

# We create the plot_temporal_regression function
plot_temporal_regression <- function(data_min, data_mean, data_max, years, title) {
  
  # We regress the data
  reg1 <- lm(data_min ~ years)
  reg2 <- lm(data_mean ~ years)
  reg3 <- lm(data_max ~ years)
  
  # We plot the regressions
  plot(years, data_min, type = "n", xlab = "Years", 
       ylab = "Temparatures (in °C)", main = title, ylim = c(min(predict(reg1), 
      predict(reg2), predict(reg3))-5, max(predict(reg1), predict(reg2), predict(reg3))+5))
  
  abline(reg1, col = "cyan3", lwd = 2)
  abline(reg2, col = "gold", lwd = 2)
  abline(reg3, col = "deeppink3", lwd = 2)
  
  legend("topleft", legend = c("MIN", "MEAN", "MAX"), col = c("cyan3", "gold", "deeppink3"), lwd = 2, cex = 0.6)}


# We test the function
plot_temporal_regression(
  data_ESP_min$Median, data_ESP_mean$Median, data_ESP_max$Median, data_ESP_max$Year, 'Spain')

plot_temporal_regression(
  data_POL_min$Median, data_POL_mean$Median, data_POL_max$Median, data_POL_max$Year, 'Poland')

plot_temporal_regression(
  data_PRT_min$Median, data_PRT_mean$Median, data_PRT_max$Median, data_PRT_max$Year, 'Portugal')

plot_temporal_regression(
  data_UKR_min$Median, data_UKR_mean$Median, data_UKR_max$Median, data_UKR_max$Year, 'Ukraine')

