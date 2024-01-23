# Load data

# ESP
# data_ESP_max <- read.csv("output_data/ESP/aggregate_max_ESP.csv", header = TRUE, sep = ",")
# data_ESP_mean <- read.csv("output_data/ESP/aggregate_mean_ESP.csv", header = TRUE, sep = ",")
# data_ESP_min <- read.csv("output_data/ESP/aggregate_min_ESP.csv", header = TRUE, sep = ",")
# 
# # POL
# data_POL_max <- read.csv("output_data/POL/aggregate_max_POL.csv", header = TRUE, sep = ",")
# data_POL_mean <- read.csv("output_data/POL/aggregate_mean_POL.csv", header = TRUE, sep = ",")
# data_POL_min <- read.csv("output_data/POL/aggregate_min_POL.csv", header = TRUE, sep = ",")
# 
# # PRT
# data_PRT_max <- read.csv("output_data/PRT/aggregate_max_PRT.csv", header = TRUE, sep = ",")
# data_PRT_mean <- read.csv("output_data/PRT/aggregate_mean_PRT.csv", header = TRUE, sep = ",")
# data_PRT_min <- read.csv("output_data/PRT/aggregate_min_PRT.csv", header = TRUE, sep = ",")
# 
# # UKR
data_UKR_max <- read.csv("output_data/UKR/aggregate_max_UKR.csv", header = TRUE, sep = ",")
data_UKR_mean <- read.csv("output_data/UKR/aggregate_mean_UKR.csv", header = TRUE, sep = ",")
data_UKR_min <- read.csv("output_data/UKR/aggregate_min_UKR.csv", header = TRUE, sep = ",")

# We create the plot_temporal_regression function
plot_temporal_regression <- function(data_min, data_mean, data_max, years, title, show_stat = TRUE) {
  
  # We regress the data
  reg1 <- lm(data_min ~ years)
  reg2 <- lm(data_mean ~ years)
  reg3 <- lm(data_max ~ years)
  
  # We plot the regressions and the raw data
  plot(years, data_min, col = "cyan3", xlab = "Years", 
       ylab = "Temparatures (in °C)", main = title, ylim = c(min(predict(reg1), 
      predict(reg2), predict(reg3))-5, max(predict(reg1), predict(reg2), predict(reg3))+5))
  points(years, data_mean, col = "gold")
  points(years, data_max, col = "deeppink3") 
  abline(reg1, col = "cyan3", lwd = 2)
  abline(reg2, col = "gold", lwd = 2)
  abline(reg3, col = "deeppink3", lwd = 2)

  pred1 <- predict(reg1)
  pred2 <- predict(reg2)
  pred3 <- predict(reg3)
  
  # We add the coefficients and the t-test
  if (show_stat == TRUE){
  text(1995, min(pred1)-2, paste(sprintf("coef = %.3f, t = %.3f", coef(reg1)[2], coef(summary(reg1))[2, "t value"])), pos = 4, col = "black")
  text(1995, min(pred2)-2, paste(sprintf("coef = %.3f, t = %.3f", coef(reg2)[2], coef(summary(reg2))[2, "t value"])), pos = 4, col = "black")
  text(1995, min(pred3)-2, paste(sprintf("coef = %.3f, t = %.3f", coef(reg3)[2], coef(summary(reg3))[2, "t value"])), pos = 4, col = "black")
  }
 
  legend("topleft", legend = c("MIN", "MEAN", "MAX"), col = c("cyan3", "gold", "deeppink3"), lwd = 2, cex = 0.6)
}



# We test the function
# plot_temporal_regression(
# data_ESP_min$Median, data_ESP_mean$Median, data_ESP_max$Median, data_ESP_max$Year, 'Spain')
# 
# plot_temporal_regression(
#   data_POL_min$Median, data_POL_mean$Median, data_POL_max$Median, data_POL_max$Year, 'Poland')
# 
# plot_temporal_regression(
#   data_PRT_min$Median, data_PRT_mean$Median, data_PRT_max$Median, data_PRT_max$Year, 'Portugal')
# 
# plot_temporal_regression(
#   data_UKR_min$Median, data_UKR_mean$Median, data_UKR_max$Median, data_UKR_max$Year, 'Ukraine')

plot_temporal_regression_q <- function(data_q05, data_q95, years, title, show_stat = TRUE) {
  
  # We regress the data
  reg1 <- lm(data_q05 ~ years)
  reg2 <- lm(data_q95 ~ years)

  
  # We plot the regressions
  plot(years, data_q05, type = 'n', xlab = "Years", 
       ylab = "Temparatures (in °C)", main = title, ylim = c(min(predict(reg1))-5, max(predict(reg2))+5))
  abline(reg1, col = "cyan3", lwd = 2)
  abline(reg2, col = "red", lwd = 2)
  pred1 <- predict(reg1)
  pred2 <- predict(reg2)
  
  # We do a polygon to show the inter quantile space
  polygon(c(years, rev(years)), c(pred1, rev(pred2)), col = rgb(0, 0, 0, alpha = 50, maxColorValue = 255), border = NA)
  
  # We add the coefficients and the t-test
  if (show_stat == TRUE){
  text(1995, min(pred1)-2, paste(sprintf("coef = %.3f, t = %.3f", coef(reg1)[2], coef(summary(reg1))[2, "t value"])), pos = 4, col = "black")
  text(1995, min(pred2)-2, paste(sprintf("coef = %.3f, t = %.3f", coef(reg2)[2], coef(summary(reg2))[2, "t value"])), pos = 4, col = "black")
  }

  legend("topleft", legend = c("quantile 0.05", "quantile 0.95"), col = c("cyan3", "red"), lwd = 2, cex = 0.6)
}

# We execute the function for the min, mean and max of all the countries
plot_temporal_regression_q(
  data_UKR_min$Quantile_0.05, data_UKR_min$Quantile_0.95, data_UKR_min$Year, 'Min Ukraine', FALSE)
# plot_temporal_regression_q(
#   data_UKR_mean$Quantile_0.05, data_UKR_mean$Quantile_0.95, data_UKR_min$Year, 'Mean Ukraine')
# plot_temporal_regression_q(
#   data_UKR_max$Quantile_0.05, data_UKR_max$Quantile_0.95, data_UKR_min$Year, 'Max Ukraine')
# plot_temporal_regression_q(
#   data_ESP_min$Quantile_0.05, data_ESP_min$Quantile_0.95, data_ESP_min$Year, 'Min Spain')
# plot_temporal_regression_q(
#   data_ESP_mean$Quantile_0.05, data_ESP_mean$Quantile_0.95, data_ESP_min$Year, 'Mean Spain')
# plot_temporal_regression_q(
#   data_ESP_max$Quantile_0.05, data_ESP_max$Quantile_0.95, data_ESP_min$Year, 'Max Spain')
# plot_temporal_regression_q(
#   data_PRT_min$Quantile_0.05, data_PRT_min$Quantile_0.95, data_UKR_min$Year, 'Min Portugal')
# plot_temporal_regression_q(
#   data_PRT_mean$Quantile_0.05, data_PRT_mean$Quantile_0.95, data_UKR_min$Year, 'Mean Portugal')
# plot_temporal_regression_q(
#   data_PRT_max$Quantile_0.05, data_PRT_max$Quantile_0.95, data_UKR_min$Year, 'Max Portugal')
# plot_temporal_regression_q(
#   data_POL_min$Quantile_0.05, data_POL_min$Quantile_0.95, data_ESP_min$Year, 'Min Poland')
# plot_temporal_regression_q(
#   data_POL_mean$Quantile_0.05, data_POL_mean$Quantile_0.95, data_ESP_min$Year, 'Mean Poland')
# plot_temporal_regression_q(
#   data_POL_max$Quantile_0.05, data_POL_max$Quantile_0.95, data_ESP_min$Year, 'Max Poland')
# 
