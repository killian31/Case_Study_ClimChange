source("./get_data_boundaries.R")

years <- 1990:2020

combined_data_1990_2020 <- bind_rows(lapply(years, get_yearly_data))

compare_distrib <- function(dataset, variable) {
  p <- ggplot(dataset, aes_string(x = "as.factor(YEAR)", y = variable, fill = "as.factor(YEAR)")) +
    geom_boxplot() +
    labs(title = paste("Comparison of ", variable, " Distribution Among Years"),
         x = "Year",
         y = variable) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
    theme(legend.position = "top")  # Move legend to the top
  
  ggsave(paste(variable, "_distribution_years.png"), plot = p, width = 10, height = 6)
}
for (variable in c("T2M", "T2M_MIN", "T2M_MAX")) {
  compare_distrib(combined_data_1990_2020, variable)
}
