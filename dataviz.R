source("./get_data_boundaries.R")

combined_data_1990_2020$Date <- as.Date(paste(combined_data_1990_2020$YEAR, combined_data_1990_2020$MM, combined_data_1990_2020$DD, sep = "-"), format = "%Y-%m-%d")
combined_data_1990_2020$DayMonth <- format(combined_data_1990_2020$Date, "%m-%d")

coldestmean=min(combined_data_1990_2020$T2M)

# Split the data frame into a list of data frames based on the 'YEAR' column
df_list <- split(combined_data_1990_2020, combined_data_1990_2020$YEAR)

sum((df_list[[1]]$T2M)-coldestmean)

# Computing the integral for each year
l=c()
for (i in 1:30) {
  l<-append(l,sum((df_list[[i]]$T2M)-coldestmean))
}
 

# Plot the list of numbers
plot(l, type = "o", main = "Plot of Integral of temperatures evolution", xlab = "Index", ylab = "Values")







# Assuming df_list is your list of data frames with 'DayMonth' and 'T2M' columns
# Replace df_list with the actual name of your list if different

# Load the ggplot2 library
library(ggplot2)

# Function to create a plot for a single data frame
create_plot <- function(df) {
  ggplot(df, aes(x = DayMonth, y = T2M, group = 1)) +
    geom_line() +
    labs(title = "Temperature over Time",
         x = "Day and Month",
         y = "Temperature (T2M)")
}

# Apply the function to each data frame in the list
list_of_plots <- lapply(df_list, create_plot)

# Display the resulting plots
list_of_plots

install.packages("shiny")

# Assuming list_of_plots is your list of ggplot objects
# Replace list_of_plots with the actual name of your list

library(shiny)
library(ggplot2)

# Define the UI
ui <- fluidPage(
  titlePanel("Navigate Plots"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("plotIndex", "Select Plot:", 
                  min = 1, max = length(list_of_plots), value = 1)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    # Plot the selected plot
    plot_to_show <- list_of_plots[[input$plotIndex]]
    print(plot_to_show)
  })
}

# Run the application
shinyApp(ui, server)


















# Assuming df_list is your list of data frames with 'DayMonth' and 'T2M' columns
# Replace df_list with the actual name of your list if different

# Load the ggplot2 library
library(ggplot2)

# Combine all data frames into a single data frame
combined_df <- do.call(rbind, df_list)

# Create a plot for the combined data frame
ggplot(combined_data_1990_2020, aes(x = DayMonth, y = T2M, color = factor(rep(seq_along(df_list), sapply(df_list, nrow))))) +
  geom_line() +
  labs(title = "Temperature over Time",
       x = "Day and Month",
       y = "Temperature (T2M)",
       color = "Data Frame") +
  scale_color_discrete(name = "Data Frame")
