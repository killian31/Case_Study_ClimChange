# Install and load required packages
# install.packages(c("shiny", "leaflet", "nasapower", "glue", "dplyr"))
library(shiny)
library(leaflet)
library(dplyr)
library(nasapower)
library(glue)

ui <- fluidPage(
  titlePanel("NASA Power Explorer"),
  fluidRow(
    column(
      width = 6,
      h4("Click on the map and download the temperature data for the clicked coordinates."),
    ),
    column(
      width = 6,
      dateInput("start_date", "Start Date", value = "1985-01-01"),
      dateInput("end_date", "End Date", value = "2023-01-31")
    )
  ),
  fluidRow(
    column(width = 12, leafletOutput("map"))
  ),
  br(),
  fluidRow(
    column(width = 12, actionButton("use_clik_loc", "Download Data", class = "btn-primary"))
  ),
  br(),
  fluidRow(
    column(width = 12, actionButton("exit_app", "Exit App", class = "btn-danger"))
  ),
  tags$style(HTML(".leaflet {height: 80vh;}")),  # Adjust the height of the leaflet map
  tags$style(HTML(".btn-primary, .btn-danger {width: 100%;}"))  # Make buttons occupy the full width
)

server <- function(input, output, session) {
  output$map <- renderLeaflet(addTiles(leaflet()))
  
  observeEvent(input$use_clik_loc, {
    last_click <- isolate(as.data.frame(input$map_click))
    lonlat <- c(last_click$lng, last_click$lat)
    
    # Get selected dates from the date inputs
    start_date <- input$start_date
    end_date <- input$end_date
    
    # Display a message about the selected location and parameters
    showNotification(
      glue("Downloading data for location {lonlat[1]}, {lonlat[2]} from {start_date} to {end_date}"),
      duration = 5,
      type = "message"
    )
    
    # Récupérer les variables d'intérêt Tmin, Tmax, Tmean
    daily_single_ag <- get_power(
      community = "ag",
      pars = c("T2M_MIN", "T2M_MAX", "T2M"),
      lonlat = lonlat,
      temporal_api = "daily",
      dates = c(start_date, end_date)
    )
    
    # Save the dataset to a CSV file
    write.csv(daily_single_ag, glue("./dataset_{lonlat[1]}_{lonlat[2]}_{start_date}_{end_date}.csv"), row.names = FALSE)
    
    # Display a success message
    showNotification(
      "Data downloaded successfully!",
      duration = 5,
      type = "default"
    )
  })
  
  observeEvent(input$exit_app, {
    # Display a message before exiting
    showNotification(
      "Exiting the application...",
      duration = 5,
      type = "message"
    )
    # Delay for a moment before exiting
    Sys.sleep(1)
    stopApp(session)
  })
}

shinyApp(ui, server)
