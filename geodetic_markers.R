setwd(/Users/theodruilhe/Documents/M1-ECO-STATS/case_study)

# Load data
data_UKR <- read.csv("data/temperature_daily_grid_UKR.csv", header = TRUE, sep = ",")
data_ESP <- read.csv("data/temperature_daily_grid_ESP.csv", header = TRUE, sep = ",")
data_POL <- read.csv("data/temperature_daily_grid_POL.csv", header = TRUE, sep = ",")
data_PRT <- read.csv("data/temperature_daily_grid_PRT.csv", header = TRUE, sep = ",")

# Charger la bibliothèque leaflet
library(leaflet)
library(dplyr)

data_UKR_distinct <- data_UKR %>%
  distinct(lat, lon)
# Créer la carte avec leaflet
ma_carte <- leaflet(data_UKR_distinct) %>%
  addTiles()  # Ajouter des tuiles pour le fond de la carte (OpenStreetMap)

# Ajouter des marqueurs pour chaque point
ma_carte <- ma_carte %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = ~paste("Latitude: ", lat, "<br>Longitude: ", lon))

# Afficher la carte
ma_carte
# point sud au bord de la mer Latitude: 46, Longitude: 30
# point montagneux ouest Latitude: 48.5, Longitude: 23.75
# point au nord continental Latitude: 52, Longitude: 33.75
# point à l'est Latitude: 49.5, Longitude: 38.75

data_UKR_south <- data_UKR %>%
  filter(lat == 46, lon == 30)

data_UKR_west <- data_UKR %>%
  filter(lat == 48.5, lon == 23.75)

data_UKR_north <- data_UKR %>%
  filter(lat == 52, lon == 33.75)

data_UKR_east <- data_UKR %>%
  filter(lat == 49.5, lon == 38.75)

data_UKR_filtered <- data_UKR %>%
  filter(
    (lat == 46 & lon == 30) |
      (lat == 48.5 & lon == 23.75) |
      (lat == 52 & lon == 33.75) |
      (lat == 49.5 & lon == 38.75)
  )

ma_carte <- leaflet(data_UKR_filtered) %>%
  addTiles()  # Ajouter des tuiles pour le fond de la carte (OpenStreetMap)

# Ajouter des marqueurs pour chaque point
ma_carte <- ma_carte %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = ~paste("Latitude: ", lat, "<br>Longitude: ", lon))

# Afficher la carte
ma_carte

