# Load data
data_UKR <- read.csv("data/temperature_daily_grid_UKR.csv", header = TRUE, sep = ",")
data_ESP <- read.csv("data/temperature_daily_grid_ESP.csv", header = TRUE, sep = ",")
data_POL <- read.csv("data/temperature_daily_grid_POL.csv", header = TRUE, sep = ",")
data_PRT <- read.csv("data/temperature_daily_grid_PRT.csv", header = TRUE, sep = ",")

# Charger la bibliothèque leaflet
library(leaflet)
library(dplyr)

#### Ukrain ####
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

data_UKR_filtered_distinct <- data_UKR_filtered %>%
  distinct(lat, lon)

carte_UKR <- leaflet(data_UKR_filtered_distinct) %>%
  addTiles()  # Ajouter des tuiles pour le fond de la carte (OpenStreetMap)

# Ajouter des marqueurs pour chaque point
carte_UKR <- carte_UKR %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = ~paste("Latitude: ", lat, "<br>Longitude: ", lon))

# Afficher la carte
carte_UKR

#### Espagne ####
data_ESP_distinct <- data_ESP %>%
  distinct(lat, lon)

ma_carte <- leaflet(data_ESP_distinct) %>%
  addTiles()  # Ajouter des tuiles pour le fond de la carte (OpenStreetMap)

# Ajouter des marqueurs pour chaque point
ma_carte <- ma_carte %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = ~paste("Latitude: ", lat, "<br>Longitude: ", lon))

# Afficher la carte
ma_carte

# point nord montagneux Latitude: 42.5, Longitude: 1.25
# point sud Latitude: 37.5, Longitude: -5.625
# point central vers madrid Latitude: 40, Longitude: -3.75
# point nord ouest Latitude: 43, Longitude: -8.125

data_ESP_north <- data_ESP %>%
  filter(lat == 42.5, lon == 1.25)

data_ESP_south <- data_ESP %>%
  filter(lat == 37.5, lon == -5.625)

data_ESP_center <- data_ESP %>%
  filter(lat == 40, lon == -3.75)

data_ESP_west <- data_ESP %>%
  filter(lat == 43, lon == -8.125)

data_ESP_filtered <- data_ESP %>%
  filter(
    (lat == 42.5 & lon == 1.25) |
      (lat == 37.5 & lon == -5.625) |
      (lat == 40 & lon == -3.75) |
      (lat == 43 & lon == -8.125)
  )

data_ESP_filtered_distinct <- data_ESP_filtered %>%
  distinct(lat, lon)

carte_ESP <- leaflet(data_ESP_filtered_distinct) %>%
  addTiles()  # Ajouter des tuiles pour le fond de la carte (OpenStreetMap)

# Ajouter des marqueurs pour chaque point
carte_ESP <- carte_ESP %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = ~paste("Latitude: ", lat, "<br>Longitude: ", lon))

# Afficher la carte
carte_ESP

#### Pologne ####
data_POL_distinct <- data_POL %>%
  distinct(lat, lon)

ma_carte <- leaflet(data_POL_distinct) %>%
  addTiles()  # Ajouter des tuiles pour le fond de la carte (OpenStreetMap)

# Ajouter des marqueurs pour chaque point
ma_carte <- ma_carte %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = ~paste("Latitude: ", lat, "<br>Longitude: ", lon))

# Afficher la carte
ma_carte

# point nord Latitude: 54.5 Longitude: 18.125
# point continental Latitude: 50 Longitude: 21.25

data_POL_north <- data_POL %>%
  filter(lat == 54.5, lon == 18.125)

data_POL_center <- data_POL %>%
  filter(lat == 50, lon == 21.25)

data_POL_filtered <- data_POL %>%
  filter(
    (lat == 54.5 & lon == 18.125) |
      (lat == 50 & lon == 21.25)
  )

data_POL_filtered_distinct <- data_POL_filtered %>%
  distinct(lat, lon)

carte_POL <- leaflet(data_POL_filtered_distinct) %>%
  addTiles()  # Ajouter des tuiles pour le fond de la carte (OpenStreetMap)

# Ajouter des marqueurs pour chaque point
carte_POL <- carte_POL %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = ~paste("Latitude: ", lat, "<br>Longitude: ", lon))

# Afficher la carte
carte_POL

#### Portugal ####
data_PRT_distinct <- data_PRT %>%
  distinct(lat, lon)

ma_carte <- leaflet(data_PRT_distinct) %>%
  addTiles()  # Ajouter des tuiles pour le fond de la carte (OpenStreetMap)

# Ajouter des marqueurs pour chaque point
ma_carte <- ma_carte %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = ~paste("Latitude: ", lat, "<br>Longitude: ", lon))

# Afficher la carte
ma_carte

# point nord Latitude: 41.5, Longitude: -6.875
# point ouest Latitude: 39, Longitude: -9.375
# point sud Latitude: 37.5, Longitude: -8.125

data_PRT_north <- data_PRT %>%
  filter(lat == 41.5, lon == -6.875)

data_PRT_west <- data_PRT %>%
  filter(lat == 39, lon == -9.375)

data_PRT_south <- data_PRT %>%
  filter(lat == 37.5, lon == -8.125)

data_PRT_filtered <- data_PRT %>%
  filter(
    (lat == 41.5 & lon == -6.875) |
      (lat == 39 & lon == -9.375) |
      (lat == 37.5 & lon == -8.125)
  )

data_PRT_filtered_distinct <- data_PRT_filtered %>%
  distinct(lat, lon)

carte_PRT <- leaflet(data_PRT_filtered_distinct) %>%
  addTiles()  # Ajouter des tuiles pour le fond de la carte (OpenStreetMap)

# Ajouter des marqueurs pour chaque point
carte_PRT <- carte_PRT %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = ~paste("Latitude: ", lat, "<br>Longitude: ", lon))

# Afficher la carte
carte_PRT
  