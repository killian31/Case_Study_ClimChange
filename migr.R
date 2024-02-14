library(readxl)

load("flow_data_for_students.RData") # load mig_data object
data_UKR <-
  read.csv("data/temperature_daily_grid_UKR.csv",
           header = TRUE,
           sep = ",")
data_ESP <-
  read.csv("data/temperature_daily_grid_ESP.csv",
           header = TRUE,
           sep = ",")
data_POL <-
  read.csv("data/temperature_daily_grid_POL.csv",
           header = TRUE,
           sep = ",")
data_PRT <-
  read.csv("data/temperature_daily_grid_PRT.csv",
           header = TRUE,
           sep = ",")
# filter the data in mig_data to keep only rows where origin is among UKR, ESP, POL, PRT

mig_data <-
  mig_data[mig_data$origin %in% c("UKR", "ESP", "POL", "PRT"),]

# load GDP_pwt1001.xlsx

gdp_data <- read_xlsx("GDP_pwt1001.xlsx", sheet = "Data")

# filter the data in gdp_data to keep only rows where country is among Ukraine, Spain, Poland, Portugal

gdp_data <-
  gdp_data[gdp_data$country %in% c("Ukraine", "Spain", "Poland", "Portugal"),]

# keep only the columns 1 to 5 and 7

gdp_data <- gdp_data[, c(1:5, 7)]

# gdp per capita

gdp_data$gdp_per_capita <- gdp_data$rgdpe / gdp_data$pop

# 5 years average

years <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020)

new_gdp_data <- data.frame()

for (i in 1:6) {
  # filter the data in gdp_data to keep only rows where year is among years[i] to years[i+1]-1
  gdp_data_5_years_UKR <-
    gdp_data[gdp_data$year %in% years[i]:(years[i + 1] - 1) &
               gdp_data$country == "Ukraine", ]
  gdp_data_5_years_ESP <-
    gdp_data[gdp_data$year %in% years[i]:(years[i + 1] - 1) &
               gdp_data$country == "Spain", ]
  gdp_data_5_years_POL <-
    gdp_data[gdp_data$year %in% years[i]:(years[i + 1] - 1) &
               gdp_data$country == "Poland", ]
  gdp_data_5_years_PRT <-
    gdp_data[gdp_data$year %in% years[i]:(years[i + 1] - 1) &
               gdp_data$country == "Portugal", ]
  # calculate the mean of the gdp per capita
  gdp_per_capita_5_years_UKR <-
    mean(gdp_data_5_years_UKR$gdp_per_capita, na.rm = TRUE)
  gdp_per_capita_5_years_ESP <-
    mean(gdp_data_5_years_ESP$gdp_per_capita, na.rm = TRUE)
  gdp_per_capita_5_years_POL <-
    mean(gdp_data_5_years_POL$gdp_per_capita, na.rm = TRUE)
  gdp_per_capita_5_years_PRT <-
    mean(gdp_data_5_years_PRT$gdp_per_capita, na.rm = TRUE)
  # add a row to new_gdp_data
  new_gdp_data <-
    rbind(
      new_gdp_data,
      data.frame(
        "country" = gdp_data_5_years_UKR$country[1],
        "countrycode" = gdp_data_5_years_UKR$countrycode[1],
        "year" = paste(years[i], years[i + 1], sep = "-"),
        gdp_per_capita = gdp_per_capita_5_years_UKR
      )
    )
  new_gdp_data <-
    rbind(
      new_gdp_data,
      data.frame(
        "country" = gdp_data_5_years_ESP$country[1],
        "countrycode" = gdp_data_5_years_ESP$countrycode[1],
        "year" = paste(years[i], years[i + 1], sep = "-"),
        gdp_per_capita = gdp_per_capita_5_years_ESP
      )
    )
  new_gdp_data <-
    rbind(
      new_gdp_data,
      data.frame(
        "country" = gdp_data_5_years_POL$country[1],
        "countrycode" = gdp_data_5_years_POL$countrycode[1],
        "year" = paste(years[i], years[i + 1], sep = "-"),
        gdp_per_capita = gdp_per_capita_5_years_POL
      )
    )
  new_gdp_data <-
    rbind(
      new_gdp_data,
      data.frame(
        "country" = gdp_data_5_years_PRT$country[1],
        "countrycode" = gdp_data_5_years_PRT$countrycode[1],
        "year" = paste(years[i], years[i + 1], sep = "-"),
        gdp_per_capita = gdp_per_capita_5_years_PRT
      )
    )
}

# ESP
data_ESP_max <-
  read.csv("output_data/ESP/aggregate_max_ESP.csv",
           header = TRUE,
           sep = ",")
data_ESP_mean <-
  read.csv("output_data/ESP/aggregate_mean_ESP.csv",
           header = TRUE,
           sep = ",")
data_ESP_min <-
  read.csv("output_data/ESP/aggregate_min_ESP.csv",
           header = TRUE,
           sep = ",")

# POL
data_POL_max <-
  read.csv("output_data/POL/aggregate_max_POL.csv",
           header = TRUE,
           sep = ",")
data_POL_mean <-
  read.csv("output_data/POL/aggregate_mean_POL.csv",
           header = TRUE,
           sep = ",")
data_POL_min <-
  read.csv("output_data/POL/aggregate_min_POL.csv",
           header = TRUE,
           sep = ",")

# PRT
data_PRT_max <-
  read.csv("output_data/PRT/aggregate_max_PRT.csv",
           header = TRUE,
           sep = ",")
data_PRT_mean <-
  read.csv("output_data/PRT/aggregate_mean_PRT.csv",
           header = TRUE,
           sep = ",")
data_PRT_min <-
  read.csv("output_data/PRT/aggregate_min_PRT.csv",
           header = TRUE,
           sep = ",")

# UKR
data_UKR_max <-
  read.csv("output_data/UKR/aggregate_max_UKR.csv",
           header = TRUE,
           sep = ",")
data_UKR_mean <-
  read.csv("output_data/UKR/aggregate_mean_UKR.csv",
           header = TRUE,
           sep = ",")
data_UKR_min <-
  read.csv("output_data/UKR/aggregate_min_UKR.csv",
           header = TRUE,
           sep = ",")

# > tail(mig_data)
# origin dest      year type hat_reverse_open
# 719736    PRT  WSM 2015-2020    f                0
# 719739    ESP  WSM 2015-2020    f                0
# 719912    POL  TON 2015-2020    f                0
# 719917    UKR  TON 2015-2020    f                0
# 719936    PRT  TON 2015-2020    f                0
# 719939    ESP  TON 2015-2020    f                0

# > head(data_ESP_max)
# Year   Minimum     Mean  Maximum Quantile_0.05 Quantile_0.10   Median Quantile_0.90 Quantile_0.95
# 1 1990 -6.590186 20.08854 42.77380      6.838272      8.910904 19.35705      32.47191      34.83091
# 2 1991 -7.330603 19.28598 45.79006      6.433296      8.292834 17.44102      33.18025      35.85313
# 3 1992 -3.690070 19.36007 46.96365      6.951781      9.025772 18.47169      31.60065      34.20706
# 4 1993 -5.530463 18.76111 43.25686      7.607523      9.422742 17.25367      31.03634      34.16572
# 5 1994 -6.616553 20.38727 44.30459      7.301877      9.695255 19.30953      33.14735      35.64144
# 6 1995 -7.824500 20.50296 44.58093      7.836160      9.987146 20.43977      31.29410      33.90198

create_new_data <- function(data, country, aggregate_type) {
  new_data <- data.frame()
  for (i in 1:6) {
    data_5_years <- data[data$Year %in% years[i]:(years[i + 1] - 1), ]
    if (aggregate_type == "max") {
      data_5_years <- mean(data_5_years$Maximum, na.rm = TRUE)
    } else if (aggregate_type == "mean") {
      data_5_years <- mean(data_5_years$Mean, na.rm = TRUE)
    } else if (aggregate_type == "min") {
      data_5_years <- mean(data_5_years$Minimum, na.rm = TRUE)
    }
    new_data <-
      rbind(new_data,
            data.frame(
              "countrycode" = country,
              "year" = paste(years[i], years[i + 1], sep = "-"),
              data_5_years
            ))
  }
  column_name <- paste(aggregate_type, "temperature", sep = "_")
  colnames(new_data) <- c("countrycode", "year", column_name)
  return(new_data)
}

new_data_ESP_max <- create_new_data(data_ESP_max, "ESP", "max")
new_data_ESP_mean <- create_new_data(data_ESP_mean, "ESP", "mean")
new_data_ESP_min <- create_new_data(data_ESP_min, "ESP", "min")

new_data_POL_max <- create_new_data(data_POL_max, "POL", "max")
new_data_POL_mean <- create_new_data(data_POL_mean, "POL", "mean")
new_data_POL_min <- create_new_data(data_POL_min, "POL", "min")

new_data_PRT_max <- create_new_data(data_PRT_max, "PRT", "max")
new_data_PRT_mean <- create_new_data(data_PRT_mean, "PRT", "mean")
new_data_PRT_min <- create_new_data(data_PRT_min, "PRT", "min")

new_data_UKR_max <- create_new_data(data_UKR_max, "UKR", "max")
new_data_UKR_mean <- create_new_data(data_UKR_mean, "UKR", "mean")
new_data_UKR_min <- create_new_data(data_UKR_min, "UKR", "min")

data_ESP <-
  merge(new_data_ESP_min,
        new_data_ESP_mean,
        by = c("countrycode", "year"))
data_ESP <-
  merge(data_ESP, new_data_ESP_max, by = c("countrycode", "year"))

data_POL <-
  merge(new_data_POL_min,
        new_data_POL_mean,
        by = c("countrycode", "year"))
data_POL <-
  merge(data_POL, new_data_POL_max, by = c("countrycode", "year"))

data_PRT <-
  merge(new_data_PRT_min,
        new_data_PRT_mean,
        by = c("countrycode", "year"))
data_PRT <-
  merge(data_PRT, new_data_PRT_max, by = c("countrycode", "year"))

data_UKR <-
  merge(new_data_UKR_min,
        new_data_UKR_mean,
        by = c("countrycode", "year"))
data_UKR <-
  merge(data_UKR, new_data_UKR_max, by = c("countrycode", "year"))

# rename column 'countrycode' to 'origin'
colnames(data_ESP)[1] <- "origin"
colnames(data_POL)[1] <- "origin"
colnames(data_PRT)[1] <- "origin"
colnames(data_UKR)[1] <- "origin"

# same for new_gdp_data
colnames(new_gdp_data)[2] <- "origin"

for (i in 1:nrow(mig_data)) {
  mig_data[i, "gdp_per_capita"] <-
    new_gdp_data[new_gdp_data$origin == mig_data[i, "origin"] &
                   new_gdp_data$year == mig_data[i, "year"],
                 "gdp_per_capita"]
}

merge_ESP <-
  merge(mig_data[mig_data$origin == "ESP", ], data_ESP, by = c("origin", "year"))
merge_POL <-
  merge(mig_data[mig_data$origin == "POL", ], data_POL, by = c("origin", "year"))
merge_PRT <-
  merge(mig_data[mig_data$origin == "PRT", ], data_PRT, by = c("origin", "year"))
merge_UKR <-
  merge(mig_data[mig_data$origin == "UKR", ], data_UKR, by = c("origin", "year"))

new_mig_data <- rbind(merge_ESP, merge_POL, merge_PRT, merge_UKR)

# > head(new_mig_data)
# origin      year dest type hat_reverse_open gdp_per_capita min_temperature mean_temperature max_temperature
# 1    ESP 1990-1995  BDI  tot                0       20143.52       -15.91306         13.22433        46.96365
# 2    ESP 1990-1995  COM  tot                0       20143.52       -15.91306         13.22433        46.96365
# 3    ESP 1990-1995  DJI  tot                0       20143.52       -15.91306         13.22433        46.96365
# 4    ESP 1990-1995  ERI  tot                0       20143.52       -15.91306         13.22433        46.96365
# 5    ESP 1990-1995  ETH  tot                0       20143.52       -15.91306         13.22433        46.96365
# 6    ESP 1990-1995  KEN  tot                0       20143.52       -15.91306         13.22433        46.96365
#
#
# compute_mean_lon_lat <- function(country) {
#   area <- world[world$ISO3 == country, ]
#   area_bbox <- st_bbox(area)
#   mean_lon_lat <- c(mean(area_bbox[1], area_bbox[3]), mean(area_bbox[2], area_bbox[4]))
#   return(mean_lon_lat)
# }
#
# # compute the mean longitude and latitude for each origin country (ESP, POL, PRT, UKR)
# mean_lon_lat_ESP <- compute_mean_lon_lat("ESP")
# mean_lon_lat_POL <- compute_mean_lon_lat("POL")
# mean_lon_lat_PRT <- compute_mean_lon_lat("PRT")
# mean_lon_lat_UKR <- compute_mean_lon_lat("UKR")
#
# # Add this information to new_mig_data
# new_mig_data$origin_lon <- NA
# new_mig_data$origin_lat <- NA
# new_mig_data$dest_lon <- NA
# new_mig_data$dest_lat <- NA
#
# for (i in 1:nrow(new_mig_data)) {
#   new_mig_data[i, "origin_lon"] <- switch(new_mig_data[i, "origin"],
#                                           "ESP" = mean_lon_lat_ESP[1],
#                                           "POL" = mean_lon_lat_POL[1],
#                                           "PRT" = mean_lon_lat_PRT[1],
#                                           "UKR" = mean_lon_lat_UKR[1])
#   new_mig_data[i, "origin_lat"] <- switch(new_mig_data[i, "origin"],
#                                           "ESP" = mean_lon_lat_ESP[2],
#                                           "POL" = mean_lon_lat_POL[2],
#                                           "PRT" = mean_lon_lat_PRT[2],
#                                           "UKR" = mean_lon_lat_UKR[2])
#   new_mig_data[i, "dest_lon"] <- compute_mean_lon_lat(new_mig_data[i, "dest"])[1]
#   new_mig_data[i, "dest_lat"] <- compute_mean_lon_lat(new_mig_data[i, "dest"])[2]
# }

library(rworldmap)
library(geosphere)

getCountryCoords <- function(iso3Code) {
  map <- getMap()
  countryIndex <- which(map$ISO3 == iso3Code)
  if (length(countryIndex) > 0) {
    lat <-
      mean(map$LAT[countryIndex], na.rm = TRUE) # Ensure NA values are removed
    lon <- mean(map$LON[countryIndex], na.rm = TRUE)
    # Debugging output
    return(c(lon, lat))
  } else {
    return(c(NA, NA))
  }
}

# now, given the origin and destination country codes, we can get the
# coordinates and compute the distHaversine to add it to new_mig_data
new_mig_data$origin_lon <- NA
new_mig_data$origin_lat <- NA
new_mig_data$dest_lon <- NA
new_mig_data$dest_lat <- NA
pb <- txtProgressBar(min = 0,
                     max = nrow(new_mig_data),
                     style = 3)
for (i in 1:nrow(new_mig_data)) {
  origin_coords <- getCountryCoords(new_mig_data[i, "origin"])
  dest_coords <- getCountryCoords(new_mig_data[i, "dest"])
  new_mig_data[i, "origin_lon"] <- origin_coords[1]
  new_mig_data[i, "origin_lat"] <- origin_coords[2]
  new_mig_data[i, "dest_lon"] <- dest_coords[1]
  new_mig_data[i, "dest_lat"] <- dest_coords[2]
  new_mig_data[i, "D_od"] <-
    distHaversine(origin_coords, dest_coords) / 1000 # in km
  setTxtProgressBar(pb, i)
}
close(pb)

# save the new_mig_data to a file
write.csv(new_mig_data, "output_data/new_mig_data.csv", row.names = FALSE)

# print head(new_mig_data) as a markdown table
head_data <- head(new_mig_data)
knitr::kable(head_data, format = "markdown")

summary(new_mig_data)
colnames(new_mig_data)
# [1] "origin"           "year"             "dest"             "type"             "hat_reverse_open" "gdp_per_capita"
# [7] "min_temperature"  "mean_temperature" "max_temperature"  "dist"             "origin_lon"       "origin_lat"
# [13] "dest_lon"         "dest_lat"

# summary statistics for the new_mig_data
summary(new_mig_data[, c(
  "hat_reverse_open",
  "gdp_per_capita",
  "min_temperature",
  "mean_temperature",
  "max_temperature",
  "dist"
)])

# plots

# plot the distribution of the hat_reverse_open variable
mig_data_flux <-
  new_mig_data[new_mig_data$hat_reverse_open > 20000, ]

hist(
  mig_data_flux$hat_reverse_open,
  breaks = 200,
  main = "Frequency histogram of the hat_reverse_open variable",
  xlab = "hat_reverse_open",
  ylab = "Frequency",
  col = "lightblue",
  border = "black",
  freq = FALSE
)


