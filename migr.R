library(readxl)
source("aggregate_data.R")

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

# convert kelvin to celsius
kelvin_to_celsius <- function(data) {
  data$temperature_min <- data$temperature_min - 273.15
  data$temperature_mean <- data$temperature_mean - 273.15
  data$temperature_max <- data$temperature_max - 273.15
  return(data)
}
data_UKR <- kelvin_to_celsius(data_UKR)
data_ESP <- kelvin_to_celsius(data_ESP)
data_POL <- kelvin_to_celsius(data_POL)
data_PRT <- kelvin_to_celsius(data_PRT)

# filter the data in mig_data to keep only rows where origin is among UKR, ESP, POL, PRT1
mig_data <-
  mig_data[mig_data$origin %in% c("UKR", "ESP", "POL", "PRT"),]

# load GDP_pwt1001.xlsx
gdp_data <- read_xlsx("GDP_pwt1001.xlsx", sheet = "Data")

# filter the data in gdp_data to keep only rows where country is among Ukraine, Spain, Poland, Portugal
# gdp_data <-
#   gdp_data[gdp_data$country %in% c("Ukraine", "Spain", "Poland", "Portugal"),]

# keep only the columns 1 to 5 and 7
gdp_data <- gdp_data[, c(1:5, 7)]

# gdp per capita
gdp_data$gdp_per_capita <- gdp_data$rgdpe / gdp_data$pop

# 5 years average
years <- c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)

new_gdp_data <- data.frame()

# Get unique list of countries from gdp_data
unique_countries <- unique(gdp_data$country)

for (country in unique_countries) {
  for (i in 1:8) {
    # Filter the data in gdp_data for the current country and the current 5-year period
    gdp_data_5_years <- gdp_data[gdp_data$year %in% years[i]:(years[i + 1] - 1) &
                                   gdp_data$country == country, ]
    
    # Calculate the mean of the gdp per capita for the current country and 5-year period
    gdp_per_capita_5_years <- mean(gdp_data_5_years$gdp_per_capita, na.rm = TRUE)
    
    # Add a row to new_gdp_data for the current country and 5-year period
    new_gdp_data <- rbind(new_gdp_data, data.frame(
      "country" = country,
      "countrycode" = gdp_data_5_years$countrycode[1], # Assuming countrycode is consistent within each country
      "year" = paste(years[i], years[i + 1], sep = "-"),
      "gdp_per_capita" = gdp_per_capita_5_years
    ))
  }
}

data_UKR_mean <- calculateYearlyStatistics(data_UKR, 1980, 2020, "temperature_mean")
data_UKR_min <- calculateYearlyStatistics(data_UKR, 1980, 2020, "temperature_min")
data_UKR_max <- calculateYearlyStatistics(data_UKR, 1980, 2020, "temperature_max")

data_ESP_mean <- calculateYearlyStatistics(data_ESP, 1980, 2020, "temperature_mean")
data_ESP_min<- calculateYearlyStatistics(data_ESP, 1980, 2020, "temperature_min")
data_ESP_max <- calculateYearlyStatistics(data_ESP, 1980, 2020, "temperature_max")

data_POL_mean <- calculateYearlyStatistics(data_POL, 1980, 2020, "temperature_mean")
data_POL_min <- calculateYearlyStatistics(data_POL, 1980, 2020, "temperature_min")
data_POL_max <- calculateYearlyStatistics(data_POL, 1980, 2020, "temperature_max")

data_PRT_mean <- calculateYearlyStatistics(data_PRT, 1980, 2020, "temperature_mean")
data_PRT_min <- calculateYearlyStatistics(data_PRT, 1980, 2020, "temperature_min")
data_PRT_max <- calculateYearlyStatistics(data_PRT, 1980, 2020, "temperature_max")

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
  for (i in 1:8) {
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

write.csv(new_data_ESP_mean, "output_data/ESP/aggregate_mean_ESP.csv", row.names=FALSE)
write.csv(new_data_ESP_max, "output_data/ESP/aggregate_max_ESP.csv", row.names=FALSE)
write.csv(new_data_ESP_min, "output_data/ESP/aggregate_min_ESP.csv", row.names=FALSE)

write.csv(new_data_POL_mean, "output_data/POL/aggregate_mean_POL.csv", row.names=FALSE)
write.csv(new_data_POL_max, "output_data/POL/aggregate_max_POL.csv", row.names=FALSE)
write.csv(new_data_POL_min, "output_data/POL/aggregate_min_POL.csv", row.names=FALSE)

write.csv(new_data_PRT_mean, "output_data/PRT/aggregate_mean_PRT.csv", row.names=FALSE)
write.csv(new_data_PRT_max, "output_data/PRT/aggregate_max_PRT.csv", row.names=FALSE)
write.csv(new_data_PRT_min, "output_data/PRT/aggregate_min_PRT.csv", row.names=FALSE)

write.csv(new_data_UKR_mean, "output_data/UKR/aggregate_mean_UKR.csv", row.names=FALSE)
write.csv(new_data_UKR_max, "output_data/UKR/aggregate_max_UKR.csv", row.names=FALSE)
write.csv(new_data_UKR_min, "output_data/UKR/aggregate_min_UKR.csv", row.names=FALSE)


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
  # add destination country's GDP per capita to migration data. If error, set to NA with tryCatch
  tryCatch({
    mig_data[i, "gdp_per_capita"] <-
      new_gdp_data[new_gdp_data$origin == mig_data[i, "dest"] &
                     new_gdp_data$year == mig_data[i, "year"],
                   "gdp_per_capita"]
  }, error = function(e) {
    mig_data[i, "gdp_per_capita"] <- NA
  })
}

# print proportion of missing values
print(sum(is.na(mig_data$gdp_per_capita)) / nrow(mig_data))

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
write.csv(new_mig_data, "output_data/new_mig_data_a.csv", row.names = FALSE)

# find row with max value for D_od
max_D_od <- new_mig_data[which.max(new_mig_data$D_od), ]
max_D_od

# find index of row at which col origin changes first time to POL
first_POL <- which(new_mig_data$origin == "POL")[1]

# rm col type, origin_lon, origin_lat, dest_lon, dest_lat
new_mig_data <- new_mig_data[, -c(4, 10:13)]
# rename hat_reverse_open col to Y_odt
colnames(new_mig_data)[4] <- "Y_odt"
sample <- new_mig_data[(first_POL - 3):(first_POL + 2), ]
sample

write.csv(new_mig_data, "output_data/final_database_a.csv", row.names = FALSE)
