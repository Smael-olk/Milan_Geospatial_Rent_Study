library(tidyverse)
library(MVN)
library(car)
library(here)
library(geosphere)
library(ggplot2)

library(sf)
library(mapview)


# read the data
rent_data <- read_csv("clean_data_onehot_encoded.csv")
head(rent_data)

bikemi_path <- here("1 - Data Extraction","bikemi_stazioni.csv")
bikemi <- read_delim(bikemi_path, delim = ";")


# clean bikemi coordinates
bikemi <- bikemi[!is.na(bikemi$LONG_X_4326) & !is.na(bikemi$LAT_Y_4326), ]

# density metric function
count_stations_within_radius <- function(lat, lon, stations_lat, stations_lon, radius_m) {
  distances <- distHaversine(
    cbind(lon, lat),
    cbind(stations_lon, stations_lat)
  )
  sum(distances <= radius_m)
}


####500M
# apply it to each room
radius <- 500  # in meters

rent_data$bikemi_density_500m <- mapply(
  count_stations_within_radius,
  rent_data$latitude,
  rent_data$longitude,
  MoreArgs = list(
    stations_lat = bikemi$LAT_Y_4326,
    stations_lon = bikemi$LONG_X_4326,
    radius_m = radius
  )
)


summary(rent_data$bikemi_density_500m)

ggplot() +
  geom_point(data = rent_data, aes(x = longitude, y = latitude, color = bikemi_density_500m), alpha = 0.5) +
  scale_color_viridis_c() +
  geom_point(data = bikemi, aes(x = LONG_X_4326, y = LAT_Y_4326), color = "red", size = 1) +
  labs(title = "Densité BikeMi autour des chambres", color = "Nb stations")


# Transformer en sf
bikemi_sf <- st_as_sf(bikemi, 
                      coords = c("LONG_X_4326", "LAT_Y_4326"), 
                      crs = 4326) %>%
  st_transform(32632)

rent_data_sf <- st_as_sf(rent_data, 
                         coords = c("longitude", "latitude"), 
                         crs = 4326)

rent_metric <- st_transform(rent_data_sf, 32632)


mapview(rent_metric, zcol = "bikemi_density_500m", layer.name = "BikeMi density 500m") +
  mapview(bikemi_sf, col.regions = "red", cex = 3, layer.name = "BikeMi stations")




####200M
# apply it to each room
radius <- 200  # in meters

rent_data$bikemi_density_200m <- mapply(
  count_stations_within_radius,
  rent_data$latitude,
  rent_data$longitude,
  MoreArgs = list(
    stations_lat = bikemi$LAT_Y_4326,
    stations_lon = bikemi$LONG_X_4326,
    radius_m = radius
  )
)

summary(rent_data$bikemi_density_200m)



# Transformer en sf
bikemi_sf <- st_as_sf(bikemi, 
                      coords = c("LONG_X_4326", "LAT_Y_4326"), 
                      crs = 4326) %>%
  st_transform(32632)

rent_data_sf <- st_as_sf(rent_data, 
                         coords = c("longitude", "latitude"), 
                         crs = 4326)

rent_metric <- st_transform(rent_data_sf, 32632)

mapview(rent_metric, zcol = "bikemi_density_200m", layer.name = "BikeMi density 200m") +
  mapview(bikemi_sf, col.regions = "red", cex = 3, layer.name = "BikeMi stations")


####100M
# apply it to each room
radius <- 100  # in meters

rent_data$bikemi_density_100m <- mapply(
  count_stations_within_radius,
  rent_data$latitude,
  rent_data$longitude,
  MoreArgs = list(
    stations_lat = bikemi$LAT_Y_4326,
    stations_lon = bikemi$LONG_X_4326,
    radius_m = radius
  )
)

summary(rent_data$bikemi_density_100m)

# Transformer en sf
bikemi_sf <- st_as_sf(bikemi, 
                      coords = c("LONG_X_4326", "LAT_Y_4326"), 
                      crs = 4326) %>%
  st_transform(32632)

rent_data_sf <- st_as_sf(rent_data, 
                         coords = c("longitude", "latitude"), 
                         crs = 4326)

rent_metric <- st_transform(rent_data_sf, 32632)

mapview(rent_metric, zcol = "bikemi_density_100m", layer.name = "BikeMi density 100m") +
  mapview(bikemi_sf, col.regions = "red", cex = 3, layer.name = "BikeMi stations")


####750M
# apply it to each room
radius <- 750  # in meters

rent_data$bikemi_density_750m <- mapply(
  count_stations_within_radius,
  rent_data$latitude,
  rent_data$longitude,
  MoreArgs = list(
    stations_lat = bikemi$LAT_Y_4326,
    stations_lon = bikemi$LONG_X_4326,
    radius_m = radius
  )
)

summary(rent_data$bikemi_density_750m)

# Transformer en sf
bikemi_sf <- st_as_sf(bikemi, 
                      coords = c("LONG_X_4326", "LAT_Y_4326"), 
                      crs = 4326) %>%
  st_transform(32632)

rent_data_sf <- st_as_sf(rent_data, 
                         coords = c("longitude", "latitude"), 
                         crs = 4326)

rent_metric <- st_transform(rent_data_sf, 32632)

mapview(rent_metric, zcol = "bikemi_density_750m", layer.name = "BikeMi density 750m") +
  mapview(bikemi_sf, col.regions = "red", cex = 3, layer.name = "BikeMi stations")





######## METRO
metro_path <- here("1 - Data Extraction","metro_stations.csv")
metro <- read_csv(metro_path)

metro_sf <- st_as_sf(metro, 
                     coords = c("lon", "lat"), 
                     crs = 4326)

mapview(metro_sf, col.regions = "blue", cex = 4, layer.name = "Metro stations")

####500M
# apply it to each room
radius <- 500  # in meters

rent_data$metro_density_500m <- mapply(
  count_stations_within_radius,
  rent_data$latitude,
  rent_data$longitude,
  MoreArgs = list(
    stations_lat = metro$lat,
    stations_lon = metro$lon,
    radius_m = radius
  )
)


summary(rent_data$metro_density_500m)


# Transformer en sf
metro_sf <- st_as_sf(metro, 
                      coords = c("lon", "lat"), 
                      crs = 4326) %>%
  st_transform(32632)

rent_data_sf <- st_as_sf(rent_data, 
                         coords = c("longitude", "latitude"), 
                         crs = 4326)

rent_metric <- st_transform(rent_data_sf, 32632)

mapview(rent_metric, zcol = "metro_density_500m", layer.name = "Metro density 500m") +
  mapview(metro_sf, col.regions = "red", cex = 3, layer.name = "metro stations")


###### TRY TO HAVE A MOBILITY SCORE 

# Normaliser les deux métriques entre 0 et 1
rent_data <- rent_data %>%
  mutate(
    bikemi_norm = (bikemi_density_500m - min(bikemi_density_500m)) / 
      (max(bikemi_density_500m) - min(bikemi_density_500m)),
    metro_norm  = (metro_density_500m - min(metro_density_500m)) / 
      (max(metro_density_500m) - min(metro_density_500m))
  )

# Score pondéré (80% métro, 20% BikeMi)
rent_data <- rent_data %>%
  mutate(mobility_score = 0.8 * metro_norm + 0.2 * bikemi_norm)


rent_data_sf <- st_as_sf(rent_data, 
                         coords = c("longitude", "latitude"), 
                         crs = 4326)

mapview(rent_data_sf, zcol = "mobility_score", layer.name = "Mobility score") +
  mapview(bikemi_sf, col.regions = "red", cex = 2, layer.name = "BikeMi") +
  mapview(metro_sf, col.regions = "blue", cex = 3, layer.name = "Metro")



######## TRAM
tram_path <- here("1 - Data Extraction","tram_stations.csv")
tram <- read_csv(tram_path)

rent_data$tram_density_500m <- mapply(
  count_stations_within_radius,
  rent_data$latitude,
  rent_data$longitude,
  MoreArgs = list(
    stations_lat = tram_stations$lat,
    stations_lon = tram_stations$lon,
    radius_m = 500
  )
)

tram_sf <- st_as_sf(tram_stations, coords = c("lon", "lat"), crs = 4326)

rent_data_sf <- st_as_sf(rent_data, 
                         coords = c("longitude", "latitude"), 
                         crs = 4326)

rent_metric <- st_transform(rent_data_sf, 32632)

mapview(rent_metric, zcol = "tram_density_500m", layer.name = "Tram density 500m") +
  mapview(tram_sf, col.regions = "orange", cex = 2, layer.name = "Tram") 




######## TRAIN
train_path <- here("1 - Data Extraction","train_stations.csv")
train_stations <- read_csv(train_path)

rent_data$train_density_500m <- mapply(
  count_stations_within_radius,
  rent_data$latitude,
  rent_data$longitude,
  MoreArgs = list(
    stations_lat = train_stations$lat,
    stations_lon = train_stations$lon,
    radius_m = 500
  )
)

train_sf <- st_as_sf(train_stations, coords = c("lon", "lat"), crs = 4326)

rent_data_sf <- st_as_sf(rent_data, 
                         coords = c("longitude", "latitude"), 
                         crs = 4326)

rent_metric <- st_transform(rent_data_sf, 32632)

mapview(rent_metric, zcol = "train_density_500m", layer.name = "Train stations density 500m") +
  mapview(train_sf, col.regions = "red", cex = 7, layer.name = "Train") 


########
# Normaliser les 4 métriques entre 0 et 1
rent_data <- rent_data %>%
  mutate(
    bikemi_norm = (bikemi_density_500m - min(bikemi_density_500m)) / 
      (max(bikemi_density_500m) - min(bikemi_density_500m)),
    metro_norm  = (metro_density_500m - min(metro_density_500m)) / 
      (max(metro_density_500m) - min(metro_density_500m)),
    tram_norm   = (tram_density_500m - min(tram_density_500m)) / 
      (max(tram_density_500m) - min(tram_density_500m)),
    train_norm  = (train_density_500m - min(train_density_500m)) / 
      (max(train_density_500m) - min(train_density_500m))
  )

# Score pondéré
rent_data <- rent_data %>%
  mutate(mobility_score = 0.50 * metro_norm + 
           0.25 * tram_norm + 
           0.15 * train_norm + 
           0.10 * bikemi_norm)

# Map
rent_data_sf <- st_as_sf(rent_data, 
                         coords = c("longitude", "latitude"), 
                         crs = 4326)

mapview(rent_data_sf, zcol = "mobility_score", layer.name = "Mobility Score") +
  mapview(bikemi_sf, col.regions = "red", cex = 2, layer.name = "BikeMi") +
  mapview(metro_sf, col.regions = "blue", cex = 3, layer.name = "Metro") +
  mapview(tram_sf, col.regions = "orange", cex = 2, layer.name = "Tram") +
  mapview(train_sf, col.regions = "green", cex = 3, layer.name = "Train")



# Without train stations
rent_data <- rent_data %>%
  mutate(
    bikemi_norm = (bikemi_density_500m - min(bikemi_density_500m)) / 
      (max(bikemi_density_500m) - min(bikemi_density_500m)),
    metro_norm  = (metro_density_500m - min(metro_density_500m)) / 
      (max(metro_density_500m) - min(metro_density_500m)),
    tram_norm   = (tram_density_500m - min(tram_density_500m)) / 
      (max(tram_density_500m) - min(tram_density_500m))
  ) %>%
  mutate(mobility_score = 0.55 * metro_norm + 
           0.30 * tram_norm + 
           0.15 * bikemi_norm)

rent_data_sf <- st_as_sf(rent_data, 
                         coords = c("longitude", "latitude"), 
                         crs = 4326)

mapview(rent_data_sf, zcol = "mobility_score", layer.name = "Mobility Score") +
  mapview(bikemi_sf, col.regions = "red", cex = 2, layer.name = "BikeMi") +
  mapview(metro_sf, col.regions = "blue", cex = 3, layer.name = "Metro") +
  mapview(tram_sf, col.regions = "orange", cex = 2, layer.name = "Tram")



### TO DO 
# decide which radius to choose
# -> (- Test correlations between price and density at different radii (100, 200, 500, 750m)
#    - For BikeMi: likely 300-500m (short walk)
#    - For Metro: likely 500-800m (people walk more for metro)
#    - For Tram: likely 300-500m
#    cor(rent_data$price_n, rent_data$bikemi_density_500m, use = "complete.obs")
#    cor(rent_data$price_n, rent_data$bikemi_density_200m, use = "complete.obs")
#    -> Keep the radius with highest correlation for each transport mode)
# 
# finalize mobility score
#
# save the final enriched dataset
# 