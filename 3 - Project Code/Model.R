library(sf)
library(tidyverse)
library(here)
library(mapview)
library(spdep)
library(ggplot2)
library(treemapify)
library(dplyr)
library(ggplot2)
library(gstat)
library(stars)
library(viridis)
library(writexl)
library(ggrepel) # Required for clear text labels
library(gstat)


# 1. Load data
# raw_path <- here("1 - Data Extraction", "final_dataset.csv")


clean_df <- read_csv("./treated_surface.csv") %>% select(-c(title,features,address,surface_n))



### conversion to Spatial Object
rent_sf <- clean_df %>%
  # Drop rows with missing coordinates (sf will throw an error otherwise)
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  
  # CRS 4326 is the standard WGS84 GPS coordinate system
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  
  # Reproject for Kriging 
  # CRS 32632 is UTM Zone 32N (The standard metric projection for Milan/Italy)
  st_transform(crs = 32632)


### Jittering for rooms on the same apartement

rent_sf <- rent_sf %>%
  st_jitter(amount = 1)


###### Testing for Autocorrelation : MicroScale : Jittering
coords <- st_coordinates(rent_sf)

# We use K-Nearest Neighbors 10 neighbours
knn_neighbors <- knearneigh(coords, k = 10)
nb <- knn2nb(knn_neighbors)

# Create a Spatial Weights Matrix
listw <- nb2listw(nb, style = "W")

# Run the Moran's I Test on your target variable (e.g., price_sqm)
moran.test(rent_sf$price_sqm, listw)

# 0.414 Test statistic, very strong and positive spatial autocorrelation.



#### Testing for Autocorrelation : MacroScale

# Aggregate to unique physical locations
rent_sf_agg <- rent_sf %>%
  group_by(geometry) %>%
  summarize(price_sqm = mean(price_sqm, na.rm = TRUE))

# 2. Re-run KNN and Moran's I
coords_agg <- st_coordinates(rent_sf_agg)
nb_agg <- knn2nb(knearneigh(coords_agg, k = 10))
listw_agg <- nb2listw(nb_agg, style = "W")

moran.test(rent_sf_agg$price_sqm, listw_agg)




#### Testing for autocorrelation : Neighbourhood 


# Aggregate identical coordinates into single "Building/Location" points
rent_points_clean <- rent_sf %>%
  group_by(geometry, macrozone, microzone) %>%
  summarize(
    # Average the price for the specific building/location
    price_sqm = mean(price_sqm, na.rm = TRUE),
    
    # Keep track of how many rooms were merged here 
    n_listings = n(), 
    .groups = "drop"
  )

#  Extract the clean coordinates
coords_clean <- st_coordinates(rent_points_clean)

#  Re-build the Spatial Weights Matrix (k=5)
nb_clean <- knn2nb(knearneigh(coords_clean, k = 5))
listw_clean <- nb2listw(nb_clean, style = "W")

#  Run the True Moran's I
moran.test(rent_points_clean$price_sqm, listw_clean)

















#### Universal Kriging

rent_points_clean <- rent_points_clean %>%
  drop_na(price_sqm, macrozone)


v_emp_spatial <- variogram(price_sqm ~ st_coordinates(rent_points_clean)[,1] + 
                             st_coordinates(rent_points_clean)[,2], 
                           data = rent_points_clean)

# Option B: Neighborhood Trend (Drift based on Zones)
# formula: price_sqm ~ macrozone
v_emp_zonal <- variogram(price_sqm ~ macrozone, data = rent_points_clean)

# Plot the variograms to visually inspect the spatial structure
plot(v_emp_zonal, 
     main = "Empirical Variogram (Residuals post-Macrozone Trend)",
     xlab = "Distance (meters)", 
     ylab = "Semivariance")










