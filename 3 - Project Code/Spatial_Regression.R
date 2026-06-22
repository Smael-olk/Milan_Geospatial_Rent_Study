# ==============================================================================
#  MILAN STUDENT HOUSING SPATIAL ANALYSIS
# ==============================================================================


#Bypass strict Windows SSL checks if OSRM throws errors again
Sys.setenv(CURL_SSL_BACKEND = "openssl")


# 0. Load Libraries
library(sf)
library(tidyverse)
library(spdep)        # For spatial weights
library(spatialreg)   # For Spatial Autoregressive Models (SAR)
library(GWmodel)      # For Geographically Weighted Regression (GWR)
library(osrm)         # For street network routing
library(mapview)      # For interactive mapping



# Load your cleaned data (Assuming it's already an sf object in CRS 4326)
clean_df <- read_csv("./treated_surface.csv") %>% select(-c(title,features,address,surface_n)) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)



# ==============================================================================
# PART 0: ENGINEER Transit FEATURES 
# ==============================================================================

print("Engineering Transit Features...")
# 1. Load your Metro data 
metro <- read_csv("./metro_stations.csv")%>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# 2. Transform everything to meters (Milan UTM 32632)
clean_df_m <- st_transform(clean_df, 32632)
metro_m <- st_transform(metro, 32632)

# 3. Calculate distance matrix from all apartments to all Metro stops
metro_dist_matrix <- st_distance(clean_df_m, metro_m)

# 4. Find the minimum distance to ANY Metro stop
min_metro_dist_meters <- as.numeric(apply(metro_dist_matrix, 1, min))

# 5. Convert to estimated walking time (minutes) using the 1.3 Tortuosity factor
clean_df$time_to_nearest_metro_mins <- (min_metro_dist_meters * 1.3) / 83.33

print("Metro proximity added!")


tram <- read_csv("./tram_stations.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

tram_m <- st_transform(tram, 32632)

# 3. Calculate distance matrix from all apartments to all tram stops
tram_dist_matrix <- st_distance(clean_df_m, tram_m)

# 4. Find the minimum distance to any Tram stop
min_tram_dist_meters <- as.numeric(apply(tram_dist_matrix, 1, min))

# 5. Convert to estimated walking time (minutes) using the 1.3 Tortuosity factor
clean_df$time_to_nearest_tram_mins <- (min_tram_dist_meters * 1.3) / 83.33

print("Tram proximity added!")


# ==============================================================================
# PART 1: ENGINEER ROUTING FEATURES (WITH CHUNKING)
# ==============================================================================

# 1. Define Major Milan University Hubs (WGS84)
unis <- data.frame(
  name = c("Politecnico (Leonardo)", "Politecnico (Bovisa)"),
  lon = c(9.2273, 9.1563),
  lat = c(45.4789, 45.5028)
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# print("Calculating network travel times in chunks to avoid rate limits...")
# 
# # 2. Setup Chunking Loop
# chunk_size <- 100
# n_rows <- nrow(clean_df)
# n_chunks <- ceiling(n_rows / chunk_size)
# all_min_times <- numeric(n_rows)
# 
# for (i in 1:n_chunks) {
#   cat(sprintf("Processing chunk %d of %d...\n", i, n_chunks))
# 
#   start_idx <- (i - 1) * chunk_size + 1
#   end_idx <- min(i * chunk_size, n_rows)
#   chunk <- clean_df[start_idx:end_idx, ]
# 
#   # Calculate walking duration
#   route_matrix <- osrmTable(
#     src = chunk,
#     dst = unis,
#     measure = "duration",
#     osrm.profile = "foot"
#   )
# 
#   # Extract minimum time
#   all_min_times[start_idx:end_idx] <- apply(route_matrix$durations, 1, min, na.rm = TRUE)
# 
#   # Be polite to the API
#   Sys.sleep(1.1)
# }
# 
# clean_df$min_time_to_uni_mins <- all_min_times
# 
# # Filter out failed routes
# model_df <- clean_df %>% filter(!is.na(min_time_to_uni_mins))
# write.csv(model_df, "./isochrone.csv", row.names = FALSE)
#          
# 
# print("Routing complete!")

raw_lines <- readLines("./isochrone.csv")
fixed_lines <- gsub("(c\\([^)]+\\))", "\"\\1\"", raw_lines)

model_df <- read.csv(text = fixed_lines, stringsAsFactors = FALSE) %>%
  mutate(geometry = str_remove_all(geometry, "c\\(|\\)")) %>%
  separate(geometry, into = c("longitude", "latitude"), sep = ",", convert = TRUE)

# ==============================================================================
# PART 2: FIX CO-LOCATION (MICRO-JITTERING)
# ==============================================================================
# To prevent nearest-neighbor algorithms from crashing when multiple rooms 
# are in the exact same apartment (distance = 0).

print("Applying micro-jitter to prevent zero-distance errors...")


model_df <- st_as_sf(
  model_df, 
  coords = c("longitude", "latitude"), 
  crs = 4326                          
)
# Transform to a metric CRS (Milan UTM = 32632) to jitter by meters
model_df_m <- st_transform(model_df, 32632)

# Add exactly 5 meters of random spatial noise to coordinates
model_df_jittered <- st_jitter(model_df_m, amount = 3) 

# Transform back to standard GPS coordinates (WGS84)
model_df_ready <- st_transform(model_df_jittered, 4326)


# ==============================================================================
# PART 3: THE GLOBAL STUDENT PREMIUM (SPATIAL LAG MODEL)
# ==============================================================================

# 1. Clean the data strictly 
df_clean <- model_df_ready %>%
  select(price_sqm, min_time_to_uni_mins, surface_room, heating, macrozone, geometry,microzone) %>%
  # Drop traditional NAs
  na.omit() %>%
  # Remove rows with empty or missing map coordinates
  filter(!st_is_empty(geometry)) %>%
  # Remove any mathematically invalid numbers (NaN or Inf)
  filter(is.finite(price_sqm) & is.finite(min_time_to_uni_mins) & is.finite(surface_room))

# 2. THE DIAGNOSTIC CHECK
n_rows_data <- nrow(df_clean)
coords <- st_coordinates(df_clean)
n_rows_coords <- nrow(coords)

print(paste("1. Dataframe rows:", n_rows_data))
print(paste("2. Coordinate matrix rows:", n_rows_coords))

knn_neighbors <- knn2nb(knearneigh(coords, k = 5)) 
listw_clean <- nb2listw(knn_neighbors, style = "W", zero.policy = TRUE)

print(paste("3. Spatial weights size:", length(listw_clean$neighbours)))


sar_model_safe <- stsls(
  price_sqm ~ min_time_to_uni_mins + surface_room + as.factor(heating), 
  data = df_clean, 
  listw = listw_clean, 
  zero.policy = TRUE
)

summary(sar_model_safe)

sar_model_safe_macro <- stsls(
  price_sqm ~ min_time_to_uni_mins + surface_room + as.factor(heating)+as.factor(macrozone), 
  data = df_clean, 
  listw = listw_clean, 
  zero.policy = TRUE
)

summary(sar_model_safe_macro)


# 1. Extract the coefficients from your model
rho <- sar_model_safe_macro$coefficients["Rho"]
beta <- sar_model_safe_macro$coefficients[-1] # Everything except Rho

# 2. Get the X matrix (your design matrix of predictors)
X <- model.matrix(price_sqm ~ min_time_to_uni_mins + surface_room + as.factor(heating)+as.factor(macrozone), data = df_clean)

# 3. Calculate the spatial lag of the dependent variable using the explicit package path
WY <- spdep::lag.listw(listw_clean, df_clean$price_sqm, zero.policy = TRUE)

# 4. Compute the prediction: (Rho * WY) + (X * Beta)
df_clean$predicted_price <- as.numeric((rho * WY) + (X %*% beta))

# View actual vs predicted
head(df_clean %>% dplyr::select(price_sqm, predicted_price))

df_clean <- df_clean %>%
  filter(price_sqm >= 10 & price_sqm <= 50)

# 1. Calculate the raw prediction errors
actuals <- df_clean$price_sqm
predictions <- df_clean$predicted_price
errors <- actuals - predictions

# 2. Compute Mean Absolute Error (MAE)
mae <- mean(abs(errors))

# 3. Compute Root Mean Squared Error (RMSE)
rmse <- sqrt(mean(errors^2))

# 4. Compute Mean Absolute Percentage Error (MAPE)
mape <- mean(abs(errors) / actuals) * 100

# Print the scorecard
print("--- MODEL PERFORMANCE SCORECARD ---")
print(paste("MAE : €", round(mae, 2), "per sqm"))
print(paste("RMSE: €", round(rmse, 2), "per sqm"))
print(paste("MAPE:", round(mape, 2), "% average error"))

# 1. Define your new property's attributes
new_apartment_data <- data.frame(
  min_time_to_uni_mins = 12.5,
  surface_room = 18,
  heating = "Centralizzato",
  macrozone = "Udine, Lambrate"
)

# 2. Create the X vector matching the exact structure of your model's coefficients
# (Intercept = 1, min_time = 12.5, surface = 18, Autonomo = 0, Centralizzato = 1)
X_new <- c(1, 12.5, 18, 0, 1) 

# 3. Create a proper spatial point for the new coordinates (WGS84)
new_point <- st_sfc(st_point(c(9.2250, 45.4780)), crs = 4326)

# 4. Calculate distances to all existing apartments to find its neighbors
distances <- st_distance(new_point, df_clean)

# 5. Grab the indices of the 5 closest existing properties
closest_indices <- order(distances)[1:5]

# 6. Calculate WY_new: the average price of those 5 nearest neighbors
WY_new <- mean(df_clean$price_sqm[closest_indices])

# 7. Compute the final spatial prediction: (Rho * WY_new) + Sum(X_new * Beta)
predicted_price_new <- as.numeric((rho * WY_new) + sum(X_new * beta))

print(paste("Predicted Price per SQM for the new apartment:", round(predicted_price_new, 2)))
