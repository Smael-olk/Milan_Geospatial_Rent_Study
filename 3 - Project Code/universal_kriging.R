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
rent_points_clean <- clean_df %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  
  st_transform(crs = 32632)

rent_ready <- rent_points_clean %>%
  drop_na(price_sqm) %>%
  mutate(
    X = st_coordinates(.)[, 1],
    Y = st_coordinates(.)[, 2]
  )

# We use a 2nd-order polynomial trend to capture the macro-gradient across Milan
trend_formula <- price_sqm ~ X + Y + I(X^2) + I(Y^2) + I(X*Y)

# Compute the empirical variogram on the residuals of this trend
v_empirical <- variogram(trend_formula, data = rent_ready)

# Fit a theoretical model (e.g., Spherical or Exponential) to the empirical points
# vgm(psill, model, range, nugget)
# Adjust these initial values based on your visual inspection of the new plot
v_fitted <- fit.variogram(v_empirical, model = vgm(psill = 100, "Sph", range = 1500, nugget = 300))

# Plot to verify if the Pure Nugget Effect is resolved
plot(v_empirical, v_fitted, main = "Fitted Residual Variogram")

# ==============================================================================
# 4. Create a Target Prediction Grid over Milan
# ==============================================================================
# Create a bounding box grid around your data points with a 100-meter resolution
milan_bbox <- st_bbox(rent_ready)
grid_spacing <- 100 # meters

grid_x <- seq(milan_bbox["xmin"], milan_bbox["xmax"], by = grid_spacing)
grid_y <- seq(milan_bbox["ymin"], milan_bbox["ymax"], by = grid_spacing)

prediction_grid <- expand.grid(X = grid_x, Y = grid_y) %>%
  st_as_sf(coords = c("X", "Y"), crs = 32632, remove = FALSE)

# ==============================================================================
# 5. Run Universal Kriging Execution
# ==============================================================================
kriging_output <- krige(
  formula = trend_formula,
  locations = rent_ready,
  newdata = prediction_grid,
  model = v_fitted
)

# ==============================================================================
# 6. Map the Unobserved Spatial Premium
# ==============================================================================
# Rename prediction columns for clarity
prediction_grid$predicted_price <- kriging_output$var1.pred
prediction_grid$kriging_variance <- kriging_output$var1.var

# Plot the final interpolated surface using ggplot2
ggplot() +
  geom_sf(data = prediction_grid, aes(color = predicted_price), size = 0.8) +
  geom_sf(data = rent_ready, color = "black", size = 1, alpha = 0.5) +
  scale_color_viridis_c(option = "plasma", name = "EUR / m²") +
  labs(
    title = "Universal Kriging Surface: Milan Room Prices",
    subtitle = "Combined Deterministic Trend + Spatial Residuals Interpolation",
    x = "Easting (meters)",
    y = "Northing (meters)"
  ) +
  theme_minimal()