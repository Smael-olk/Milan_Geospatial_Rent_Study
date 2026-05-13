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
# 1. Load data
raw_path <- here("1 - Data Extraction", "final_dataset.csv")
rent_data <- read_csv(raw_path)



clean_df <- rent_data %>%
  mutate(
    price_n   = readr::parse_number(price_value),
    surface_n = readr::parse_number(surface),
    rooms_n   = readr::parse_number(rooms),
    price_sqm = price_n / surface_n
  ) %>%
  # 1. REMOVE IMPOSSIBLE DATA (Global Outliers)
  filter(
    !is.na(latitude), 
    !is.na(longitude),
    price_n > 100,          # Remove fake/placeholder prices like €1
    surface_n > 5,          # Remove impossible 1m2 rooms
    price_sqm > 5,          # Min price in Milan periphery
    price_sqm < 300         # Remove ultra-luxury penthouses (not for students)
  )%>% select(-c(photo_count,url,description, phone_number,agency_name,price_value,price_formatted))
  
  
#  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

clean_df %>% write_xlsx("milan_rent_clean.xlsx")

############################################







# 3. Quick ESDA Plot
# This will show you where the expensive areas are in Milan
mapview(clean_df, zcol = "price_sqm", layer.name = "Euro/m²")


# Transform your data to meters too
rent_metric <- st_transform(clean_df, 32632)

# Create a smoothed dataframe by Microzone
df_smoothed <- rent_metric %>%
  group_by(macrozone) %>%
  summarize(
    avg_price_sqm = mean(price_sqm, na.rm = TRUE),
    listing_count = n(),
    # Geometry of the group (centroids)
    geometry = st_centroid(st_union(geometry)) 
  )

mapview(df_smoothed, 
        zcol = "avg_price_sqm", 
        cex = "listing_count",
        layer.name = "Avg Price (€/m²)") +
  mapview(df_smoothed, 
          zcol = "macrozone", 
          legend = TRUE, 
          layer.name = "Macrozone Area")

##############################################################################

# 1. Clean data: Remove NAs and extreme outliers
rent_ready <- rent_metric %>%
  filter(!is.na(price_sqm)) %>%
  filter(price_sqm > 5 & price_sqm < 150)

# 2. Create a much FINER grid (Higher resolution)
# 50m cells instead of 100m will make it look much smoother
grid_fine <- st_bbox(rent_ready) %>%
  st_as_stars(dx = 20, dy = 20) 

# 3. Interpolate with Smoothing
# nmax = 20: Each pixel is an average of the 20 nearest points (blends the data)
# idp = 2: Standard weight (Inverse Distance Power)
idw_smooth <- idw(price_sqm ~ 1, rent_ready, grid_fine, nmax = 20, idp = 2)

# 4. Plot with geom_stars (Continuous look)
ggplot() +
  geom_stars(data = idw_smooth, aes(fill = var1.pred, x = x, y = y)) +
  scale_fill_viridis_c(option ="C", name = "€/m²", na.value = "transparent") +
  theme_void() + # Clean map look
  labs(title = "Milan Continuous Rental Price Surface",
       subtitle = "Inverse Distance Weighting (n=20)")

################################################################################


# Define the center of Milan (Duomo)
duomo <- st_sfc(st_point(c(9.1899, 45.4642)), crs = 4326) %>% 
  st_transform(32632) # Transform to meters

# Transform your data to meters too
rent_metric <- st_transform(clean_df, 32632)

# Calculate distance in kilometers
rent_metric$dist_to_cbd <- as.numeric(st_distance(rent_metric, duomo)) / 1000

# Plot the relationship
ggplot(rent_metric, aes(x = dist_to_cbd, y = price_sqm)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "gam") +
  labs(title = "Rent Decay: Price/m² vs Distance to Duomo",
       x = "Distance (km)", y = "€ / m²")


zone_analysis <- clean_df %>%
  st_drop_geometry() %>% 
  group_by(macrozone, microzone) %>%
  summarise(
    count = n(),
    avg_price_sqm = mean(price_sqm, na.rm = TRUE),
    price_sd = sd(price_sqm, na.rm = TRUE),
    .groups = "drop"
  )


ggplot(clean_df, aes(x = microzone, y = price_sqm, fill = macrozone)) +
  geom_boxplot() +
  facet_wrap(~macrozone, scales = "free_x") +
  theme_minimal() +
  labs(title = "Price Distribution: Microzones within Macrozones",
       y = "Price per m²",
       x = "Microzone") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


zone_gap <- clean_df %>%
  st_drop_geometry() %>%
  group_by(macrozone, microzone) %>%
  summarise(avg_sqm = mean(price_sqm, na.rm = TRUE), .groups = "drop_last") %>%
  summarise(
    n_microzones = n(),
    price_range = max(avg_sqm) - min(avg_sqm),
    cheapest = microzone[which.min(avg_sqm)],
    priciest = microzone[which.max(avg_sqm)]
  ) %>%
  arrange(desc(price_range))

# View the most "diverse" macrozones
head(zone_gap)


# Identify the 10 macrozones with the most internal price variation
top_10_varied <- zone_gap %>% slice_max(price_range, n = 10) %>% pull(macrozone)

clean_df %>%
  filter(macrozone %in% top_10_varied) %>%
  ggplot(aes(x = reorder(macrozone, price_sqm, FUN = median), y = price_sqm)) +
  geom_boxplot(outlier.shape = NA, fill = "skyblue", alpha = 0.5) +
  geom_jitter(aes(color = microzone), width = 0.2, alpha = 0.3) + 
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") + # Hide legend because there are too many microzones
  labs(title = "Top 10 Macrozones with Widest Price Gaps",
       subtitle = "Dots represent individual listings colored by Microzone",
       x = "Macrozone", y = "Price per sqm")





# 1. Prepare and Clean Data
tree_data <- clean_df %>%
  st_drop_geometry() %>%
  filter(!is.na(macrozone), !is.na(microzone), !is.na(price_sqm)) %>%
  group_by(macrozone, microzone) %>%
  summarise(
    avg_price = mean(price_sqm, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  # Filter out groups with 0 or NA values that break the geometry
  filter(n > 0, avg_price > 0)

# 2. Plot with a check
if(nrow(tree_data) > 0) {
  ggplot(tree_data, aes(area = n, fill = avg_price, label = microzone, subgroup = macrozone)) +
    geom_treemap() +
    geom_treemap_subgroup_border(colour = "white", size = 2) +
    geom_treemap_subgroup_text(place = "centre", grow = FALSE, alpha = 0.5, colour = "black", min.size = 8) +
    scale_fill_viridis_c(name = "Price/sqm", option = "plasma") +
    labs(title = "Rent Market Hierarchy")
} else {
  print("No data available to plot!")
}



clean_df %>%
  st_drop_geometry() %>%
  group_by(macrozone) %>%
  summarise(mean_price = mean(price_sqm, na.rm = TRUE)) %>%
  filter(!is.na(macrozone)) %>%
  ggplot(aes(x = reorder(macrozone, mean_price), y = mean_price)) +
  geom_segment(aes(xend = macrozone, yend = 0), color = "grey") +
  geom_point(size = 4, color = "orange") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Macrozones Ranked by Price", x = "", y = "Avg Price per sqm")
