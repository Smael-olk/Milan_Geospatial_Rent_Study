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
library(car)

# raw_path <- here("1 - Data Extraction", "final_dataset.csv")
clean_df <- read_csv("./treated_surface.csv") %>% select(-c(title,features,address,surface_n))
clean_df <- clean_df[!is.na(clean_df$macrozone), ]

# MANOVA taking into account correlation between price_n and surface of room
manova_macro <- manova(cbind(price_n, surface_room) ~ macrozone, data = clean_df)
summary(manova_macro)

manova_heating <- manova(cbind(price_n, surface_room) ~ heating, data = clean_df)
summary(manova_heating)



# ANOVA model on MacroZone
model <- lm(price_sqm ~ macrozone, data = clean_df)
Anova(model, type = 2)


# ANOVA model on MicroZone
model <- lm(price_sqm ~ microzone, data = clean_df)
Anova(model, type = 2)




#### Plotting

# Calculate means and standard errors for the error bars
plot_data <- clean_df %>%
  group_by(macrozone) %>%
  summarise(
    mean_price = mean(price_sqm, na.rm = TRUE),
    sd_price = sd(price_sqm, na.rm = TRUE),
    n = n(),
    se = sd_price / sqrt(n)
  )

# Create a Mean Plot with Confidence Intervals
ggplot(plot_data, aes(x = reorder(macrozone, mean_price), y = mean_price)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = mean_price - 1.96 * se, ymax = mean_price + 1.96 * se), width = 0.2) +
  coord_flip() + # Makes long zone names easier to read
  labs(
    title = "Mean Price per Sqm by Milan Macrozone",
    subtitle = "Bars represent 95% Confidence Intervals",
    x = "Macrozone",
    y = "Mean Price per Sqm (€)"
  ) +
  theme_minimal()



