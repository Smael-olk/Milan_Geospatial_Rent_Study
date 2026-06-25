# ============================================================
# Milan Rent Price Prediction - Applied Statistics Classic Algorithms
# ============================================================
# Data Sources:
#   1. rentals_clean.csv - Rent/price data (target variable)
#   2. tpl_metropercorsi.csv - Milan metro line data
#   3. bikemi_clean.csv - Bike sharing station data
#
# Algorithms Implemented:
#   - Linear Regression
#   - Ridge Regression
#   - LASSO Regression
#   - Decision Tree (CART)
#   - Random Forest
# ============================================================


# --------------------------
# Step 1: Install and Load Required Packages
# --------------------------
cat("=== Step 1: Loading required packages ===\n")

required_packages <- c("tidyverse", "caret", "glmnet", "rpart", "rpart.plot",
                       "randomForest", "ggplot2", "corrplot",
                       "geosphere", "Metrics", "gridExtra")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE, quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Set random seed for reproducibility
set.seed(12345)

# --------------------------
# Step 2: Data Loading and Initial Exploration
# --------------------------
cat("\n=== Step 2: Data loading and initial exploration ===\n")

# Load datasets
rentals <- read.csv("rentals_clean.csv", stringsAsFactors = FALSE)
metro <- read.csv("tpl_metropercorsi.csv", stringsAsFactors = FALSE)
bikemi <- read.csv("bikemi_clean.csv", stringsAsFactors = FALSE)

cat("Rentals data dimensions:", dim(rentals), "\n")
cat("Metro data dimensions:", dim(metro), "\n")
cat("Bike sharing data dimensions:", dim(bikemi), "\n")

# Target variable distribution
cat("\nTarget variable (price) summary statistics:\n")
print(summary(rentals$price))

# --------------------------
# Step 3: Data Preprocessing
# --------------------------
cat("\n=== Step 3: Data preprocessing ===\n")

# 3.1 Check for missing values
cat("Missing values check:\n")
print(colSums(is.na(rentals)))

# Remove rows with missing values
rentals_clean <- rentals %>% drop_na()
cat("Records after removing missing values:", nrow(rentals_clean), "\n")

# 3.2 Outlier removal - IQR method for price
Q1 <- quantile(rentals_clean$price, 0.25)
Q3 <- quantile(rentals_clean$price, 0.75)
IQR_val <- Q3 - Q1
lower_bound <- Q1 - 3 * IQR_val
upper_bound <- Q3 + 3 * IQR_val

rentals_clean <- rentals_clean %>%
  filter(price >= lower_bound & price <= upper_bound)

cat("Records after removing price outliers:", nrow(rentals_clean), "\n")

# 3.3 Area outlier removal
rentals_clean <- rentals_clean %>%
  filter(sqm >= 10 & sqm <= 300)

cat("Records after removing area outliers:", nrow(rentals_clean), "\n")

# --------------------------
# Step 4: Feature Engineering
# --------------------------
cat("\n=== Step 4: Feature engineering ===\n")

# 4.1 Calculate distance to nearest metro station
calculate_min_distance <- function(lon, lat, poi_df, lon_col, lat_col) {
  distances <- apply(poi_df[, c(lon_col, lat_col)], 1, function(poi) {
    distHaversine(c(lon, lat), c(poi[lon_col], poi[lat_col]))
  })
  return(min(distances))
}

# Calculate distance to nearest metro station for each property
cat("Calculating distance to nearest metro station...\n")
rentals_clean$dist_to_metro <- mapply(
  calculate_min_distance,
  rentals_clean$longitude,
  rentals_clean$latitude,
  MoreArgs = list(poi_df = metro, lon_col = "LONG_X_4326_CENTROID", 
                  lat_col = "LAT_Y_4326_CENTROID")
)
rentals_clean$dist_to_metro <- rentals_clean$dist_to_metro / 1000  # Convert to kilometers

# Calculate distance to nearest bike sharing station
cat("Calculating distance to nearest bike sharing station...\n")
rentals_clean$dist_to_bikemi <- mapply(
  calculate_min_distance,
  rentals_clean$longitude,
  rentals_clean$latitude,
  MoreArgs = list(poi_df = bikemi, lon_col = "lon", lat_col = "lat")
)
rentals_clean$dist_to_bikemi <- rentals_clean$dist_to_bikemi / 1000  # Convert to kilometers

# 4.2 Create price per sqm feature (for analysis only, not used for prediction)
rentals_clean$price_per_sqm <- rentals_clean$price / rentals_clean$sqm

# 4.3 Categorical variable encoding
# Heating type encoding
cat("Heating type distribution:\n")
print(table(rentals_clean$heating))

rentals_clean <- rentals_clean %>%
  mutate(
    heating_central = ifelse(heating == "Centralizzato", 1, 0),
    heating_autonomous = ifelse(heating == "Autonomo", 1, 0),
    heating_none = ifelse(heating == "Assente", 1, 0)
  )

# 4.4 Neighborhood feature - average price by macrozone
macrozone_avg <- rentals_clean %>%
  group_by(macrozone) %>%
  summarise(
    macrozone_avg_price = mean(price),
    macrozone_count = n(),
    .groups = "drop"
  )

rentals_clean <- rentals_clean %>%
  left_join(macrozone_avg, by = "macrozone")

# 4.5 Select final features
features <- rentals_clean %>%
  select(
    # Basic property features
    sqm, n_rooms, n_bath, floor_num,
    
    # Amenity features
    has_elevator, feat_arredato, feat_balcone, feat_ascensore,
    feat_lavatrice, feat_aria_condizionata, feat_portineria,
    feat_terrazzo, feat_giardino, feat_lavastoviglie,
    
    # Location features
    dist_to_metro, dist_to_bikemi,
    
    # Heating features
    heating_central, heating_autonomous,
    
    # Neighborhood feature
    macrozone_avg_price,
    
    # Target variable
    price
  )

cat("\nFinal feature dimensions:", dim(features), "\n")
cat("Feature list:\n")
print(names(features))

# --------------------------
# Step 5: Correlation Analysis
# --------------------------
cat("\n=== Step 5: Correlation analysis ===\n")

cor_matrix <- cor(features)
high_cor <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
if (nrow(high_cor) > 0) {
  cat("Highly correlated feature pairs (|r| > 0.7):\n")
  for (i in 1:nrow(high_cor)) {
    if (high_cor[i,1] < high_cor[i,2]) {
      cat(sprintf("  %s <-> %s: %.3f\n",
                  rownames(cor_matrix)[high_cor[i,1]],
                  colnames(cor_matrix)[high_cor[i,2]],
                  cor_matrix[high_cor[i,1], high_cor[i,2]]))
    }
  }
}

# --------------------------
# Step 6: Train-Test Split
# --------------------------
cat("\n=== Step 6: Train-test split ===\n")

train_index <- createDataPartition(features$price, p = 0.8, list = FALSE)
train_data <- features[train_index, ]
test_data <- features[-train_index, ]

cat("Training set size:", nrow(train_data), "\n")
cat("Test set size:", nrow(test_data), "\n")

# Standardization (for linear models)
preprocess_params <- preProcess(train_data[, -ncol(train_data)], method = c("center", "scale"))
train_data_scaled <- predict(preprocess_params, train_data)
test_data_scaled <- predict(preprocess_params, test_data)

# --------------------------
# Step 7: Model Training and Evaluation
# --------------------------
cat("\n=== Step 7: Model training and evaluation ===\n")

# Define evaluation function
evaluate_model <- function(actual, predicted, model_name) {
  r2 <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  mape <- mean(abs((actual - predicted) / actual)) * 100
  
  return(data.frame(
    Model = model_name,
    R2 = round(r2, 4),
    RMSE = round(rmse, 2),
    MAE = round(mae, 2),
    MAPE = round(mape, 2)
  ))
}

results <- data.frame()

# --------------------------
# 7.1 Linear Regression
# --------------------------
cat("\nTraining Linear Regression model...\n")
lm_model <- lm(price ~ ., data = train_data)
lm_pred <- predict(lm_model, newdata = test_data)
results <- rbind(results, evaluate_model(test_data$price, lm_pred, "Linear Regression"))

# --------------------------
# 7.2 Ridge Regression
# --------------------------
cat("Training Ridge Regression model...\n")
x_train <- as.matrix(train_data_scaled[, -ncol(train_data_scaled)])
y_train <- train_data_scaled$price
x_test <- as.matrix(test_data_scaled[, -ncol(test_data_scaled)])
y_test <- test_data_scaled$price

# Cross-validation to select lambda
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0)
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = cv_ridge$lambda.min)
ridge_pred <- predict(ridge_model, newx = x_test)
results <- rbind(results, evaluate_model(y_test, ridge_pred[,1], "Ridge Regression"))

# --------------------------
# 7.3 LASSO Regression
# --------------------------
cat("Training LASSO Regression model...\n")
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = cv_lasso$lambda.min)
lasso_pred <- predict(lasso_model, newx = x_test)
results <- rbind(results, evaluate_model(y_test, lasso_pred[,1], "LASSO Regression"))

# Output LASSO selected features
cat("\nLASSO selected non-zero coefficient features:\n")
lasso_coef <- coef(lasso_model)
nonzero_features <- rownames(lasso_coef)[which(lasso_coef[,1] != 0)]
print(nonzero_features)

# --------------------------
# 7.4 Decision Tree (CART)
# --------------------------
cat("\nTraining Decision Tree model...\n")
tree_model <- rpart(price ~ ., data = train_data, 
                    control = rpart.control(cp = 0.01, minsplit = 20))
tree_pred <- predict(tree_model, newdata = test_data)
results <- rbind(results, evaluate_model(test_data$price, tree_pred, "Decision Tree"))

# --------------------------
# 7.5 Random Forest
# --------------------------
cat("Training Random Forest model...\n")
rf_model <- randomForest(price ~ ., data = train_data, 
                         ntree = 100, mtry = 5, importance = TRUE)
rf_pred <- predict(rf_model, newdata = test_data)
results <- rbind(results, evaluate_model(test_data$price, rf_pred, "Random Forest"))

# Output feature importance
cat("\nRandom Forest Top 10 Feature Importance:\n")
importance_df <- data.frame(
  Feature = rownames(importance(rf_model)),
  Importance = importance(rf_model)[,1]
) %>% arrange(desc(Importance))
print(head(importance_df, 10))

# --------------------------
# Step 8: Model Performance Comparison
# --------------------------
cat("\n=== Step 8: Model performance comparison ===\n")
print(results %>% arrange(desc(R2)))

# Find best model
best_model <- results %>% arrange(desc(R2)) %>% slice(1)
cat(sprintf("\nBest Model: %s (R² = %.4f, RMSE = %.2f)\n", 
            best_model$Model, best_model$R2, best_model$RMSE))

# --------------------------
# Step 9: Visualization
# --------------------------
cat("\n=== Step 9: Generating visualizations ===\n")

# 9.1 Model performance comparison plots
p1 <- ggplot(results, aes(x = reorder(Model, R2), y = R2, fill = Model)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Model R² Score Comparison", x = "Model", y = "R²") +
  theme_minimal() +
  theme(legend.position = "none")

p2 <- ggplot(results, aes(x = reorder(Model, RMSE), y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Model RMSE Comparison", x = "Model", y = "RMSE") +
  theme_minimal() +
  theme(legend.position = "none")

# 9.2 Predicted vs Actual scatter plot (best model)
if (best_model$Model == "Random Forest") {
  best_pred <- rf_pred
} else if (best_model$Model == "Linear Regression") {
  best_pred <- lm_pred
} else if (best_model$Model == "Ridge Regression") {
  best_pred <- ridge_pred[,1]
} else if (best_model$Model == "LASSO Regression") {
  best_pred <- lasso_pred[,1]
} else {
  best_pred <- rf_pred
}

p3 <- ggplot(data.frame(Actual = test_data$price, Predicted = best_pred), 
             aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = sprintf("%s: Predicted vs Actual Values", best_model$Model),
       x = "Actual Rent (EUR)", y = "Predicted Rent (EUR)") +
  theme_minimal()

# 9.3 Residual plot
residuals <- test_data$price - best_pred
p4 <- ggplot(data.frame(Predicted = best_pred, Residuals = residuals),
             aes(x = Predicted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residual Plot", x = "Predicted Values", y = "Residuals") +
  theme_minimal()

# Save plots
ggsave("model_comparison_r2.png", p1, width = 10, height = 6)
ggsave("model_comparison_rmse.png", p2, width = 10, height = 6)
ggsave("prediction_scatter.png", p3, width = 10, height = 8)
ggsave("residual_plot.png", p4, width = 10, height = 8)

# --------------------------
# Step 10: Save Results
# --------------------------
cat("\n=== Step 10: Saving results ===\n")

# Save model performance results
write.csv(results, "model_performance.csv", row.names = FALSE)

# Save feature importance
write.csv(importance_df, "rf_feature_importance.csv", row.names = FALSE)

cat("\n=== Analysis Complete! ===\n")
cat("Generated files:\n")
cat("  - model_performance.csv: Model performance comparison\n")
cat("  - rf_feature_importance.csv: Random Forest feature importance\n")
cat("  - *.png: Visualization plots\n")
