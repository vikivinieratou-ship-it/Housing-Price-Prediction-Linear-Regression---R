#Load Dataset
housing <- read.csv("housing.csv")

#Select Numeric Variables
housing_numeric <- housing[sapply(housing, is.numeric)]

# Handle missing values
housing_numeric[is.na(housing_numeric)] <- colMeans(housing_numeric, na.rm = TRUE)


#  Data Scaling
housing_scaled <- scale(housing_numeric)

#Correlation Analysis
cor_with_price <- cor(housing_scaled, housing_scaled[, "price"])

cor_with_price <- as.vector(cor_with_price)
names(cor_with_price) <- colnames(housing_scaled)

#Remove target variable
cor_with_price <- cor_with_price[names(cor_with_price) != "price"]

# Create dataframe
cor_df <- data.frame(
  feature = names(cor_with_price),
  correlation = cor_with_price
)

# Sort by importance
cor_df <- cor_df[order(abs(cor_df$correlation), decreasing = TRUE), ]

# Select top 3 features
selected_features <- cor_df[1:3, ]

# Plot correlations
barplot(
  selected_features$correlation,
  names.arg = selected_features$feature,
  col = "steelblue",
  main = "Top 3 Correlated Features with Price",
  ylab = "Correlation"
)

#Linear Regression Model

top_features <- selected_features$feature

formula <- as.formula(
  paste("price ~", paste(top_features, collapse = " + "))
)

lm_model <- lm(formula, data = as.data.frame(housing_scaled))

summary(lm_model)

#Model Evaluation

predicted_values <- predict(lm_model)

actual_values <- housing_scaled[, "price"]

mae <- mean(abs(actual_values - predicted_values))

cat("MAE:", round(mae, 4), "\n")


#De-normalization

price_mean <- mean(housing_numeric$price)
price_sd <- sd(housing_numeric$price)

denormalized_predictions <- (predicted_values * price_sd) + price_mean
denormalized_actual <- (actual_values * price_sd) + price_mean

#Visualization

plot(
  denormalized_actual,
  denormalized_predictions,
  main = "Predicted vs Actual Prices",
  xlab = "Actual Prices",
  ylab = "Predicted Prices",
  col = "blue",
  pch = 16
)

abline(a = 0, b = 1, col = "red", lty = 2)
