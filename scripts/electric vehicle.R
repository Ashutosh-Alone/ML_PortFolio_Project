# Install required packages if not already installed
install.packages(c("tidyverse", "caret", "randomForest", "gbm", "e1071"))
# Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)
library(gbm)
library(e1071)
library(class)
library(rpart)
library(pROC)
# Load the dataset
data <- read.csv("C:/Users/ASHUTOSH/Desktop/DA/DMML/PROJECT/Electric_Vehicle_Population_Data.csv")
# Display summary statistics
summary(data)

# Check for missing values
missing_values <- colSums(is.na(data))
print(missing_values)
# Print column names
print(colnames(data))
# Explore unique values in categorical columns
categorical_columns <- c("County", "Make", "Electric Vehicle Type", "Clean Alternative Fuel Vehicle (CAFV) Eligibility", "Electric Utility")

# Ensure correct column names
categorical_columns <- intersect(categorical_columns, colnames(data))

unique_values <- lapply(data[, categorical_columns, drop = FALSE], unique)
print(unique_values)
# Drop irrelevant columns
data <- data %>% select(-c("VIN..1.10.", "State", "Vehicle.Location"))

# Convert data types
data$`Electric.Vehicle.Type` <- as.factor(data$`Electric.Vehicle.Type`)
data$`Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility` <- as.factor(data$`Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility`)

# Handle missing values
data[, sapply(data, is.numeric)] <- lapply(data[, sapply(data, is.numeric)], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
# Create new features if needed
#For example, extract the year from Model Year
data$`Model.Year` <- as.factor(data$`Model.Year`)
data$YearExtracted <- as.numeric(sub(".*\\s(\\d{4})", "\\1", data$`Model.Year`))

# Select relevant features for modeling
selected_features <- c("Model.Year", "Make", "Electric.Vehicle.Type", "Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility", "Electric.Range", "Base.MSRP", "Legislative.District", "Electric.Utility", "YearExtracted")
data <- data[, selected_features]
# Visualization 1: Histogram of Electric Range
ggplot(data, aes(x = `Electric.Range`)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Electric Range", x = "Electric Range", y = "Frequency")

# Visualization 2: Boxplot of Electric Range by Electric Vehicle Type
ggplot(data, aes(x = `Electric.Vehicle.Type`, y = `Electric.Range`, fill = `Electric.Vehicle.Type`)) +
  geom_boxplot() +
  labs(title = "Electric Range by Vehicle Type", x = "Electric Vehicle Type", y = "Electric Range")

# Visualization 3: Scatterplot of Electric Range vs. Base MSRP
ggplot(data, aes(x = `Base.MSRP`, y = `Electric.Range`)) +
  geom_point(aes(color = `Electric.Vehicle.Type`)) +
  labs(title = "Scatterplot of Electric Range vs. Base MSRP", x = "Base MSRP", y = "Electric Range", color = "Vehicle Type")
set.seed(123)
split_index <- createDataPartition(data$`Electric.Range`, p = 0.7, list = FALSE)
train_data <- data[split_index, ]
test_data <- data[-split_index, ]
# Train Random Forest model
rf_model <- randomForest(`Electric.Range` ~ ., data = train_data)
# Make predictions on the test set
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate model performance
rf_performance <- postResample(pred = rf_predictions, obs = test_data$`Electric.Range`)
print(rf_performance)

# Convert relevant categorical variables to factors
train_data$Make <- as.factor(train_data$Make)
train_data$Electric.Vehicle.Type <- as.factor(train_data$Electric.Vehicle.Type)
train_data$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility <- as.factor(train_data$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)
train_data$Electric.Utility <- as.factor(train_data$Electric.Utility)


# Train Gradient Boosting model
gbm_model <- gbm(`Electric.Range` ~ ., data = train_data, distribution = "gaussian", n.trees = 100, interaction.depth = 5)
# Check for missing values in the test data
missing_values_test <- colSums(is.na(test_data))
print(missing_values_test)
# Handle missing values in the test data (replace missing numeric values with mean)
test_data[, sapply(test_data, is.numeric)] <- lapply(test_data[, sapply(test_data, is.numeric)], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
# Ensure column types in the test data
str(test_data)
# Convert "Make" to factor in the test data
test_data$Make <- as.factor(test_data$Make)
# Make predictions on the test set after handling missing values
gbm_predictions <- predict(gbm_model, newdata = test_data, n.trees = 100)
# Evaluate model performance
gbm_performance <- postResample(pred = gbm_predictions, obs = test_data$`Electric.Range`)
print(gbm_performance)




# Check levels in Electric.Utility column in both train and test datasets
unique_levels_train <- levels(train_data$Electric.Utility)
unique_levels_test <- levels(test_data$Electric.Utility)

# Identify new levels in the test dataset
new_levels_test <- setdiff(unique_levels_test, unique_levels_train)

# Replace new levels in the test dataset with a new level ("Other")
test_data$Electric.Utility <- factor(test_data$Electric.Utility, levels = c(unique_levels_train, "Other"))

# Now, train the Decision Tree model again
tree_model <- rpart(`Electric.Range` ~ ., data = train_data, method = "anova")
tree_predictions <- predict(tree_model, newdata = test_data)

# Evaluate model performance
tree_performance <- postResample(pred = tree_predictions, obs = test_data$`Electric.Range`)
print(tree_performance)

# Assuming you have the predictions and true labels for each model
# For Random Forest
rf_roc <- roc(test_data$`Electric.Range`, rf_predictions)

# For Gradient Boosting
gbm_roc <- roc(test_data$`Electric.Range`, gbm_predictions)

# For Decision Tree
tree_roc <- roc(test_data$`Electric.Range`, tree_predictions)

# Plot ROC curves
par(pty = "s")  # Set plot type to square
plot(rf_roc, col = "blue", main = "ROC Curves", lwd = 2, cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2)
plot(gbm_roc, col = "green", add = TRUE, lwd = 2)
plot(tree_roc, col = "red", add = TRUE, lwd = 2)





# Metrics for Random Forest
random_forest_metrics <- list(RMSE =36.9000791 , R_squared = 0.9133217, MAE = 17.9460951)

# Metrics for Gradient Boosting
gbm_metrics <- list(RMSE = 8.707735, R_squared = 0.991813, MAE = 4.109235)

# Metrics for Decision Tree
tree_metrics <- list(RMSE = 18.9240663, R_squared = 0.9611847, MAE = 11.4043255)

# Create a data frame for model performance comparison
model_metrics <- data.frame(
  Model = c("Random Forest", "Gradient Boosting", "Decision Tree"),
  RMSE = c(random_forest_metrics$RMSE, gbm_metrics$RMSE, tree_metrics$RMSE),
  R_squared = c(random_forest_metrics$R_squared, gbm_metrics$R_squared, tree_metrics$R_squared),
  MAE = c(random_forest_metrics$MAE, gbm_metrics$MAE, tree_metrics$MAE)
)

# Model Performance Comparison: Bar Chart
ggplot(model_metrics, aes(x = Model)) +
  geom_bar(aes(y = RMSE), fill = "skyblue", stat = "identity") +
  geom_bar(aes(y = R_squared), fill = "orange", stat = "identity") +
  geom_bar(aes(y = MAE), fill = "lightgreen", stat = "identity") +
  labs(title = "Model Performance Comparison", y = "Performance Metric", x = "Model") +
  theme_minimal()
























