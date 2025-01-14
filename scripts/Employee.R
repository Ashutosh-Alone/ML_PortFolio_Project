# Loading necessary libraries
library(reshape2)
library(glmnet)
library(caret)
library(ggplot2)
library(plyr)
library(dplyr)
library(skimr)
library(pROC)
library(Metrics)


# Read the dataset
data<- read.csv("C:/Users/ASHUTOSH/Desktop/DA/DMML/PROJECT/turnover.csv")
# Summary statistics
summary(data)
# Structure of the data
str(data)
# Use skimr for a concise summary
library(skimr)
skim(data)
# Display the first few rows
head(data)
# Check for missing values
missing_values <- sum(is.na(data))
missing_values
# Create a bar plot
ggplot(data, aes(x = factor(left), fill = factor(left))) +
  geom_bar() +
  scale_fill_manual(values = c("blue", "green")) +
  labs(title = "Distribution of Employee Attrition",
       x = "Attrition (1: Yes, 0: No)",
       y = "Count")
# Explore numeric variables with boxplots
numeric_vars <- sapply(data, is.numeric)
numeric_data <- data[, numeric_vars]
for (var in colnames(numeric_data)) {
  print(
    ggplot(data, aes(x = left, y = get(var), fill = factor(left))) +
      geom_boxplot() +
      labs(title = paste("Boxplot of", var),
           x = "Attrition (1: Yes, 0: No)",
           y = var)
  )
}
categorical_vars <- sapply(data, function(x) is.factor(x) | is.character(x))
categorical_data <- data[, categorical_vars]
for (var in colnames(categorical_data)) {
  print(
    ggplot(data, aes(x = get(var), fill = factor(left))) +
      geom_bar() +
      labs(title = paste("Distribution of", var),
           x = var,
           y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap(~left)
  )
}
cor_matrix <- cor(numeric_data)
ggplot(melt(cor_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed()
set.seed(123)
trainIndex <- createDataPartition(data$left, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

dim(train_data)
dim(test_data)
#apply logistic regression
logistic_model <- glm(left ~ ., data = train_data, family = "binomial")
head(logistic_model)
summary(logistic_model)
predictions <- predict(logistic_model, newdata = test_data, type = "response")
head(predictions)
summary(predictions)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
summary(predicted_classes)
conf_matrix <- confusionMatrix(table(predicted_classes, test_data$left))
accuracy <- conf_matrix$overall["Accuracy"]
print(accuracy)
roc_curve <- roc(test_data$left, predictions)
plot(roc_curve, col = "red", main = "ROC Curve", lwd = 2)

# Mean Absolute Error (MAE)
mae_value <- mae(test_data$left, predicted_classes)
cat("Mean Absolute Error (MAE):", mae_value, "\n")

# Root Mean Squared Error (RMSE)
rmse_value <- rmse(test_data$left, predicted_classes)
cat("Root Mean Squared Error (RMSE):", rmse_value, "\n")

# R-squared
rsquared_value <- R2(predicted_classes, test_data$left)
cat("R-squared:", rsquared_value, "\n")


