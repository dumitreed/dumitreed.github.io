# Import required libraries
library(readr)
library(dplyr)
library(tidyr)
library(glue)
library(yardstick)

# Start coding!

# Import data
car_insurance <- read_csv("car_insurance.csv")

# Clean data
car_insurance_clean <- car_insurance %>%
  select(-id) %>%
  mutate(age = as.factor(age),
         gender = as.factor(gender),
         race = as.factor(race),
         driving_experience = as.factor(driving_experience),
         education = as.factor(education),
         income = as.factor(income),
         vehicle_ownership = as.factor(vehicle_ownership),
         vehicle_year = as.factor(vehicle_year),
         married = as.factor(married),
         postal_code = as.factor(postal_code),
         vehicle_type = as.factor(vehicle_type),
         outcome = as.factor(outcome)
  ) %>%
  replace_na(list(credit_score = mean(car_insurance$credit_score, na.rm = TRUE),
                  annual_mileage = mean(car_insurance$annual_mileage, na.rm = TRUE)))

# Set up data frame with all features
logistic_regression_results <- data.frame(feature = colnames(car_insurance_clean)) %>%
  filter(feature != c("outcome")) %>%
  mutate(accuracy = NA)

# For loop to train model and get accuracy for each feature
for (col in logistic_regression_results$feature) {
  model <- glm(glue("outcome ~ {col}"), data = car_insurance_clean, family = "binomial")
  predictions <- round(fitted(model))
  accuracy <- mean(car_insurance_clean$outcome == predictions)
  logistic_regression_results[which(logistic_regression_results$feature == col), 2] <- accuracy
}

# Extract the best feature and accuracy
best_feature_df <- logistic_regression_results %>%
  slice_max(accuracy, n = 1) %>%
  select(best_feature = feature, best_accuracy = accuracy)