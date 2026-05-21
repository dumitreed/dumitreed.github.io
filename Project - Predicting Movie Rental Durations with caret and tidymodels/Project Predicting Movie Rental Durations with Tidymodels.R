# Imports
library(dplyr)
library(rsample)
library(tidymodels)
library(lubridate)
library(caret)
library(glmnet)
library(stringr)

# Import data
rental_info <- read.csv("rental_info.csv")

# Inspect data
str(rental_info)
summary(rental_info)

# Cleaning data
rental_info_clean <- rental_info %>%
  mutate(rental_date = ymd_hms(rental_date),
         return_date = ymd_hms(return_date),
         special_features = as.factor(special_features),
         Trailers = as.integer(ifelse(str_detect(special_features, "Trailers"), 1, 0)),
         Commentaries = as.integer(ifelse(str_detect(special_features, "Commentaries"), 1, 0)),
         Deleted_scenes = as.integer(ifelse(str_detect(special_features, "Deleted Scenes"), 1, 0)),
         Behind_the_scenes = as.integer(ifelse(str_detect(special_features, "Behind the Scenes"), 1, 0)),
         rental_period = as.integer(ceiling(time_length(difftime(return_date, rental_date), "days")))) %>%
  select(-special_features, -rental_date, -return_date)

# Reinspect data
str(rental_info_clean)
summary(rental_info_clean)
summary(rental_info_clean$special_features)

# Split data into training and testing
set.seed(13)
rental_info_split <- initial_split(rental_info_clean, prop = 3/4)
rental_info_train <- training(rental_info_split)
rental_info_test <- testing((rental_info_split))

# Create recipe
rental_info_recipe <- recipe(rental_period ~ ., data = rental_info_train)

# Build linear model
glmnet_mod <- linear_reg(penalty = 0.0000000001,
                         mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Set resampling method
folds <- vfold_cv(rental_info_train, v = 5, repeats = 5)

# Create workflow
glmnet_wf <- workflow() %>%
  add_model(glmnet_mod) %>%
  add_formula(rental_period ~ .)

# Fit with resampling and collect metrics
glmnet_fit_rs <- glmnet_wf %>%
  fit_resamples(folds)
collect_metrics(glmnet_fit_rs)
glmnet_fit <- glmnet_wf %>%
  fit(data = rental_info_train)

# Get predictions on test set
glmnet_pred <- predict(glmnet_fit, new_data = rental_info_test)
(lm_mse <- rmse_vec(truth = rental_info_test$rental_period, estimate = glmnet_pred$.pred) ^ 2)

# Build decision tree model
dt_mod <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")

# Create workflow
dt_wf <- workflow() %>%
  add_model(dt_mod) %>%
  add_formula(rental_period ~ .)

# Fit with resampling and collect metrics
dt_fit_rs <- dt_wf %>%
  fit_resamples(folds)
collect_metrics(dt_fit_rs)
dt_fit <- dt_wf %>%
  fit(data = rental_info_train)

# Get predictions on test set
dt_pred <- predict(dt_fit, new_data = rental_info_test)
(dt_mse <- rmse_vec(truth = rental_info_test$rental_period, estimate = dt_pred$.pred) ^ 2)

# Build randomForest model
rf_mod <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("regression")

# Create workflow
rf_wf <- workflow() %>%
  add_model(rf_mod) %>%
  add_formula(rental_period ~ .)

# Fit with resampling and collect metrics
rf_fit_rs <- rf_wf %>%
  fit_resamples(folds)
collect_metrics(rf_fit_rs)
rf_fit <- rf_wf %>%
  fit(data = rental_info_train)

# Get predictions on test set
rf_pred <- predict(rf_fit, new_data = rental_info_test)
(rf_mse <- rmse_vec(truth = rental_info_test$rental_period, estimate = rf_pred$.pred) ^ 2)
(best_model <- rf_fit)
(best_mse <- rf_mse)