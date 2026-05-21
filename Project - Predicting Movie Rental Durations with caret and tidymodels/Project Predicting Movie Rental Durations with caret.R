# Imports
library(dplyr)
library(rsample)
library(tidymodels)
library(lubridate)
library(caret)
library(glmnet)
library(stringr)
library(kernlab)
library(plyr)
library(gbm)
library(ranger)
library(e1071)
library(xgboost)
library(tictoc)

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
train_index <- createDataPartition(rental_info_clean$rental_period,
                                   p = 0.8,
                                   list = FALSE)
rental_info_train <- rental_info_clean[train_index, ]
rental_info_test <- rental_info_clean[-train_index, ]
train_x <- rental_info_train %>% select(-rental_period)
train_y <- rental_info_train %>% select(rental_period) %>% unlist()
test_x <- rental_info_test %>% select(-rental_period)
test_y <- rental_info_test %>% select(rental_period) %>% unlist()

# Linear model
glmnet_mod <- train(x = train_x,
                 y = train_y,
                 method = "glmnet")
glmnet_pred <- predict(glmnet_mod, newdata = test_x)
(glmnet_mse <- mean((test_y - glmnet_pred)^2))

# Linear SVM
svmlin_mod <- train(x = train_x,
                    y = train_y,
                    method = "svmLinear")
svmlin_pred <- predict(svmlin_mod, newdata = test_x)
(svmlin_mse <- mean((test_y - svmlin_pred)^2))

# Stochastic gradient boosting
gbm_mod <- train(x = train_x,
                 y = train_y,
                 method = "gbm",
                 verbose = FALSE)
gbm_pred <- predict(gbm_mod, newdata = test_x)
(gbm_mse <- mean((test_y - gbm_pred)^2))

# Random forest
rf_mod <- train(x = train_x,
                y = train_y,
                method = "ranger",
                verbose = FALSE)
rf_pred <- predict(rf_mod, newdata = test_x)
(rf_mse <- mean((test_y - rf_pred)^2))

# eXtreme Gradient Boosting
xgb_mod <- train(x = train_x,
                 y = train_y,
                 method = "xgbTree",
                 verbosity = 0)
xgb_pred <- predict(xgb_mod, newdata = test_x)
(xgb_mse <- mean((test_y - xgb_pred)^2))

# Comparison of all MSE
model_comparison <- tibble(model = c("glmnet", "svmLinear", "gbm", "ranger", "xgbTree"), mse = c(glmnet_mse, svmlin_mse, gbm_mse, rf_mse, xgb_mse))
model_comparison

# Brief tunning of randomForest model
rf_mod
fitControl <- trainControl(method = "repeatedcv",
                           number = 2,
                           repeats = 2)
rf_grid <- expand.grid(mtry = c(5, 7, 9),
                       splitrule = "extratrees",
                       min.node.size = c(3, 5, 7))
tic()
rf_mod_tune <- train(x = train_x,
                     y = train_y,
                     method = "ranger",
                     trControl = fitControl,
                     tuneGrid = rf_grid,
                     metric = "RMSE",
                     verbose = FALSE)
toc()
ggplot(rf_mod_tune)

# Optimized model
finalControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)
final_grid <- data.frame(mtry = 5,
                         splitrule = "extratrees",
                         min.node.size = 3)
tic()
rf_mod_final <- train(x = train_x,
                      y = train_y,
                      method = "ranger",
                      trControl = fitControl,
                      tuneGrid = final_grid,
                      metric = "RMSE",
                      verbose = FALSE)
toc()

# Final predictions and model performance on test dataset
rf_pred_final <- predict(rf_mod_final, newdata = test_x)
(rf_mse_final <- mean((test_y - rf_pred_final)^2))