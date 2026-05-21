# Load necessary libraries
suppressPackageStartupMessages(library(dplyr))
library(lubridate) 
library(ranger)    
library(xgboost)   
library(ggplot2)
library(caret)
library(plyr)
library(mboost)
library(LiblineaR)
library(e1071)
library(h2o)
library(gbm)

# Load and inspect the training and testing datasets
df_train <- read.csv("df_train.csv")
df_test <- read.csv("df_test.csv")

## Explore the structure of the dataset
glimpse(df_train)
summary(df_train)

# Start coding here...add as many cells as you like!

# Data cleaning
df_train <- df_train %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         day_in_week = as.factor(day_in_week))
df_test <- df_test %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         day_in_week = as.factor(day_in_week))

# One hot encoding of day_in_week
dummies_train <- dummyVars(~ day_in_week, data = df_train)
df_train_onehot <- predict(dummies_train, newdata = df_train)
df_train_ready <- bind_cols(df_train, df_train_onehot) %>% select(-day_in_week)
dummies_test <- dummyVars(~ day_in_week, data = df_test)
df_test_onehot <- predict(dummies_test, newdata = df_test)
df_test_ready <- bind_cols(df_test, df_test_onehot) %>% select(-day_in_week)

# Preparing data for training and model assessment
train_x <- df_train_ready %>% select(-date, -power_consumption)
train_y <- df_train_ready %>% select(power_consumption)
test_x <- df_test_ready %>% select(-date, -power_consumption)
test_y <- df_test_ready %>% select(power_consumption)

# Generalized linear model with caret
glm_model <- train(x = train_x,
                    y = train_y$power_consumption,
                    method = "glm")
predict_y <- predict(glm_model, newdata = test_x)
(glm_rmse <- sqrt(mean((test_y$power_consumption - predict_y)^2)))

# Boosted generalized linear model with caret
bglm_model <- train(x = train_x,
                   y = train_y$power_consumption,
                   method = "glmboost")
predict_y <- predict(bglm_model, newdata = test_x)
(bglm_rmse <- sqrt(mean((test_y$power_consumption - predict_y)^2)))

# Regularized Support Vector Machine (dual) with Linear Kernel with caret
rsvm_model <- train(x = train_x,
                    y = train_y$power_consumption,
                    method = "svmLinear3")
predict_y <- predict(rsvm_model, newdata = test_x)
(rsvm_rmse <- sqrt(mean((test_y$power_consumption - predict_y)^2)))

# Random forest with caret
rf_model <- train(x = train_x,
                  y = train_y$power_consumption,
                  method = "ranger")
predict_y <- predict(rf_model, newdata = test_x)
(rf_rmse <- sqrt(mean((test_y$power_consumption - predict_y)^2)))

# Stochastic gradient boosting with caret
gbm_model <- train(x = train_x,
               y = train_y$power_consumption,
               method = "gbm",
               metric = "RMSE",
               verbose = FALSE)
predict_y <- predict(gbm_model, newdata = test_x)
(gbm_rmse <- sqrt(mean((test_y$power_consumption - predict_y)^2)))

# Gradient boosting machines with caret and h2o
gbmh2o_model <- train(x = train_x,
                      y = train_y$power_consumption,
                      model = "gbm_h2o")
predict_y <- predict(gbmh2o_model, newdata = test_x)
(gbmh2o_rmse <- sqrt(mean((test_y$power_consumption - predict_y)^2)))

# eXtreme gradient boosting with caret
xgb_model <- train(x = train_x,
                   y = train_y$power_consumption,
                   model = "xgbTree")
predict_y <- predict(xgb_model, newdata = test_x)
(xgb_rmse <- sqrt(mean((test_y$power_consumption - predict_y)^2)))

# eXtreme gradient boosting with caret
xgb2_model <- train(x = train_x,
                   y = train_y$power_consumption,
                   model = "xgbDART")
predict_y <- predict(xgb2_model, newdata = test_x)
(xgb_rmse <- sqrt(mean((test_y$power_consumption - predict_y)^2)))

# eXtreme gradient boosting linear with caret
xgb3_model <- train(x = train_x,
                    y = train_y$power_consumption,
                    model = "xgbLinear")
predict_y <- predict(xgb3_model, newdata = test_x)
(xgb_rmse <- sqrt(mean((test_y$power_consumption - predict_y)^2)))

# Optimization of stochastic gradient boosting with caret
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 10)
system.time(gbm_model1 <- train(x = train_x,
                   y = train_y$power_consumption,
                   method = "gbm",
                   trControl = ctrl,
                   metric = "RMSE",
                   verbose = FALSE))
predict_y <- predict(gbm_model1, newdata = test_x)
(gbm_rmse1 <- sqrt(mean((test_y$power_consumption - predict_y)^2)))

gbmGrid <-  expand.grid(interaction.depth = c(1, 6, 10), 
                        n.trees = c(100, 500, 1000), 
                        shrinkage = 0.01,
                        n.minobsinnode = 10)
system.time(gbm_model2 <- train(x = train_x,
                    y = train_y$power_consumption,
                    method = "gbm",
                    trControl = ctrl,
                    tuneGrid = gbmGrid,
                    metric = "RMSE",
                    verbose = FALSE))
predict_y <- predict(gbm_model2, newdata = test_x)
(gbm_rmse2 <- sqrt(mean((test_y$power_consumption - predict_y)^2)))

system.time(gbm_model3 <- train(x = train_x,
                                y = train_y$power_consumption,
                                method = "gbm",
                                trControl = ctrl,
                                preProcess = c("nzv", "pca"),
                                metric = "RMSE",
                                verbose = FALSE))
predict_y <- predict(gbm_model3, newdata = test_x)
(gbm_rmse3 <- sqrt(mean((test_y$power_consumption - predict_y)^2)))

# Save the lowest RMSE achieved
selected_rmse <- gbm_rmse
  
# Plot the power_consumption predictions and actual daily power_consumption
predict_y_final <- predict(gbm_model, newdata = test_x)
df_test_ready <- df_test_ready %>% 
  mutate(power_consumption_pred = predict_y_final)
df_test_ready %>%
  ggplot(aes(x = power_consumption, y = power_consumption_pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  ylim(0, 3000) +
  xlim(0, 3000)
trend_similarity <- "yes"
