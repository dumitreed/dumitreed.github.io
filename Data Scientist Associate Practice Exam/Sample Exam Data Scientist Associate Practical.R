# TASK 1
# Load packages
library(readr)
library(tidyverse)
library(forcats)

# Import data
loyalty_df <- read_csv("loyalty.csv")

# Data exploration
str(loyalty_df)
head(loyalty_df)
glimpse(loyalty_df)
summary(loyalty_df)

# Data cleaning
clean_data <- loyalty_df %>%
  mutate(spend = round(spend, digits = 2),
         first_month = round(as.double(first_month), digits = 2),
         first_month = replace_na(first_month, 0),
         intems_in_first_month = as.integer(items_in_first_month),
         region = as.factor(region),
         loyalty_years = factor(loyalty_years, levels =  c("0-1", "1-3", "3-5", "5-10", "10+")),
         joining_month = replace_na(joining_month, "Unknown"),
         joining_month = as.factor(joining_month),
         promotion = as.factor(promotion),
         promotion = fct_recode(promotion, No = "NO", Yes = "YES"))

# Post-cleaning exploration
summary(clean_data)
str(clean_data)
summary(clean_data$first_month)
summary(clean_data$loyalty_years)
levels(clean_data$loyalty_years)
summary(clean_data$joining_month)
summary(clean_data$promotion)

# TASK 2
# Load packages
library(readr)
library(tidyverse)
library(forcats)

# Import data
loyalty_df <- read_csv("loyalty.csv")

# Data exploration
str(loyalty_df)
head(loyalty_df)
glimpse(loyalty_df)
summary(loyalty_df)

# Data summary
spend_by_years <- loyalty_df %>%
  mutate(loyalty_years = factor(loyalty_years, levels =  c("0-1", "1-3", "3-5", "5-10", "10+"))) %>%
  select(loyalty_years, spend) %>%
  group_by(loyalty_years) %>%
  summarize(avg_spend = round(mean(spend), digits = 2),
            var_spend = round(var(spend), digits = 2))
spend_by_years

# TASK 3
# Load packages
library(readr)
library(tidyverse)
library(forcats)
library(caret)

# Import data
train_df <- read_csv("train.csv")
test_df <- read_csv("test.csv")

# Data exploration
str(train_df)
str(test_df)
summary(train_df)
summary(test_df)

# Data cleaning
train_clean <- train_df %>%
  mutate(region = as.factor(region),
         loyalty_years = as.factor(loyalty_years),
         joining_month = as.factor(joining_month),
         promotion = as.factor(promotion))
test_clean <- test_df %>%
  mutate(region = as.factor(region),
         loyalty_years = as.factor(loyalty_years),
         joining_month = as.factor(joining_month),
         promotion = as.factor(promotion))

# Post-cleaning exploration
str(train_clean)
str(test_clean)
summary(train_clean)
summary(test_clean)
summary(train_clean$joining_month)

# Data prep for modelling
train_x <- train_clean %>% select(-customer_id, -spend)
train_y <- train_clean$spend
test_x <- test_clean %>% select(-customer_id)

# Generalized linear model
glm_mod <- train(x = train_x,
                    y = train_y,
                    method = "glm",
                    metric = "RMSE")
glm_mod
test_clean$spend <- predict(glm_mod, newdata = test_x)
base_result <- test_clean %>% select(customer_id, spend)
base_result

# TASK4
# Load packages
library(readr)
library(tidyverse)
library(forcats)
library(caret)

# Import data
train_df <- read_csv("train.csv")
test_df <- read_csv("test.csv")

# Data exploration
str(train_df)
str(test_df)
summary(train_df)
summary(test_df)

# Data cleaning
train_clean <- train_df %>%
  mutate(region = as.factor(region),
         loyalty_years = as.factor(loyalty_years),
         joining_month = as.factor(joining_month),
         promotion = as.factor(promotion))
test_clean <- test_df %>%
  mutate(region = as.factor(region),
         loyalty_years = as.factor(loyalty_years),
         joining_month = as.factor(joining_month),
         promotion = as.factor(promotion))

# Post-cleaning exploration
str(train_clean)
str(test_clean)
summary(train_clean)
summary(test_clean)
summary(train_clean$joining_month)

# One hot encoding of all factor variables
dummies <- dummyVars(~ region + loyalty_years + joining_month + promotion, data = train_clean)
train_x_dummy <- predict(dummies, newdata = train_clean)
train_clean <- train_clean %>%
  bind_cols(train_x_dummy) %>%
  select(-region, -loyalty_years, -joining_month, -promotion)
test_x_dummy <- predict(dummies, newdata = test_clean)
test_clean <- test_clean %>%
  bind_cols(test_x_dummy) %>%
  select(-region, -loyalty_years, -joining_month, -promotion)

# Data prep for modelling
train_x <- train_clean %>% select(-customer_id, -spend)
train_y <- train_clean$spend
test_x <- test_clean %>% select(-customer_id)

# Random forest model
set.seed(13)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 10)
tune_grid <- expand.grid(mtry = 15,
                        splitrule = "extratrees",
                        min.node.size = 17)
rf_mod <- train(x = train_x,
                y = train_y,
                method = "ranger",
                trControl = ctrl,
                tuneGrid = tune_grid,
                metric = "RMSE",
                verbose = FALSE)
test_clean$spend <- predict(rf_mod, newdata = test_x)
compare_result <- test_clean %>% select(customer_id, spend)