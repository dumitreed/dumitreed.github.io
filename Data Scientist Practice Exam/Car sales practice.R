# Load packages
library(tidyverse)
library(readr)
library(tidymodels)

# Import data
toyota <- read_csv("toyota.csv")
str(toyota)
glimpse(toyota)
summary(toyota)

# Data cleaning
toyota_clean <- toyota %>%
  mutate(across(where(is.character), factor))
summary(toyota_clean)

# Data exploration
# Eliminating cars from years with only one
toyota_clean %>% count(year)
toyota_clean %>%
  filter(year %in% c(1998, 1999, 2000, 2001))
toyota_clean <- toyota_clean %>%
  filter(year > 2001)

# Exploring cars with unusually low prices
toyota_clean %>%
  ggplot(aes(x = price)) +
  geom_histogram() +
  xlim(0, 10000)
toyota_clean %>%
  filter(price < 2000) %>%
  arrange(price) %>%
  print(n = 19)

# Exploring extreme values in the mpg variable and removing low value extremes
toyota_clean %>%
  ggplot(aes(y = mpg)) +
  geom_boxplot()
toyota_clean %>%
  filter(mpg > 100 | mpg < 25) %>%
  arrange(desc(mpg)) %>%
  print(n = 41)
toyota_clean <- toyota_clean %>%
  filter (mpg > 25)

# Exploring mileage extreme values and removing values corresponding to older years
toyota_clean %>%
  ggplot(aes(x = mileage)) +
  geom_histogram()
summary(toyota_clean$mileage)
toyota_remove <- toyota_clean %>%
  filter(mileage < 1000, year < 2019) %>%
  arrange(year)
toyota_clean <- toyota_clean %>%
  setdiff(toyota_remove)

# Exploring price vs year
toyota_clean %>%
  ggplot(aes(x = year, y = price)) +
  geom_point()

# Exploring price vs transmission
toyota_clean %>%
  ggplot(aes(x = transmission, y = price)) +
  geom_boxplot()
summary(toyota_clean$transmission)

# Removing single "other" value for transmission
toyota_clean <- toyota_clean %>%
  filter(transmission != "Other")

# Exploring price by fuel type
toyota_clean %>%
  ggplot(aes(x = fuelType, y = price)) +
  geom_boxplot()
summary(toyota_clean$fuelType)

# Interaction between model and year
toyota_clean %>%
  group_by(year, model) %>%
  summarize (avg_price = mean(price)) %>%
  ggplot(aes(x = year, y = avg_price, color = model)) +
  geom_line()

# Preparing data for modeling
set.seed(111)
split <- initial_split(data = toyota_clean, prop = 0.75)
train <- training(split)
test <- training(split)

# Base model
base_recipe <- recipe(price ~ ., data = train) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_factor_predictors())
base_model <- linear_reg(mode = "regression")
base_workflow <- workflow() %>%
  add_recipe(base_recipe) %>%
  add_model(base_model)
base_fit <- base_workflow %>%
  fit(data = train)
base_fit
base_pred <- base_fit %>%
  predict(test)
base_mape <- mape_vec(truth = test$price, estimate = base_pred$.pred)
test %>%
  bind_cols(base_pred) %>%
  ggplot(aes(x = price, y = .pred)) +
  geom_point() +
  geom_abline(a = 1, b = 0)

# Comparison model
comp_recipe <- recipe(price ~ ., data = train) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_factor_predictors())
comp_model <- rand_forest(mode = "regression")
comp_workflow <- workflow() %>%
  add_recipe(comp_recipe) %>%
  add_model(comp_model)
comp_fit <- comp_workflow %>%
  fit(data = train)
comp_pred <- comp_fit %>%
  predict(test)
comp_mape <- mape_vec(truth = test$price, estimate = comp_pred$.pred)
test %>%
  bind_cols(comp_pred) %>%
  ggplot(aes(x = price, y = .pred)) +
  geom_point() +
  geom_abline()
test_comp <- test %>%
  bind_cols(comp_pred) %>%
  mutate(error = abs(price - .pred),
         error_pct = error / price * 100) %>%
  arrange(desc(error_pct))