# Load packages
library(tidyverse)
library(tidymodels)
library(lubridate)
library(devtools)
library(readr)
library(naniar)

# Part I
# Load data for height
height_df <- read_csv("height.csv")

# Explore data
str(height_df)

# Build a simple linear model
height_lm <- lm(height ~ ., data = height_df)
height_df <- height_df %>%
  mutate(predicted = predict(height_lm))
height_df %>%
  ggplot(aes(x = height, y = predicted)) +
  geom_point()

# Build a linear model with a quadratic term
height_lm2 <- lm(height ~ time + I(time ^ 2), data = height_df)
height_df2 <- height_df %>%
  mutate(predicted = predict(height_lm2))
height_df2 %>%
  ggplot(aes(x = height, y = predicted)) +
  geom_point()

# Part II
# Load data
cancelations_df <- read_csv("cancelations_live.csv")

# Explore data
str(cancelations_df)
glimpse(cancelations_df)

# Data cleaning
cancelations_df <- cancelations_df %>%
  mutate(IsCanceled = factor(IsCanceled),
         ReservedRoomType = factor(ReservedRoomType),
         AssignedRoomType = factor(AssignedRoomType),
         DepositType = factor(DepositType),
         CustomerType = factor(CustomerType))
summary(cancelations_df)

# Split data
set.seed(123)
split <- initial_split(cancelations_df, prop = 0.8, strata = IsCanceled)
train_df <- training(split)
test_df <- testing(split)
summary(train_df$IsCanceled) %>% prop.table()
summary(test_df$IsCanceled) %>% prop.table()

# Set up logistic regression model
logistic_reg <- logistic_reg()

# Set up the recipe
lr_recipe <- recipe(IsCanceled ~ ., data = train_df) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_date(arrival_date, features = c("dow", "week", "month")) %>%
  step_holiday(arrival_date, holidays = c("ChristmasEve", "Easter", "NewYearsDay", "USChristmasDay", "USMemorialDay", "USIndependenceDay", "USLaborDay", "USThanksgivingDay"), keep_original_cols =  FALSE) %>%
  step_dummy(all_nominal_predictors())
lr_recipe

# Build a workflow
lr_workflow <- workflow() %>%
  add_recipe(lr_recipe) %>%
  add_model(logistic_reg)
lr_workflow

# Fit the workflow
lr_fit <- lr_workflow %>%
  fit(data = train_df)
tidy(lr_fit) %>% arrange(p.value) %>% print(n = 55)

# Model performance
lr_aug <- lr_fit %>%
  augment(new_data = test_df)
lr_aug
lr_accuracy <- lr_aug %>%
  accuracy(truth = IsCanceled, estimate = .pred_class)
lr_accuracy

# Part III
# Load data
loan_data_df <- read_csv("Loan_Data.csv")

# Explore data
str(loan_data_df)
glimpse(loan_data_df)
summary(loan_data_df)

# Clean data
loan_data_df <- loan_data_df %>%
  mutate(across(where(is.character), factor))
loan_data_df %>%
  is.na() %>%
  table()
loan_data_df %>%
  is.na() %>%
  as_tibble() %>%
  summarize(across(everything(), mean)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")
vis_miss(loan_data_df)
loan_data_df %>%
  select(Gender, Married, Dependents, Self_Employed, LoanAmount, Loan_Amount_Term, Credit_History) %>%
  vis_miss()

# Split data
set.seed(111)
split <- initial_split(loan_data_df, prop = 0.8, strata = Loan_Status)
loan_train <- training(split)
loan_test <- testing(split)

# Set up logistic regression model
logistic_reg <- logistic_reg()

# Set up the recipe
loan_recipe <- recipe(Loan_Status ~ ., data = loan_train) %>%
  update_role(Loan_ID, new_role = "ID") %>%
  step_impute_knn(all_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Build a workflow
loan_workflow <- workflow() %>%
  add_recipe(loan_recipe) %>%
  add_model(logistic_reg)
loan_workflow

# Fit the workflow
loan_fit <- loan_workflow %>%
  fit(data = loan_train)
tidy(loan_fit) %>% arrange(p.value)

# Model performance
loan_aug <- loan_fit %>%
  augment(new_data = loan_test)
loan_aug
loan_accuracy <- loan_aug %>%
  accuracy(truth = Loan_Status, estimate = .pred_class)
loan_accuracy

