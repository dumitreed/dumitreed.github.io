# TASK 1
# Import libraries
library(readr)
library(tidyverse)

# Import data
house_sales <- read_csv("house_sales.csv")

# Data exploration
head(house_sales)
str(house_sales)
glimpse(house_sales)
summary(house_sales)

# Data cleaning
house_sales <- house_sales %>%
  mutate(city = ifelse(city %in% c("Poppleton", "Riverford", "Silvertown", "Teasdale"), city, "Unknown"))
house_sales <- house_sales %>%
  mutate(city = factor(city))

# Post-cleaning summary
summary(house_sales$city)

# Calculate the number of missing values of the city
missing_city <- house_sales %>%
  filter(city == "Unknown") %>%
  count(city) %>%
  pull()
missing_city

# TASK 2
# Import libraries
library(readr)
library(tidyverse)
library(stringr)
library(forcats)

# Import data
house_sales <- read_csv("house_sales.csv")

# Data exploration
str(house_sales)
summary(house_sales)

# Data cleaning
clean_data <- house_sales %>%
  mutate(city = ifelse(city %in% c("Poppleton", "Riverford", "Silvertown", "Teasdale"), city, "Unknown"),
         months_listed = ifelse(is.na(months_listed), round(mean(months_listed, na.rm = TRUE), digits = 1), months_listed),
         house_type = ifelse(house_type == "Det.", "Detached", house_type),
         house_type = ifelse(house_type == "Semi", "Semi-detached", house_type),
         house_type = ifelse(house_type == "Terr.", "Terraced", house_type),
         area = round(as.double(str_replace(area, " sq.m.", "")), digits = 1))
  
# Create a cleaned version of the dataframe
clean_data

#TASK 3
# Import libraries
library(readr)
library(tidyverse)

# Import data
house_sales <- read_csv("house_sales.csv")

# Data exploration
str(house_sales)
summary(house_sales)

# A table showing the difference in the average sale price by number of bedrooms along with the variance
price_by_rooms <- house_sales %>%
  group_by(bedrooms) %>%
  summarize(avg_price = round(mean(sale_price), digits = 1),
            var_price = round(var(sale_price), digits = 1))
price_by_rooms

#TASK 4
# Import libraries
library(readr)
library(tidyverse)
library(caret)

# Import data
train <- read_csv("train.csv")
validation <- read_csv("validation.csv")

# Data exploration
str(train)
str(validation)
summary(train)
summary(validation)

# Data cleaning
train <- train %>%
  mutate(city = factor(city),
         house_type = factor(house_type))
validation <- validation %>%
  mutate(city = factor(city),
         house_type = factor(house_type))

# Data preparation for training
dummies <- dummyVars(~ city + house_type, data = train)
train_dummy <- predict(dummies, newdata = train)
train_set <- train %>%
  bind_cols(train_dummy) %>%
  select(-house_id, -city, -house_type, -sale_price)
str(train_set)
train_sale_price <- train %>%
  select(sale_price)
str(train_sale_price)
validation_dummy <- predict(dummies, newdata = validation)
validation_set <- validation %>%
  bind_cols(validation_dummy) %>%
  select(-house_id, -city, -house_type)
str(validation_set)

# Base model fitting
set.seed(13)
glm_mod <- train(x = train_set,
                 y = train_sale_price$sale_price,
                 method = "glm",
                 metric = "RMSE")
glm_mod
validation_set$price <- predict(glm_mod, newdata = validation_set)
base_result <- validation %>%
  bind_cols(validation_set) %>%
  select(house_id, price)
base_result

#TASK 5
# Import libraries
library(readr)
library(tidyverse)
library(caret)

# Import data
train <- read_csv("train.csv")
validation <- read_csv("validation.csv")

# Data exploration
str(train)
str(validation)
summary(train)
summary(validation)

# Data cleaning
train <- train %>%
  mutate(city = factor(city),
         house_type = factor(house_type))
validation <- validation %>%
  mutate(city = factor(city),
         house_type = factor(house_type))

# Data preparation for training
dummies <- dummyVars(~ city + house_type, data = train)
train_dummy <- predict(dummies, newdata = train)
train_set <- train %>%
  bind_cols(train_dummy) %>%
  select(-house_id, -city, -house_type, -sale_price)
str(train_set)
train_sale_price <- train %>%
  select(sale_price)
str(train_sale_price)
validation_dummy <- predict(dummies, newdata = validation)
validation_set <- validation %>%
  bind_cols(validation_dummy) %>%
  select(-house_id, -city, -house_type)
str(validation_set)

# Comparison model fitting
set.seed(13)
rf_mod <- train(x = train_set,
                y = train_sale_price$sale_price,
                method = "ranger",
                metric = "RMSE",
                verbose = FALSE)
rf_mod
validation_set$price <- predict(rf_mod, newdata = validation_set)
compare_result <- validation %>%
  bind_cols(validation_set) %>%
  select(house_id, price)
compare_result
