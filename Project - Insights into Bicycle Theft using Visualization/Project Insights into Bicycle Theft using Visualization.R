## Load tidyverse package
library(tidyverse)
## Read `bike_data`
bike_data <- read_csv("Cleaned_Bicycle_Thefts_Open_Data.csv")
## Take a glance of the `bike_data`
head(bike_data)
str(bike_data)

# Reformatting data
bike_data_clean <- bike_data %>%
  mutate(quarter = quarters(quarter))

# Which quarter has the highest and lowest number of stolen bikes?
(high <- bike_data_clean %>%
  count(quarter) %>%
  slice_max(order_by = n, n = 1) %>%
  pull(quarter))

(low <- bike_data_clean %>%
    count(quarter) %>%
    slice_min(order_by = n, n = 1) %>%
    pull(quarter))

# What are the most frequent locations for bike thefts in Toronto?
bike_most_stolen <- bike_data_clean %>%
  count(location) %>%
  mutate(prop = round(n / sum(n), digits = 1)) %>%
  slice_max(order_by = prop, n = 1)
(location <- bike_most_stolen %>%
  pull(location))
(percentage <- bike_most_stolen %>%
  pull(prop))

# In which region of Toronto is the median value of stolen bikes the highest?
(region <- bike_data_clean %>%
  group_by(neighborhood) %>%
  summarize(median = median(bike_cost)) %>%
  slice_max(order_by = median, n = 1) %>%
  pull(neighborhood))

# What course of action would you recommend to the police station based on your findings?
action <- "Provide free bike locks for residential structures"