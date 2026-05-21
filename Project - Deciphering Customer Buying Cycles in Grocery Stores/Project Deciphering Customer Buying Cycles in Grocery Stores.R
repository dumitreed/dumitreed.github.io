# Import packages
library(dplyr)
library(lubridate)
library(readr)

# Start your code here

# Data import
grocery_data1 <- read_csv("grocery_data1.csv")
grocery_data2 <- read_csv("grocery_data2.csv")

# Data cleaning and merging
grocery_data1_clean <- grocery_data1 %>%
  mutate(Date = as.Date(DateRaw, format = "%B %d, %Y"),
         Week = week(Date),
         Hour = hour(Time))

grocery_data2_clean <- grocery_data2 %>%
  mutate(Date = as.Date(DateRaw, format = "%d %B %Y"),
         Week = week(Date),
         Hour = hour(Time))

grocery_all <- bind_rows(grocery_data1_clean, grocery_data2_clean, .id = "Store") %>%
  mutate(Store = as.integer(Store))

# what week of the year had the smallest absolute deviation in sales value compared to the mean weekly sales over that same time period?

(smallest_sales_deviation <- grocery_all %>%
  group_by(Week) %>%
  summarize(Sales = sum(PriceUSD * Quantity)) %>%
  ungroup() %>%
  mutate(Deviation = Sales - mean(Sales)) %>%
  slice_min(order_by = abs(Deviation), n = 1) %>%
  select(Week) %>%
  pull() %>%
  as.integer())

# What hour of the day had the most hourly total sales?
(most_hourly_sales <- grocery_all %>%
  group_by(Hour) %>%
  summarize(Sales = sum(PriceUSD * Quantity)) %>%
  ungroup() %>%
  slice_max(order_by = Sales, n = 1) %>%
  select(Hour) %>%
  pull() %>%
  as.integer())

# How many days went by between the three purchases of cornflakes by CustomerID 107?
cornflakes_days <- vector(mode = "integer",length = 2)
cornflakes_cust107 <- grocery_all %>%
  filter(CustomerID == 107, ProductName == "Cornflakes") %>%
  arrange(Date)
cornflakes_days[1] <- as.integer(cornflakes_cust107[2, "Date"] - cornflakes_cust107[1, "Date"])
cornflakes_days[2] <- as.integer(cornflakes_cust107[3, "Date"] - cornflakes_cust107[2, "Date"])
(cornflakes_days <- unlist(cornflakes_days))