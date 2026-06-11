# Load necessary packages
library(readr)
library(dplyr)
options(dplyr.summarise.inform = FALSE)
library(lubridate)
library(forecast)
library(magrittr)

# Load the data
beans <- read.csv("data/Beans (dry).csv")
cassava <- read.csv("data/Cassava.csv")
chili <- read.csv("data/Chili (red).csv")
maize <- read.csv("data/Maize.csv")
oranges <- read.csv("data/Oranges (big size).csv")
peas <- read.csv("data/Peas (fresh).csv")
potatoes <- read.csv("data/Potatoes (Irish).csv")
sorghum <- read.csv("data/Sorghum.csv")
tomatoes <- read.csv("data/Tomatoes.csv")

# Start coding here
# Use as many cells as you like
get_median_price_by_date <- function(file_path) {
  df <- read.csv(file_path)
  df <- df %>%
    mutate(mp_date = ymd(paste(mp_year, mp_month, "01"))) %>%
    group_by(mp_date) %>%
    summarize(mp_price_median = median(mp_price))
  return(df)
}
get_median_price_by_date("data/Beans (dry).csv")

forecast_price <- function(data) {
  df <- data
  start_year <- year(df$mp_date[1])
  start_month <- month(df$mp_date[1])
  end_year <- year(df$mp_date[nrow(df)])
  end_month <- month(df$mp_date[nrow(df)])
  data_ts <- ts(df$mp_price_median,
                start = c(start_year, start_month),
                end = c(end_year, end_month),
                frequency = 12)
  data_forecast <- forecast(data_ts)
  return(data_forecast)
}
forecast_price(get_median_price_by_date("data/Beans (dry).csv"))
forecast_price(get_median_price_by_date("data/Cassava.csv"))
cassava_feb2017 <- 225