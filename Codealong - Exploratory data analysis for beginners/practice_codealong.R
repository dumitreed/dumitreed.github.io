# Load packages
library(tidyverse)
library(readr)
library(ggplot2)
library(lubridate)

# Import data
user_page_view_history <- read_csv("user_page_view_history.csv")

# Explore data
str(user_page_view_history)
glimpse(user_page_view_history)
summary(user_page_view_history$user_flow)

# Cleaning data
(user_page_view_history <- user_page_view_history %>%
  select(user_flow, date_visited, current_page_url, referral_page_url) %>%
  mutate(user_flow = factor(user_flow)))

# What pages do users enter DataCamp from?
user_page_view_history %>%
  count(referral_page_url)

# What pages do users visit after they land? How many users entered through each page?
user_page_view_history %>%
  count(current_page_url)

# What paths do users take through onboarding?
user_page_view_history %>%
  count(user_flow) %>%
  arrange(desc(n))

# Graph the number of visits over time
user_page_view_history %>%
  count(date_visited) %>%
  ggplot(aes(x = date_visited, y = n)) +
  geom_line()

# Quantify drop in visits after 2022-07-01
(drop <- user_page_view_history %>%
  count(date_visited) %>%
  summarize(old_avg = mean(n[date_visited < "2022-07-01"]),
            new_avg = mean(n[date_visited >= "2022-07-01"])))
(drop_pct <- (drop$old_avg - drop$new_avg) / drop$old_avg)

# How has the landing page performed over time?
user_page_view_history %>%
  count(user_flow, date_visited) %>%
  ggplot(aes(x = date_visited, y = n, color = user_flow)) +
  geom_line()

user_page_view_history %>%
  filter(date_visited < "2023-01-01") %>%
  mutate(week = floor_date(date_visited, unit = "week")) %>%
  count(user_flow, week) %>%
  ggplot(aes(x = week, y = n, color = user_flow)) +
  geom_line()

# Calculate the average page views before and after the first of August!
(blogToSignup_drop <- user_page_view_history %>%
  filter(user_flow == "blogToSignup") %>%
  count(date_visited) %>%
  summarize(old_avg = mean(n[date_visited < "2022-08-01"]),
            new_avg = mean(n[date_visited >= "2022-08-01"])))
(blogToSignup_pct_drop <- (blogToSignup_drop$old_avg - blogToSignup_drop$new_avg) / blogToSignup_drop$old_avg)
  