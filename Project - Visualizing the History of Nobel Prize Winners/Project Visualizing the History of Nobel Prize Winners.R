# Loading in required libraries
library(tidyverse)
library(readr)
library(ggplot2)

# Start coding here!

# Import data
nobel <- read_csv("nobel.csv")

# What is the most commonly awarded gender?
(top_gender <- nobel %>%
  count(sex) %>%
  slice_max(order_by = n, n = 1) %>%
  select(sex) %>%
  pull())

# What is the most common birth country?
(top_country <- nobel %>%
  count(birth_country) %>%
  slice_max(order_by = n, n = 1) %>%
  select(birth_country) %>%
  pull())

# What decade had the highest proportion of US-born winners?

nobel_new <- nobel %>%
  mutate(decade = floor(year/10) * 10)

(max_decade_usa <- nobel_new %>%
  select(decade, birth_country) %>%
  group_by(decade, birth_country) %>%
  summarize(n = n()) %>%
  ungroup(birth_country) %>%
  mutate(freq = n/sum(n)) %>%
  ungroup(decade) %>%
  filter(birth_country == "United States of America") %>%
  slice_max(order_by = freq, n = 1) %>%
  select(decade) %>%
  pull())

# What decade and category pair had the highest proportion of female laureates?

(max_female <- nobel_new %>%
  select(decade, category, sex) %>%
  group_by(decade, category, sex) %>%
  summarize(n = n()) %>%
  ungroup(sex) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup() %>%
  filter(sex == "Female") %>%
  slice_max(order_by = freq, n = 1))

(max_female_list <- max_female %>%
  select(decade, category) %>%
  as.list())

# Who was the first woman to receive a Nobel Prize, and in what category?

(first_woman <- nobel %>%
  select(year, category, full_name, sex) %>%
  filter(sex == "Female") %>%
  slice_min(order_by = year, n = 1))

(first_woman_name <- first_woman %>%
  select(full_name) %>%
  pull())

(first_woman_category <- first_woman %>%
  select(category) %>%
  pull())

# Which individuals or organizations have won multiple Nobel Prizes throughout the years?

(repeats <- nobel %>%
  filter(!is.na(organization_name)) %>%
  count(organization_name) %>%
  filter(n > 1) %>%
  select(organization_name) %>%
  as.list())