# Load libraries
library(readr)
library(tidyverse)
library(forcats)
library(ggplot2)

# Optimize plot area
# Set the default figure font size to 20 (and use the gray theme for plot colors)
theme_set(theme_gray(20))

# Display plots in the workspace with a width of 10 inches and a height of 7 inches
opts <- options(repr.plot.width = 10, repr.plot.height = 8)

# Import data
vgsales <- read_csv("vgsales.csv")

# Explore data
str(vgsales)
glimpse(vgsales)

# What are the top selling video games in the dataset?
vgsales %>%
  group_by(Name) %>%
  summarize(Total_Global_Sales = sum(Global_Sales)) %>%
  slice_max(order_by = Total_Global_Sales, n = 10) %>%
  arrange(desc(Total_Global_Sales)) %>%
  ggplot(aes(x = reorder(Name, Total_Global_Sales), y = Total_Global_Sales)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 Selling Video Games",
       y = "Total Global Sales",
       x = "Video Game")

# What are the total yearly sales of the 7th gen games included in the dataset?
seventh_generation <- vgsales %>%
  filter(Platform_Generation == "7th")

(total_7th_gen_global_sales_by_year <- seventh_generation %>%
  group_by(Year) %>%
  summarize(Total_Global_Sales = sum(Global_Sales)) %>%
  arrange(desc(Total_Global_Sales)))

total_7th_gen_global_sales_by_year %>%
  ggplot(aes(x = Year, y = Total_Global_Sales)) +
  geom_line(linewidth = 2) +
  labs(title = "7th Generation Total Global Sales by Year",
       y = "Total Global Sales")

# What's the split of those games by platform?
(total_7th_gen_global_sales_by_year_platform <- seventh_generation %>%
  group_by(Year, Platform) %>%
  summarize(Total_Global_Sales = sum(Global_Sales)) %>%
  arrange(desc(Total_Global_Sales)))

total_7th_gen_global_sales_by_year_platform %>%
  ggplot(aes(x = Year, y = Total_Global_Sales, color = Platform)) +
  geom_line(linewidth = 2) +
  labs(title = "7th Generation Total Global Sales by Year and Platform",
       y = "Total Global Sales")

# How can we visualize all generations together?
(total_global_sales_by_year_platform <- vgsales %>%
    group_by(Year, Platform) %>%
    summarize(Total_Global_Sales = sum(Global_Sales)) %>%
    arrange(desc(Total_Global_Sales)))

total_global_sales_by_year_platform %>%
  ggplot(aes(x = Year, y = Total_Global_Sales, color = Platform)) +
  geom_line(linewidth = 2) +
  labs(title = "Total Global Sales by Year and Platform",
       y = "Total Global Sales")

vgsales %>%
  group_by(Year, Platform_Generation, Platform_Company) %>%
  summarize(Total_Global_Sales = sum(Global_Sales)) %>%
  ungroup() %>%
  ggplot(aes(x = Year, y = Total_Global_Sales, color = Platform_Company)) +
  geom_line(linewidth = 2) +
  facet_wrap(~ Platform_Generation, ncol = 1)
