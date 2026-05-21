library(dplyr)
library(tidyr)
library(ggplot2)

life_expectancy=read.csv("UNdata.csv")
head(life_expectancy)
str(life_expectancy)

# Start coding here
# Use as many cells as you need

# Does the Value column contain any missing data?
summary(life_expectancy$Value)
(missing <- ifelse(sum(is.na(life_expectancy$Value)) > 0, TRUE, FALSE))

# How does life expectancy differ between men and women across countries overall, in the 2000-2005 period?
(subgroup <- life_expectancy %>%
  filter(Year == "2000-2005") %>%
  group_by(Subgroup) %>%
  summarize(Mean = mean(Value)) %>%
  slice_max(order_by = Mean, n = 1) %>%
  select(Subgroup) %>%
  pull())

# Which countries exhibit the largest disparities in life expectancy between genders, in the 2000-2005 subgroup?
(disparities <- life_expectancy %>%
  filter(Year == "2000-2005") %>%
  group_by(Country.or.Area) %>%
  summarize(Disparity = diff(Value)) %>%
  slice_max(order_by = abs(Disparity), n = 3) %>%
  pull(Country.or.Area))
(disparities <- disparities[1:3])
  