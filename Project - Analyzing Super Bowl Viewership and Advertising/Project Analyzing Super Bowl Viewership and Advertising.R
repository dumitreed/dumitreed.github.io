# Load packages
library(tidyverse)
library(gridExtra)

# Load the CSV data
tv  <-  read_csv("tv.csv", show_col_types=FALSE)
super_bowls  <-  read_csv("super_bowls.csv", show_col_types=FALSE)

# Data cleaning. Remove unused columns. Join tables.
tv_clean <- tv  %>%
  select(super_bowl, avg_us_viewers, share_household, rating_household, ad_cost)
super_bowls_clean <- super_bowls %>%
  select(super_bowl, difference_pts)
tv_super_bowls <- tv_clean %>%
  inner_join(super_bowls_clean, by = "super_bowl")

# Do large point differences result in lost viewers across super bowl games? Visualize and interpret the data.
g1 <- tv_super_bowls %>%
  ggplot(aes(x = super_bowl, y = difference_pts)) +
  geom_line() +
  theme_bw()
g2 <- tv_super_bowls %>%
  ggplot(aes(x = super_bowl, y = avg_us_viewers)) +
  geom_line() +
  theme_bw()
grid.arrange(g1, g2, nrow = 2)
(corr_viewers_diff_pts <- cor(tv_super_bowls$difference_pts, tv_super_bowls$avg_us_viewers))

tv_super_bowls <- tv_super_bowls %>%
  mutate(avg_us_viewers_change = avg_us_viewers - lag(avg_us_viewers))
g3 <- tv_super_bowls %>%
  ggplot(aes(x = super_bowl, y = avg_us_viewers_change)) +
  geom_line() +
  theme_bw()
grid.arrange(g1, g3, nrow = 2)
difference_pts <- as.vector(tv_super_bowls$difference_pts)
difference_pts <- difference_pts[2:59]
avg_us_viewers_change <- as.vector(tv_super_bowls$avg_us_viewers_change)
avg_us_viewers_change <- avg_us_viewers_change[2:59]
(corr_viewers_diff_pts2 <- cor.test(difference_pts, avg_us_viewers_change))

(score_impact <- "weak")

# How has the number of viewers and TV ratings trended alongside advertisement costs? Which one increases first? Visualize and interpret
g4 <- tv_super_bowls %>%
  ggplot(aes(x = super_bowl, y = rating_household)) +
  geom_line() +
  theme_bw()
g5 <- tv_super_bowls %>%
  ggplot(aes(x = super_bowl, y = ad_cost)) +
  geom_line() +
  theme_bw()
grid.arrange(g2, g4, g5, nrow = 3)

(first_to_increase <- "ratings")