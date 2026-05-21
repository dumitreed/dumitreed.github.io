# Import necessary libraries
library(tidyverse)

# Start your code here!
# Use as many cells as you like
women_results <- read.csv("women_results.csv", header = TRUE)
men_results <- read.csv("men_results.csv", header = TRUE)
women_results_filtered <- women_results %>%
  mutate(date = as.Date(date)) %>%
  filter(tournament == "FIFA World Cup" & date >= "2002-01-01") %>%
  mutate(total_goals = home_score + away_score)
men_results_filtered <- men_results %>%
  mutate(date = as.Date(date)) %>%
  filter(tournament == "FIFA World Cup" & date >= "2002-01-01") %>%
  mutate(total_goals = home_score + away_score)
glimpse(women_results_filtered)
glimpse(men_results_filtered)
women_results_sample_size = nrow(women_results_filtered)
women_results_sample_size
men_results_sample_size = nrow(men_results_filtered)
men_results_sample_size
ggplot(women_results_filtered, aes(x = total_goals)) +
  geom_histogram(binwidth = 1)
ggplot(men_results_filtered, aes(x = total_goals)) +
  geom_histogram(binwidth = 1)
shapiro.test(women_results_filtered$total_goals)
shapiro.test(men_results_filtered$total_goals)
ttest <- wilcox.test(women_results_filtered$total_goals, men_results_filtered$total_goals, alternative = "greater", conf.level = 0.9)
ttest
result_df <- data.frame(p_val = ttest$p.value, result = ifelse(ttest$p.value > 0.1, "fail to reject", "reject"))
result_df