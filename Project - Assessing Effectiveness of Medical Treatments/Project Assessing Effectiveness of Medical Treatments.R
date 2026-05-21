# Load the necessary packages
library(readr)
library(dplyr)
library(ggplot2)
library(broom)

# Load the data
data <- read_csv("kidney_stone_data.csv")

# Inspect the first five rows
head(data, 5)

# Start coding here...add as many cells as you like!
# Summary table
(data_summary <- data %>%
  group_by(treatment, stone_size, success) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N)))

# Identify confounding variable
chisq <- chisq.test(data$treatment, data$stone_size)
tidy(chisq)

# Logistic regression
logistic_model <- glm(success ~ treatment + stone_size, data = data, family = "binomial")
(logistic_model_tidy <- tidy(logistic_model))

# Get answers
# After controlling for the treatment effect, are smaller stones more likely to result in a successful treatment?
(small_high_success <- as.character(ifelse(logistic_model_tidy[3, 5] < 0.05, "Yes", "No")))

# Is Treatment A significantly more effective than Treatment B?
(A_B_sig <- as.character(ifelse(logistic_model_tidy[2, 5] < 0.05, "Yes", "No")))