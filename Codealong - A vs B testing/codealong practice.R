# Load/Install packages
library(tidyverse)
install.packages("lmtest")
install.packages("sandwich")
library(lmtest)
library(sandwich)
library(readr)
library(broom)

# Import data
experiment_data <- read_csv("experiment_data.csv")

# Explore data
str(experiment_data)
glimpse(experiment_data)
summary(experiment_data)

cat("Rows and unique rows in the dataset:\n")
nrow(experiment_data)
nrow(distinct(experiment_data))

cat("\nUnique/distinct months in the dataset:\n")
length(unique(experiment_data$Month))
table(experiment_data$Month)

cat("\nUnique/distinct customers in the dataset:\n")
length(unique(experiment_data$id))
nrow(distinct(experiment_data, id))

cat("\nNumber of clients by group ('New' vs 'Existing'):\n")
table(experiment_data$Group)

cat("\n")
table(experiment_data$Month, experiment_data$Treated)

# Aggregate the whole dataset by Month and Group and look at the Dollars spent with a line plot
(month_group_data <- experiment_data %>%
  group_by(Month, Group) %>%
  summarize(Dollars = mean(Dollars),
            Treated = mean(Treated)))
month_group_data %>%
  ggplot(aes(x = factor(Month), y = Dollars, color = Group, group = Group)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_vline(xintercept = 4)

# Filter out the month with mixed treatment
experiment_data_clean <- experiment_data %>%
  filter(Month != "202210") %>%
  mutate(AB_period = ifelse(Month %in% c("202207", "202208", "202209"), 0, 1))
table(experiment_data_clean$Month, experiment_data_clean$AB_period)

# Plot the Dollars spent by Group in the actual A/B time period
experiment_data_clean %>%
  filter(AB_period == 1) %>%
  ggplot(aes(x = Dollars, fill = Group)) +
  geom_density(alpha = 0.1)

# Aggregate on the customer-level that we get one row for each customer before and after seeing the "New" product
(aggregate <- experiment_data_clean %>%
  group_by(id, Treated, Group, AB_period) %>%
  summarize(Dollars = mean(Dollars)) %>%
  arrange(id, Treated, Group))

# Compare the Dollars spent between New vs. Existing Group in the actual A/B testing period
t.test(aggregate %>% filter(Group == "New" & AB_period == 1) %>% pull(Dollars),
       aggregate %>% filter(Group == "Existing" & AB_period == 1) %>% pull(Dollars))

# Compare only New before and after implementing the A/B test
t.test(aggregate %>% filter(Group == "New" & AB_period == 0) %>% pull(Dollars),
       aggregate %>% filter(Group == "New" & AB_period == 1) %>% pull(Dollars))

# Calculate the necessary sample size to get statistical significant results
(sd <- round(sd(aggregate %>% filter(Group == "New" & AB_period == 1) %>% pull(Dollars)), 1))
(power <- power.t.test(delta = 2.2, sd = sd, sig.level = 0.05, power = 0.8))

# Run a linear regression approach where we regress Treated, Group, as.factor(Month) on Dollars
lm <- lm(Dollars ~ Treated + Group + factor(Month), data = experiment_data_clean)
summary(lm)
tidy(lm)