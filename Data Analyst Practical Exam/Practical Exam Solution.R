# Load packages
library(readr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(forcats)

# Import data
account_info <- read_csv("da_fitly_account_info.csv")
customer_support <- read_csv("da_fitly_customer_support.csv")
user_activity <- read_csv("da_fitly_user_activity.csv")

# Initial exploratory
str(account_info)
str(customer_support)
str(user_activity)

# Cleaning datasets
# account_info
account_info_clean <- account_info %>%
  mutate(customer_id = as.numeric(str_replace_all(customer_id, "[:alpha:]", "")), # removed any letters from customer_id
         state = factor(state), # factorized all features that are categories except churn_status
         plan = factor(plan, levels = c("Free", "Basic", "Pro", "Enterprise")))
summary(account_info_clean %>% mutate(churn_status = factor(churn_status)))
summary(account_info_clean$state) # validating the state options for errors
account_info_clean$churn_status <- account_info_clean$churn_status %>%
  replace_na("N") %>%
  factor() # replace NAs with "N" and factorize

# customer_support
summary(account_info_clean$churn_status)
customer_support_clean <- customer_support %>%
  mutate(channel = factor(str_replace(channel, "-", "unknown")), #replace missing channel values with "unknown"
         topic = factor(topic)) # factorize all features that are categories
summary(customer_support_clean)
customer_support_clean %>% # checking for duplicate tickets
  count(ticket_time, user_id) %>%
  arrange(desc(n))

# user_activity
user_activity_clean <- user_activity %>%
  mutate(event_type = factor(event_type))
summary(user_activity_clean)

# Data exploration for churn patterns
# Plan type analysis
account_info_clean %>%
  ggplot(aes(x = plan)) +
  geom_bar() +
  labs(x = "Plan Type",
       y = "Number of Customers",
       title = "Number of Customers Across Different Plan Tiers") +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, 20))
account_info_summary_metrics <- account_info_clean %>%
  group_by(plan) %>%
  summarize(total_customers = n(),
            mean_price = mean(plan_list_price),
            med_price = median(plan_list_price),
            min_price = min(plan_list_price),
            max_price = max(plan_list_price),
            mean_churn = mean(churn_status == "Y")) %>%
  arrange(mean_price)
account_info_summary_metrics
account_info_summary_metrics %>%
  mutate(mean_churn_pct = mean_churn * 100) %>%
  ggplot(aes(x = plan, y = mean_churn_pct)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = mean(account_info_clean$churn_status == "Y") * 100) +
  annotate("text", x = 4.55, y = 33, label = paste0("Mean churn: ", mean(account_info_clean$churn_status == "Y") * 100, "%")) +
  labs(title = "Churn % by Plan Tier",
       y = "Churn (%)",
       x = "Plan Tiers")
account_info_clean %>%
  mutate(paid_plan = ifelse(plan == "Free", "No", "Yes")) %>%
  group_by(paid_plan) %>%
  summarize(mean_churn_pct = mean(churn_status == "Y")* 100)

account_info_clean %>%
  filter(plan != "Free") %>%
  group_by(plan, churn_status) %>%
  summarize(mean_price = mean(plan_list_price),
            med_price = median(plan_list_price),
            min_price = min(plan_list_price),
            max_price = max(plan_list_price))
account_info_clean %>%
  filter(plan != "Free") %>%
  ggplot(aes(x = plan, y = plan_list_price)) +
  geom_boxplot() +
  facet_wrap(~ churn_status, labeller = as_labeller(c('N' = "No Churn", 'Y' = "Churn"))) +
  labs(title = "Plan Price Distribution by Plan Type and Churn Status",
       x = "Plan Tiers",
       y = "Plan Price")

# Support activity analysis
customer_support_clean %>%
  ggplot(aes(x = resolution_time_hours)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of Resolution Times for All Submitted Tickets",
       x = "Resolution Time (Hours)",
       y = "Tickets Count") +
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  scale_x_continuous(breaks = seq(0, 35, 5))
customer_support_join <- customer_support_clean %>%
  group_by(user_id) %>%
  summarize(count = n(),
            mean_resolution = mean(resolution_time_hours),
            median_resolution = median(resolution_time_hours),
            min_resolution = min(resolution_time_hours),
            max_resolution = max(resolution_time_hours)) %>%
  right_join(account_info_clean, by = c("user_id" = "customer_id")) %>%
  replace_na(list(count = 0)) %>%
  select(user_id:max_resolution, churn_status)
head(customer_support_join)

customer_support_join %>%
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of Number of Tickets Per Individual User",
       x = "Tickets Count",
       y = "User Count") +
  scale_x_continuous(breaks = seq(0, 8, 1)) +
  scale_y_continuous(breaks = seq(0, 120, 20),
                     limits = c(0, 120))

customer_support_join %>%
  ggplot(aes(x = churn_status, y = count)) +
  geom_boxplot() +
  labs(title = "User-Associated Tickets Count Distribution by Churn Status",
       x = "Churn Status",
       y = "Tickets Count") +
  scale_x_discrete(labels = c("No", "Yes"))

g1 <- customer_support_join %>%
  ggplot(aes(x = churn_status, y = mean_resolution)) +
  geom_boxplot() +
  labs(title = "Mean Resolution",
       x = "Churn Status",
       y = "Mean Resolution") +
  scale_x_discrete(labels = c("No", "Yes"))
g1
g2 <- customer_support_join %>%
  ggplot(aes(x = churn_status, y = median_resolution)) +
  geom_boxplot() +
  labs(title = "Median Resolution",
       x = "Churn Status",
       y = "Median Resolution") +
  scale_x_discrete(labels = c("No", "Yes"))
g2
g3 <- customer_support_join %>%
  ggplot(aes(x = churn_status, y = min_resolution)) +
  geom_boxplot() +
  labs(title = "Minimum Resolution",
       x = "Churn Status",
       y = "Minimum Resolution") +
  scale_x_discrete(labels = c("No", "Yes"))
g3
g4 <- customer_support_join %>%
  ggplot(aes(x = churn_status, y = max_resolution)) +
  geom_boxplot() +
  labs(title = "Maximum Resolution",
       x = "Churn Status",
       y = "Maximum Resolution") +
  scale_x_discrete(labels = c("No", "Yes"))
g4
grid.arrange(g1, g2, g3, g4, nrow = 2, top = "Resolution Metric Distribution by Churn Status")

customer_support_clean %>%
  mutate(resolution_over_12_hrs = ifelse(resolution_time_hours <= 12, "No", "Yes")) %>%
  inner_join(account_info_clean, by = c("user_id" = "customer_id")) %>%
  group_by(user_id, churn_status) %>%
  summarize(any_resolution_over_12_hrs = ifelse(any(resolution_over_12_hrs == "Yes"), "Yes", "No")) %>%
  ggplot(aes(x = churn_status, fill = any_resolution_over_12_hrs)) +
  geom_bar(position = "fill") +
  labs(title = "Relative Comparison of Churn Status For Individual User Based on Presence of at Least \nOne Ticket with >12 Hours Resolution",
       x = "Churn Status",
       y = "Relative User Count",
       fill = "Any Tickets With\n>12 Hours Resolution?") +
  scale_x_discrete(labels = c("No", "Yes")) +
  scale_y_continuous(breaks = seq(0, 1, 0.1))

customer_support_clean %>%
  mutate(resolution_over_12_hrs = ifelse(resolution_time_hours <= 12, "No", "Yes")) %>%
  summarize(resolution_over_12_hrs_pct = mean(resolution_over_12_hrs == "Yes") * 100)

# User activity analysis
user_activity_clean %>%
  ggplot(aes(x = fct_infreq(event_type))) +
  geom_bar() +
  labs(title = "Activity Type Count Across All Users",
       x = "Event Type",
       y = "Count") +
  scale_x_discrete(labels = c("Read Article", "Watch Video", "Track Workout", "Share Workout")) +
  scale_y_continuous(breaks = seq(0, 140, 20),
                     limits = c(0, 140))

user_activity_join <- user_activity_clean %>%
  group_by(user_id) %>%
  summarize(event_count = n()) %>%
  right_join(account_info_clean, by = c("user_id" = "customer_id")) %>%
  replace_na(list(event_count = 0)) %>%
  select(-(email:plan_list_price))
g5 <- user_activity_join %>%
  ggplot(aes(x = event_count)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Individual User Distribution by Associated\nActivities Count",
       x = "Event Count",
       y = "User Count") +
  scale_x_continuous(breaks = seq(0, 6, 1)) +
  scale_y_continuous(limits = c(0, 160),
                     breaks = seq(0, 160, 20))
g5
g6 <- user_activity_join %>%
  mutate(any_activity = ifelse(event_count > 0, "1+ Activities", "0 Activities")) %>%
  ggplot(aes(x = any_activity)) +
  geom_bar() +
  labs(title = "Non-Engaged vs Engaged Users",
       x = "Activities Count",
       y = "User Count")
g6
grid.arrange(g5, g6, nrow = 1, top = "User Engagement Overview")

g7 <- user_activity_join %>%
  mutate(engagement_status = ifelse(event_count > 0, "Engaged", "Non-Engaged")) %>%
  ggplot(aes(x = churn_status, fill = engagement_status)) +
  geom_bar(position = "fill") +
  labs(title = "Relative Engagement Status Ratio\nby Churn Status",
       x = "Churn Status",
       y = "Relative Engagement Status Ratio",
       fill = "Engagement Status") +
  scale_x_discrete(labels = c("No", "Yes"))
g7
g8 <- user_activity_join %>%
  mutate(event_count = factor(event_count)) %>%
  ggplot(aes(x = event_count, fill = churn_status)) +
  geom_bar(position = "fill") +
  labs(title = "Relative Churn Status Ratio\nby Activities Count",
       x = "Events Count",
       y = "Relative Churn Status Ratio",
       fill = "Churn Status") +
  scale_fill_discrete(labels = c("No", "Yes"),
                      palette = c("gray", "red"))
g8
grid.arrange(g7, g8, nrow = 1, top = "User Engagement vs Churn")
max(user_activity_clean$event_time) - min(user_activity_clean$event_time)
user_activity_join %>%
  summarize(non_engaged_users_pct = mean(event_count == 0) * 100,
            one_plus_activities_users_pct = mean(event_count >= 1) * 100,
            two_plus_activities_users_pct = mean(event_count >= 2) * 100)

# Graphs designed for presentation
account_info_clean %>%
  ggplot(aes(x = plan)) +
  geom_bar() +
  labs(x = "Plan Type",
       y = "Number of Customers",
       title = "Number of Customers Across Different Plan Tiers") +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, 20)) +
  theme(text = element_text(size = 18))

account_info_summary_metrics %>%
  mutate(mean_churn_pct = mean_churn * 100) %>%
  ggplot(aes(x = plan, y = mean_churn_pct)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = mean(account_info_clean$churn_status == "Y") * 100) +
  annotate("text", x = 4.5, y = 36, size = 6, label = paste0("Mean churn: ", mean(account_info_clean$churn_status == "Y") * 100, "%")) +
  labs(title = "Churn % by Plan Tier",
       y = "Churn (%)",
       x = "Plan Tiers") +
  theme(text = element_text(size = 18))

customer_support_clean %>%
  ggplot(aes(x = resolution_time_hours)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of Resolution Times for All Tickets",
       x = "Resolution Time (Hours)",
       y = "Tickets Count") +
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  scale_x_continuous(breaks = seq(0, 35, 5)) +
  theme(text = element_text(size = 18))

customer_support_join %>%
  ggplot(aes(x = churn_status, y = mean_resolution)) +
  geom_boxplot() +
  labs(title = "User-Specific Mean Resolution Values by\nChurn Status",
       x = "Churn Status",
       y = "Mean Resolution") +
  scale_x_discrete(labels = c("No", "Yes")) +
  theme(text = element_text(size = 18))

customer_support_clean %>%
  mutate(resolution_over_12_hrs = ifelse(resolution_time_hours <= 12, "No", "Yes")) %>%
  inner_join(account_info_clean, by = c("user_id" = "customer_id")) %>%
  group_by(user_id, churn_status) %>%
  summarize(any_resolution_over_12_hrs = ifelse(any(resolution_over_12_hrs == "Yes"), "Yes", "No")) %>%
  ggplot(aes(x = churn_status, fill = any_resolution_over_12_hrs)) +
  geom_bar(position = "fill") +
  labs(title = "Relative Comparison of Churn Status For Individual\nUser Based on Presence of at Least One Ticket with\n>12 Hours Resolution",
       x = "Churn Status",
       y = "Relative User Count",
       fill = "Any Tickets With\n>12 Hours Resolution?") +
  scale_x_discrete(labels = c("No", "Yes")) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  theme(text = element_text(size = 18))

user_activity_clean %>%
  ggplot(aes(x = fct_infreq(event_type))) +
  geom_bar() +
  labs(title = "Activity Type Count Across All Users",
       x = "Event Type",
       y = "Count") +
  scale_x_discrete(labels = c("Read Article", "Watch Video", "Track Workout", "Share Workout")) +
  scale_y_continuous(breaks = seq(0, 140, 20),
                     limits = c(0, 140)) +
  theme(text = element_text(size = 18))

user_activity_join %>%
  mutate(engagement_status = ifelse(event_count > 0, "Engaged\n(1+ activities)", "Non-Engaged\n(0 activities)")) %>%
  ggplot(aes(x = churn_status, fill = engagement_status)) +
  geom_bar(position = "fill") +
  labs(title = "Relative Engagement Status Ratio by Churn Status",
       x = "Churn Status",
       y = "Relative Engagement Status Ratio",
       fill = "Engagement Status") +
  scale_x_discrete(labels = c("No", "Yes")) +
  theme(text = element_text(size = 18))

user_activity_join %>%
  mutate(event_count = factor(event_count)) %>%
  ggplot(aes(x = event_count, fill = churn_status)) +
  geom_bar(position = "fill") +
  labs(title = "Relative Churn Status Ratio by Activities Count",
       x = "Events Count",
       y = "Relative Churn Status Ratio",
       fill = "Churn Status") +
  scale_fill_discrete(labels = c("No", "Yes")) +
  theme(text = element_text(size = 18))