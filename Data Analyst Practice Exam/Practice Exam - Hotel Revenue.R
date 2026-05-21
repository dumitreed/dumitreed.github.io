# Load packages
library(readr)
library(tidyverse)
library(stringr)
library(ggplot2)

# Import data
ancillary_spend <- read_csv("da_sample_ancillary_spend.csv")
stay_details <- read_csv("da_sample_stay_details.csv")
guest_profiles <- read_csv("da_sample_guest_profiles.csv")

# Initial exploratory
str(ancillary_spend)
glimpse(ancillary_spend)
summary(ancillary_spend %>% mutate(guest_id = factor(guest_id), category = factor(category)))

str(stay_details)
glimpse(stay_details)
summary(stay_details %>% mutate(guest_id = factor(guest_id), stay_id = as.integer(stay_id), booking_channel = factor(booking_channel), reason_for_stay = factor(reason_for_stay), number_of_guests = factor(number_of_guests)))

str(guest_profiles)
glimpse(guest_profiles)
summary(guest_profiles %>% mutate(loyalty_tier = factor(loyalty_tier)))

# Cleaning datasets
ancillary_spend_clean <- ancillary_spend %>%
  mutate(guest_id = as.integer(str_replace_all(guest_id, "[:alpha:]", "")),
         category = factor(category))
ancillary_spend_clean %>%
  ggplot(aes(x = amount_spent)) +
  geom_histogram() +
  facet_wrap(~category)
ancillary_spend_clean %>%
  filter(is.na(amount_spent)) %>%
  summary()
ancillary_spend_clean %>% summary()
ancillary_spend_clean <- ancillary_spend_clean %>%
  replace_na(list(amount_spent = median(ancillary_spend_clean$amount_spent, na.rm = TRUE)))

stay_details_clean <- stay_details %>%
  mutate(stay_id = seq.int(from = 1, to = 600, by = 1),
         booking_channel = factor(booking_channel),
         reason_for_stay = factor(reason_for_stay),
         number_of_guests = factor(number_of_guests))
summary(stay_details_clean)
stay_details_clean %>%
  group_by(guest_id, check_in_date) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

guest_profiles_clean <- guest_profiles %>%
  mutate(loyalty_tier = factor(loyalty_tier))
summary(guest_profiles_clean)

# Data exploration for spending patterns
# Total and average spent by guest_id
ancillary_spend_total_by_guest <- ancillary_spend_clean %>%
  group_by(guest_id) %>%
  summarize(total_spent = sum(amount_spent))

ancillary_spend_avg_by_guest <- ancillary_spend_clean %>%
  group_by(guest_id) %>%
  summarize(total_spent = sum(amount_spent),
            visits = n(),
            avg_spent = total_spent/visits)

ancillary_spend_total_by_guest %>%
  ggplot(aes(x = total_spent)) +
  geom_histogram(binwidth = 10)
ancillary_spend_total_by_guest %>%
  select(-guest_id) %>%
  summary()

ancillary_spend_avg_by_guest %>%
  ggplot(aes(x = avg_spent)) +
  geom_histogram(binwidth = 10)
ancillary_spend_avg_by_guest %>%
  select(avg_spent) %>%
  summary()

# Total/Average spent by loyalty_tier
ancillary_spend_total_by_guest %>%
  inner_join(guest_profiles_clean, by = "guest_id") %>%
  ggplot(aes(x = loyalty_tier, y = total_spent)) +
  geom_boxplot()
ancillary_spend_avg_by_guest %>%
  inner_join(guest_profiles_clean, by = "guest_id") %>%
  ggplot(aes(x = loyalty_tier, y = avg_spent)) +
  geom_boxplot()

# Total/Average spent by marketing_consent
ancillary_spend_total_by_guest %>%
  inner_join(guest_profiles_clean, by = "guest_id") %>%
  ggplot(aes(x = marketing_consent, y = total_spent)) +
  geom_boxplot()
ancillary_spend_avg_by_guest %>%
  inner_join(guest_profiles_clean, by = "guest_id") %>%
  ggplot(aes(x = marketing_consent, y = avg_spent)) +
  geom_boxplot()