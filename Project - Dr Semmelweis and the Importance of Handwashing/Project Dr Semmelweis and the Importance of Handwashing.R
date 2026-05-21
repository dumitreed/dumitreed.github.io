# Imported libraries
library(tidyverse)
library(readr)
library(ggplot2)

# Start coding here..

# Import data
yearly <- read_csv("yearly_deaths_by_clinic.csv")
monthly <- read_csv("monthly_deaths.csv")

# Check data
str(yearly)
str(monthly)
yearly
monthly

# Add a proportion_deaths column to each df
(yearly <- yearly %>%
  mutate(proportion_deaths = deaths / births))
(monthly <- monthly %>%
  mutate(proportion_deaths = deaths / births))

# Create two ggplot line plots
yearly %>%
  ggplot(aes(x = year, y = proportion_deaths, color = clinic)) +
  geom_line() +
  theme_bw()
monthly %>%
  ggplot(aes(x = date, y = proportion_deaths)) +
  geom_line() +
  theme_bw()

# Add a handwashing_started boolean column and new plot
monthly <- monthly %>%
  mutate(handwashing_started = ifelse(date >= as.Date("1847-06-01"), TRUE, FALSE))
monthly %>%
  ggplot(aes(x = date, y = proportion_deaths, color = handwashing_started)) +
  geom_line() +
  theme_bw()

# Calculate the mean proportion of deaths before and after handwashing
(monthly_summary <- monthly %>%
  group_by(handwashing_started) %>%
  summarize(mean_prop_deaths = mean(proportion_deaths)))