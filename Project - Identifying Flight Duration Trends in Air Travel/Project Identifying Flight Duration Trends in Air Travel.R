# Import required packages
library(dplyr)
library(readr)

# Load the data
flights <- read_csv("flights2022-h2.csv")
airlines <- read_csv("airlines.csv")
airports <- read_csv("airports.csv")

# Data filtering and joining
flights_2 <- flights %>%
  select(origin, carrier, dest, air_time) %>%
  left_join(airlines, by = "carrier") %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  rename(airport_name = name.y, airline_name = name.x) %>%
  group_by(airline_name, airport_name) %>%
  summarize(n_flights = n(),
            avg_duration = mean(air_time, na.rm = TRUE)) %>%
  ungroup()
flights_2

# Which airline and airport pair receives the most flights from NYC and what is the average duration of that flight?
frequent <- flights_2 %>%
  arrange(desc(n_flights)) %>%
  select(airline_name, airport_name, avg_duration) %>%
  head(n = 1)
frequent

# Find the airport that has the longest average flight duration (in hours) from NYC. What is the name of this airport?
longest <- flights_2 %>%
  arrange(desc(avg_duration)) %>%
  select(airline_name, airport_name, avg_duration) %>%
  head(n = 1)
longest

# Which airport is the least frequented destination for flights departing from JFK?
least_df <- flights %>%
  select(origin, dest) %>%
  filter(origin == "JFK") %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  group_by(name) %>%
  summarize(n_flights = n()) %>%
  arrange(n_flights)
least <- toString(least_df[1, "name"])
least