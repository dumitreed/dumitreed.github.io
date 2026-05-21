# We've loaded the necessary packages for you in the first cell. Please feel free to add as many cells as you like!
suppressMessages(library(dplyr)) # This line is required to check your answer correctly
options(readr.show_types = FALSE) # This line is required to check your answer correctly
library(readr)
library(readxl)
library(stringr)

# Begin coding here ...
airbnb_price <- read_csv("airbnb_price.csv")
airbnb_room_type <- read_excel("airbnb_room_type.xlsx")
airbnb_last_review <- read_tsv("airbnb_last_review.tsv")

# airbnb_price df price column change to numeric
airbnb_price$price <- as.double(str_replace(airbnb_price$price, " dollars", ""))

# airbnb_room_type room_type column as factor cleaned
airbnb_room_type <- airbnb_room_type %>%
  mutate(room_type = as.factor(str_to_title(room_type)))

# airbnb_last_review last_review column as date
airbnb_last_review$last_review <- as.Date(airbnb_last_review$last_review, format = "%B %d %Y")

# merge all df into one
airbnb_all <- airbnb_room_type %>%
  full_join(airbnb_price, by = "listing_id") %>%
  full_join(airbnb_last_review, by ="listing_id")

# find first_reviewed
first_reviewed <- airbnb_all %>%
  slice_min(order_by = last_review, n = 1, with_ties = FALSE) %>%
  select(last_review) %>%
  pull()

# find last_reviewed
last_reviewed <- airbnb_all %>%
  slice_max(order_by = last_review, n = 1, with_ties = FALSE) %>%
  select(last_review) %>%
  pull()

# number of private room listings
nb_private_rooms <- airbnb_all %>%
  count(room_type) %>%
  filter(room_type == "Private Room") %>%
  select(n) %>%
  pull()

# average price for all rooms
avg_price <- airbnb_all %>%
  summarize(mean = round(mean(price), digits = 2)) %>%
  pull()

# final tibble containing answers
review_dates <- as_tibble(data.frame(first_reviewed = first_reviewed, last_reviewed = last_reviewed, nb_private_rooms = nb_private_rooms, avg_price = avg_price))