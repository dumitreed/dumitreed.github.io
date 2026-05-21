# Load the necessary packages
library(tidyverse)
library(yardstick)
library(Metrics)
library(caret)

# Load the data
hd_data <- read.csv("Cleveland_hd.csv")

# Inspect the first five rows
head(hd_data, 5)
str(hd_data)

# Start coding here...add as many cells as you like!
# Data preparation
hd_data_clean <- hd_data %>%
  mutate(sex = as.factor(sex),
         cp = as.factor(cp),
         fbs = as.factor(fbs),
         restecg = as.factor(restecg),
         exang = as.factor(exang),
         slope = as.factor(slope),
         thal = as.factor(thal),
         class = as.factor(class))
summary(hd_data_clean)
hist(hd_data_clean$ca)
hd_data_clean$ca[is.na(hd_data_clean$ca)] <- mean(hd_data_clean$ca, na.rm = TRUE)
ggplot(hd_data_clean, aes(x = thal)) +
  geom_bar()
hd_data_clean$thal[is.na(hd_data_clean$thal)] <- as.factor(3)
hd_data_clean <- hd_data_clean %>%
  mutate(hd = as.factor(ifelse(class == 0, 0, 1)))

# Which predictors are related to heart disease as indicated by the class column?
highly_significant <- list("age", "sex", "thalach")

# Fit a model
model <- glm(hd ~ age + sex + thalach, data = hd_data_clean, family = "binomial")
summary(model)
hd_data_clean$hd_predicted <- predict(model, data = hd_data_clean, type = "response")
hd_data_clean$hd_predicted_class <- as.factor(ifelse(hd_data_clean$hd_predicted < 0.5, 0, 1))
(accuracy <- accuracy(hd_data_clean$hd_predicted_class, hd_data_clean$hd))
(confusion <- conf_mat(table(hd_data_clean$hd_predicted_class, hd_data_clean$hd)))