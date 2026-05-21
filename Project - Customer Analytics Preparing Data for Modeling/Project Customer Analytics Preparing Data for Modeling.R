# Start your code here!
library(readr)
library(dplyr)
library(forcats)

# Import data
customer_train <- read_csv("customer_train.csv")

# Explore data
head(customer_train)
str(customer_train)
summary(customer_train)
head(customer_train_clean)
str(customer_train_clean)
summary(customer_train_clean)
summary(customer_train_clean$company_size)
summary(customer_train_clean$experience)
summary(ds_jobs_clean)

# Cleaning data
customer_train_clean <- customer_train %>%
  mutate(student_id = as.integer(student_id),
         city = as.factor(city),
         city_development_index = as.double(city_development_index),
         gender = as.factor(gender),
         relevant_experience = as.factor(relevant_experience),
         enrolled_university = as.factor(enrolled_university),
         education_level = as.factor(education_level),
         major_discipline = as.factor(major_discipline),
         experience = as.factor(experience),
         company_size = as.factor(company_size),
         company_type = as.factor(company_type),
         last_new_job = as.factor(last_new_job),
         training_hours = as.integer(training_hours),
         job_change = as.factor(job_change))
customer_train_clean <- customer_train_clean %>%
mutate(company_size = fct_recode(customer_train_clean$company_size, "Micro" = "<10",
                                 "Small" = "10-49",
                                 "Small" = "50-99",
                                 "Medium" = "100-499",
                                 "Medium" = "500-999",
                                 "Large" = "1000-4999",
                                 "Large" = "5000-9999",
                                 "Large" = "10000+"),
       experience = fct_collapse(customer_train_clean$experience,
                                 "<5" = c("<1", "1", "2", "3", "4"),
                                 "5-10" = c("5", "6", "7", "8", "9", "10"),
                                 ">10" = c("11", "12", "13", "14", "15", "16", "17", "18", "19", "20", ">20")))
ds_jobs_clean <- customer_train_clean %>%
  filter(experience == ">10", company_size == "Large")

# Memory usage check
object.size(customer_train)
object.size(ds_jobs_clean)