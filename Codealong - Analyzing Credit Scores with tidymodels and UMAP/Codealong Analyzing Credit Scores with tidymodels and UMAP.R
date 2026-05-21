# Load packages
library(tidyverse)
library(tidymodels)
library(readr)
library(embed)

# Load data
url <- "https://assets.datacamp.com/production/repositories/6081/datasets/e02471e553bc28edddc1fe862666d36e04daed80/credit_score.csv"
credit_df <- read_csv(url)

# Explore data
str(credit_df)
glimpse(credit_df)
summary(credit_df)

credit_df <- credit_df %>% # change credit_score as target variable
  mutate(credit_score = factor(credit_score, levels = c("Poor", "Standard", "Good")))
summary(credit_df$credit_score)

# annual income density plot
ggplot(credit_df, aes(x = annual_income, color = credit_score)) +
  geom_density() +
  labs(title = "Anual Income by Credit Score",
       x = "Annual Income",
       y = "Density") +
  theme_bw() +
  xlim(0, 200000)

# age density plot
ggplot(credit_df, aes(x = age, color = credit_score)) +
  geom_density() +
  labs(title = "Age by Credit Score",
       x = "Age",
       y = "Density") +
  theme_bw()

# delay from due date by credit history
ggplot(credit_df, aes(x = delay_from_due_date, y = credit_history_months, color = credit_score)) +
  geom_point(position = "jitter", alpha = 0.4) +
  labs(title = "Delayed Payments vs Credit History Length",
       x = "Delay From Due Date (Days)",
       y = "Credit History Length (Months)") +
  theme_bw()

# credit utilization by number of credit cards
ggplot(credit_df, aes(x = num_credit_card, y = credit_utilization_ratio, color = credit_score)) +
  geom_jitter(alpha = 0.4) +
  labs(title = "Credit Utilization vs Number of Credit Cards",
       x = "Number of Credit Cards",
       y = "Credit Utilization (%)") +
  theme_bw() +
  xlim(0, 12)

# Create tidymodels recipe using UMAP dimensionality reduction
umap_recipe <- recipe(credit_score ~ ., data = credit_df) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_umap(all_predictors(), outcome = vars(credit_score), num_comp = 2)

# Train and extract the UMAP transformed data
umap_credit_df <- umap_recipe %>%
  prep() %>%
  bake(new_data = NULL)
glimpse(umap_credit_df)
ggplot(umap_credit_df, aes(x = UMAP1, y = UMAP2, color = credit_score)) +
  geom_jitter(alpha = 0.4) +
  labs(title = "UMAP Dimensions Plot") +
  theme_bw()

# Prepare training and testing data sets
set.seed(3)
credit_split <- initial_split(credit_df, prop = 0.75)
credit_train <- training(credit_split)
credit_test <- testing(credit_split)

# Build workflow without UMAP using decision trees
dt_recipe <- recipe(credit_score ~ ., data = credit_df)
dt_model <- decision_tree(mode = "classification")
dt_workflow <- workflow() %>%
  add_recipe(dt_recipe) %>%
  add_model(dt_model)
dt_fit <- dt_workflow %>%
  fit(data = credit_train)
predict_df <- credit_test %>%
  bind_cols(predicted = predict(dt_fit, credit_test))
glimpse(predict_df)
accuracy(predict_df, truth = credit_score, estimate = .pred_class)
precision(predict_df, truth = credit_score, estimate = .pred_class)
f_meas(predict_df, truth = credit_score, estimate = .pred_class)

# Build workflow with UMAP using decision trees
dt_recipe_umap <- recipe(credit_score ~ ., data = credit_df) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_umap(all_predictors(), outcome = vars(credit_score), num_comp = 2)
dt_model <- decision_tree(mode = "classification")
dt_workflow_umap <- workflow() %>%
  add_recipe(dt_recipe_umap) %>%
  add_model(dt_model)
dt_fit_umap <- dt_workflow_umap %>%
  fit(data = credit_train)
predict_umap_df <- credit_test %>%
  bind_cols(predicted = predict(dt_fit_umap, credit_test))
glimpse(predict_umap_df)
accuracy(predict_umap_df, truth = credit_score, estimate = .pred_class)
precision(predict_umap_df, truth = credit_score, estimate = .pred_class)
f_meas(predict_umap_df, truth = credit_score, estimate = .pred_class)
