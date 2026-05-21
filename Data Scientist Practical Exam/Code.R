# Load packages
library(tidyverse)
library(readr)
library(stringr)
library(forcats)
library(tidymodels)
library(caret)
library(vip)

# Set seed
set.seed(13)

# Import data
recipe_site_traffic <- read_csv("recipe_site_traffic_2212.csv")

# Explore data
glimpse(recipe_site_traffic)
summary(recipe_site_traffic)
slice_sample(recipe_site_traffic, n = 100) %>%
  print(n = 100)

# Replace high_traffic NAs with "Not high" label
# Recode "Chicken Breast" into "Chicken"
# Transform servings from chr to numeric, extracting only the numeric part from the string
# Factorize all relevant features
recipe_site_traffic_clean <- recipe_site_traffic %>%
  replace_na(list(high_traffic = "Not high")) %>%
  mutate(recipe = factor(as.integer(recipe)),
         category = fct_collapse(factor(category), "Chicken" = c("Chicken", "Chicken Breast")),
         servings = as.numeric(str_extract(servings, "\\d")),
         high_traffic = factor(high_traffic))
recipe_site_traffic_clean %>%
  count(recipe) %>%
  arrange(desc(n))
summary(recipe_site_traffic_clean)
summary(recipe_site_traffic_clean$category)
glimpse(recipe_site_traffic_clean)
  
# Exploring missing data
visdat::vis_miss(recipe_site_traffic) +
  theme(axis.text.x = element_text(angle = 60)) +
  labs(title = "Heatmap of Missing Values in the Initial Data Set")
visdat::vis_miss(recipe_site_traffic_clean) +
  theme(axis.text.x = element_text(angle = 60)) +
  labs(title = "Heatmap of Missing Values for Numeric Variables")
recipe_site_traffic_missing <- recipe_site_traffic_clean %>%
  filter(is.na(calories))
missing_plot1 <- recipe_site_traffic_missing %>%
  ggplot(aes(x = category)) +
  geom_bar() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(size = 8.5)) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 3)) +
  labs(title = "Rows with Missing Values by Category",
       x = "Category",
       y = "Count")
missing_plot2 <- recipe_site_traffic_missing %>%
  ggplot(aes(x = servings)) +
  geom_bar() +
  theme_classic() +
  theme(plot.title = element_text(size = 8.5)) +
  scale_x_continuous(breaks = seq(1, 6, by = 1)) +
  labs(title = "Rows with Missing Values by Servings",
       y = "Count",
       x = "Servings")
missing_plot3 <- recipe_site_traffic_missing %>%
  ggplot(aes(x = high_traffic)) +
  geom_bar() +
  theme_classic() +
  theme(plot.title = element_text(size = 8.5)) +
  labs(title = "Rows with Missing Values by Outcome",
       x = "Outcome",
       y = "Count")
gridExtra::grid.arrange(missing_plot3, missing_plot1, missing_plot2, ncol = 3)
recipe_site_traffic_clean <- recipe_site_traffic_clean %>%
  na.omit()
visdat::vis_miss(recipe_site_traffic_clean) +
  theme(axis.text.x = element_text(angle = 60)) +
  labs(title = "Heatmap of Missing Values after Removal")

# Exploratory analysis of data set by variable
glimpse(recipe_site_traffic_clean)
recipe_site_traffic_clean %>%
  ggplot(aes(x = high_traffic)) +
  geom_bar() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 600),
                     breaks = seq(0, 600, 100)) +
  labs(title = "Count of the Bi-Level Outcome Variable",
       x = "Outcome",
       y = "Count")
recipe_site_traffic_clean %>%
  ggplot(aes(x = calories)) +
  geom_histogram(bins = 100) +
  facet_wrap(~ high_traffic, nrow = 2) +
  theme_classic() +
  labs(title = "Distribution of Calories by Outcome",
       x = "Calories",
       y = "Count")
carb_plot <- recipe_site_traffic_clean %>%
  ggplot(aes(x = high_traffic, y = carbohydrate)) +
  geom_boxplot() +
  theme_classic() +
  theme(plot.title = element_text(size = 9)) +
  scale_y_continuous(limits = c(0, 600),
                     breaks = seq(0, 600, 100)) +
  labs(title = "Boxplot of Carbohydrate by Outcome",
       x = "Outcome",
       y = "Carbohydrate")
sugar_plot <- recipe_site_traffic_clean %>%
  ggplot(aes(x = high_traffic, y = sugar)) +
  geom_boxplot() +
  theme_classic() +
  theme(plot.title = element_text(size = 9)) +
  labs(title = "Boxplot of Sugar by Outcome",
       x = "Outcome",
       y = "Sugar")
protein_plot <- recipe_site_traffic_clean %>%
  ggplot(aes(x = high_traffic, y = protein)) +
  geom_boxplot() +
  theme_classic() +
  theme(plot.title = element_text(size = 9)) +
  scale_y_continuous(limits = c(0, 400),
                     breaks = seq(0, 400, 100)) +
  labs(title = "Boxplot of Protein by Outcome",
       x = "Outcome",
       y = "Protein")
gridExtra::grid.arrange(carb_plot, sugar_plot, protein_plot, ncol = 3)
recipe_site_traffic_clean %>%
  ggplot(aes(x = servings, fill = high_traffic)) +
  geom_bar(position = "dodge") +
  theme_classic() +
  scale_x_continuous(breaks = seq(1, 6, by = 1)) +
  scale_y_continuous(limits = c(0, 250),
                     breaks = seq(0, 250, 50)) +
  labs(title = "Count of Servings by Outcome",
       x = "Servings",
       y = "Count",
       fill = "Outcome")
recipe_site_traffic_clean %>%
  ggplot(aes(x = fct_infreq(category))) +
  geom_bar() +
  coord_flip() +
  theme_classic() +
  ylim(0, 200) +
  labs(title = "Overall Count of Category Types",
       x = "Category",
       y = "Count")
recipe_site_traffic_clean %>%
  ggplot(aes(x = category, fill = fct_relevel(high_traffic, "Not high", "High"))) +
  geom_bar(position = "fill") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0)) +
  coord_flip() +
  scale_fill_discrete(breaks = c("High", "Not high")) +
  labs(title = "Distribution Ratio of Category by Outcome",
       x = "Category",
       y = "Outcomes Ratio",
       fill = "Outcome")

# Data preparation for modeling
split <- initial_split(recipe_site_traffic_clean, prop = 0.75, strata = high_traffic)
train <- training(split)
test <-testing(split)
mean(train$high_traffic == "High")
mean(test$high_traffic == "High")

# Fitting a baseline model
baseline_recipe <- recipe(high_traffic ~ ., data = train) %>%
  update_role(recipe, new_role = "ID") %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_nzv(all_predictors())
baseline_model <- logistic_reg()
baseline_workflow <- workflow() %>%
  add_recipe(baseline_recipe) %>%
  add_model(baseline_model)
baseline_fit <- baseline_workflow %>%
  fit(data = train)
vip(extract_fit_parsnip(baseline_fit)) +
  theme_classic() +
  scale_x_discrete(labels = c("Calories", "Category: Breakfast", "Category: Chicken", "Category: Lunch/Snacks", "Category: Dessert", "Category: One Dish Meal", "Category: Vegetable", "Category: Meat", "Category: Pork", "Category: Potato")) +
  labs(title = "Feature Importance for Logistic Regression Model")
baseline_predict <- baseline_fit %>%
  predict(new_data = test)
baseline_precision <- precision_vec(truth = test$high_traffic, estimate = baseline_predict$.pred_class)
baseline_precision
baseline_conf_mat <- confusionMatrix(data = baseline_predict$.pred_class, reference = test$high_traffic, positive = "High")
baseline_conf_mat

baseline_predict_prob <- baseline_fit %>%
  predict(new_data = test, type = "prob")
baseline_predict_prob %>%
  ggplot(aes(x = .pred_High)) +
  geom_histogram(bins = 50) +
  theme_classic() +
  ylim(0, 30) +
  labs(title = "Histogram of Probability Values Associated with High Traffic for Logistic Regression Model",
       x = "High Outcome Probability",
       y = "Count")
baseline_threshold_comparison <- data.frame(threshold = seq(0.5, 0.95, 0.05))
for (i in 1:nrow(baseline_threshold_comparison)) {
  threshold <- baseline_threshold_comparison$threshold[i]
  pred_class <- factor(ifelse(baseline_predict_prob$.pred_High > threshold, "High", "Not high"), levels = c("High", "Not high"))
  baseline_threshold_comparison$precision[i] <- precision_vec(truth = test$high_traffic, estimate = pred_class)
  baseline_threshold_comparison$true_positives[i] <- sum(test$high_traffic == pred_class & pred_class == "High")
}
baseline_threshold_comparison

# Fitting a comparison model
comparison_recipe <- recipe(high_traffic ~ ., data = train) %>%
  update_role(recipe, new_role = "ID") %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_nzv(all_predictors())
comparison_model <- rand_forest() %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")
comparison_workflow <- workflow() %>%
  add_recipe(comparison_recipe) %>%
  add_model(comparison_model)
train_cv <- vfold_cv(train,
                     v = 10,
                     repeats = 5,
                     strata = high_traffic)
tune_spec <- rand_forest(trees = tune(),
                         min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("classification")
tune_grid <- grid_regular(trees(),
                          min_n(),
                          levels = 5)
tune_workflow <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(high_traffic ~ .)
tune_results <- tune_workflow %>%
  tune_grid(resamples = train_cv,
            grid = tune_grid,
            metrics = metric_set(yardstick::precision))
tune_results %>%
  collect_metrics() %>%
  arrange(desc(mean)) %>%
  print(n = 25)
best <- tune_results %>%
  select_best(metric = "precision")
final_model <- rand_forest(trees = 1000,
                           min_n = 40) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")
final_workflow <- workflow() %>%
  add_recipe(comparison_recipe) %>%
  add_model(final_model)
comparison_fit <- final_workflow %>%
  fit(data = train)
vip(extract_fit_parsnip(comparison_fit)) +
  theme_classic() +
  scale_x_discrete(labels = c("Category: Meat", "Category: Pork", "Category: Chicken", "Carbohydrate", "Sugar", "Category: Potato", "Category: Breakfast", "Calories", "Category: Vegetable", "Protein")) +
  labs(title = "Feature Importance for Random Forest Model")
comparison_predict <- comparison_fit %>%
  predict(new_data = test)
comparison_precision <- precision_vec(truth = test$high_traffic, estimate = comparison_predict$.pred_class)
comparison_precision
comparison_conf_mat <- confusionMatrix(data = comparison_predict$.pred_class, reference = test$high_traffic, positive = "High")
comparison_conf_mat

comparison_predict_prob <- comparison_fit %>%
  predict(new_data = test, type = "prob")
comparison_predict_prob %>%
  ggplot(aes(x = .pred_High)) +
  geom_histogram(bins = 50) +
  theme_classic() +
  ylim(0, 15) +
  labs(title = "Histogram of Probability Values Associated with High Traffic for Random Forest Model",
       x = "High Outcome Probability",
       y = "Count")
threshold_comparison <- data.frame(threshold = seq(0.5, 0.95, 0.05))
for (i in 1:nrow(threshold_comparison)) {
  threshold <- threshold_comparison$threshold[i]
  pred_class <- factor(ifelse(comparison_predict_prob$.pred_High > threshold, "High", "Not high"), levels = c("High", "Not high"))
  threshold_comparison$precision[i] <- precision_vec(truth = test$high_traffic, estimate = pred_class)
  threshold_comparison$true_positives[i] <- sum(test$high_traffic == pred_class & pred_class == "High")
}
threshold_comparison

# Comparison table & plot for threshold effect
final_comparison <- bind_rows(list(baseline_model = baseline_threshold_comparison, comparison_model = threshold_comparison), .id = "id") %>%
  arrange(threshold)
final_comparison
threshold_graph1 <- final_comparison %>%
  ggplot(aes(x = threshold, y = precision, color = id)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.8, linetype = 3) +
  scale_color_discrete(labels = c("Logistic Regression", "Random Forest")) +
  theme_classic() +
  labs(title = "Model Precision Dependency on Probability Threshold",
       x = "Probability Threshold",
       y = "Precision",
       color = "Model")
threshold_graph2 <- final_comparison %>%
  ggplot(aes(x = threshold, y = true_positives, color = id)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 0.65, linetype = 3) +
  scale_color_discrete(labels = c("Logistic Regression", "Random Forest")) +
  theme_classic() +
  labs(title = "Model True Positives Count Dependency on Probability Threshold",
       x = "Probability Threshold",
       y = "True Positives Count",
       color = "Model")
gridExtra::grid.arrange(threshold_graph1, threshold_graph2, ncol = 1)