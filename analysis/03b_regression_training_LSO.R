# Basic xgboost model with limited
# hyperparameter tuning with
# leave site out cross validation

# load the ecosystem
library(tidymodels)
library(dplyr)
library(here)
library(xgboost)

source("R/read_ml_data.R")
set.seed(0)

# read in training data
df <- read_ml_data(
  here::here("data/machine_learning_training_data.rds"),
  spatial = TRUE
) |>
  dplyr::select(
    -date, -is_flue_drought
  )

# Cross-validation by site (1 fold per site)
folds <- group_vfold_cv(df, group = site, v = 5, balance = "groups")

# Define recipe
rec <- recipe(flue ~ ., data = df |> dplyr::select(-site)) %>%
  step_normalize(all_predictors())

# Define xgboost model spec with tuning
xgb_spec <- boost_tree(
  trees = 1000,
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# alternative: random forest model spec
rf_spec <- rand_forest(
  mtry = tune(),         # number of predictors to consider at each split
  min_n = tune(),        # minimum number of data points in a node
  trees = 500            # number of trees (fixed here)
) %>%
  set_engine("ranger") %>%
  set_mode("regression")


# Create workflow
wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(rf_spec)

# Set up grid for tuning
# for xgboost
grid <- grid_space_filling(
  min_n(),
  tree_depth(),
  learn_rate(range = c(0.01, 0.3)),
  loss_reduction(),
  size = 3 # 100 xxx
)

# for random forest
grid <- grid_space_filling(
  mtry(range = c(1, 2)),     # since we have only 2 predictors
  min_n(range = c(2, 10)),
  size = 10
)

# Tune the model
set.seed(456)
tune_res <- tune_grid(
  wf,
  resamples = folds,
  grid = grid,
  metrics = metric_set(rmse),
  control = control_grid(save_pred = TRUE)
)

# select the best hyperparameter combination
best_config <- select_best(tune_res, "rmse")

# extract predictions on the held-out folds
cv_predictions_best <- collect_predictions(tune_res) %>%
  filter(.config == best_config$.config)

# inspect visually
library(ggplot2)

cv_predictions_best %>%
  ggplot(aes(x = y, y = .pred)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ id) +
  labs(title = "Held-out predictions per fold")

# Select best model and finalize
best_params <- select_best(tune_res, "rmse")

final_wf <- finalize_workflow(wf, best_params)

final_fit <- fit(final_wf, data = df)


# #---- model definition and tuning ----
#
# # setup model
# model <- parsnip::boost_tree(
#   trees = 50,
#   min_n = tune()
# ) |>
#   set_engine("xgboost") |>
#   set_mode("regression")
#
# # create workflow
# wflow <- workflows::workflow() |>
#   workflows::add_model(model) |>
#   workflows::add_formula(flue ~ .)
#
# # set hyperparameter selection settings
# hp_settings <- dials::grid_latin_hypercube(
#   tune::extract_parameter_set_dials(wflow),
#   size = 3
# )
#
#
# # Example recipe
# rec <- recipe(y ~ x1 + x2, data = df)
#
# # Model specification
# rf_spec <- rand_forest(mtry = 2, trees = 100) %>%
#   set_engine("ranger") %>%
#   set_mode("regression")
#
# # Workflow
# wf <- workflow() %>%
#   add_recipe(rec) %>%
#   add_model(rf_spec)
#
# # Fit with resampling
# results <- fit_resamples(
#   wf,
#   resamples = cv_folds,
#   metrics = metric_set(rmse, rsq),
#   control = control_resamples(save_pred = TRUE)
# )
