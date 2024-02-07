# Full model training using the original
# complete data set including the old
# 6.0 MODIS data releases which include
# some of the legacy ocean based products
# hosted on Appeears (currently discontinued
# as in access - but still available elsewhere)
#
# Basic xgboost model with limited
# hyperparameter tuning

# load the ecosystem
library(tidymodels)
library(ranger)
library(dplyr)
source("R/read_ml_data.R")
source("R/normalized_difference.R")
source("R/merge_normalized_differences.R")
set.seed(0)

#---- data partitioning ----

# read in training data
ml_df <- read_ml_data(
  here::here("data/machine_learning_training_data.rds"),
  spatial = FALSE
)

# add normalized difference ratios for surf_refl
# columns
ml_df <- merge_normalized_differences(ml_df)

# create a data split across
# across both droughted and non-droughted days
ml_df_split <- ml_df |>
  rsample::initial_split(
    strata = is_flue_drought,
    prop = 0.8
  )

# select training and testing
# data based on this split
train <- rsample::training(ml_df_split) |>
  select(-flue)
test <- rsample::testing(ml_df_split) |>
  select(-flue)

#---- model definition and tuning ----

# setup model
model <- parsnip::boost_tree(
  trees = 50,
  min_n = tune(),
  tree_depth = tune()
) |>
  set_engine("xgboost") |>
  set_mode("classification")

# create workflow
wflow <-
  workflows::workflow() |>
  workflows::add_model(model) |>
  workflows::add_formula(is_flue_drought ~ .)

# set hyperparameter selection settings
hp_settings <- dials::grid_latin_hypercube(
  tune::extract_parameter_set_dials(wflow),
  size = 3
)

# cross-validation settings
folds <- rsample::vfold_cv(
  train,
  v = 3
  )

# optimize the model (hyper) parameters
# using the:
# 1. workflow (i.e. model)
# 2. the cross-validation across training data
# 3. the (hyper) parameter specifications
# all data are saved for evaluation
results <- tune::tune_grid(
  wflow,
  resamples = folds,
  grid = hp_settings,
  control = tune::control_grid(
    save_pred = TRUE,
    save_workflow = TRUE
    )
)

# select the best model
best <- tune::select_best(
  results,
  metric = "roc_auc"
)

# cook up a model using finalize_workflow
# combining best parameters with a workflow
best_wflow <- tune::finalize_workflow(
  wflow,
  best
)

# run (consolidate) fit on best hyperparameters
best_model <- fit(best_wflow, train)

# run the model on our test data
# using predict()
test_results <- predict(best_model, test)

# load the caret library to
# access confusionMatrix functionality
library(caret)

# use caret's confusionMatrix function to get
# a full overview of metrics
caret::confusionMatrix(
  reference = as.factor(test$is_flue_drought),
  data = as.factor(test_results$.pred_class)
)

# save best model
saveRDS(
  best_model,
  "data/classification_model_full.rds",
  compress = "xz"
)

