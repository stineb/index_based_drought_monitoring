# Basic xgboost model with limited
# hyperparameter tuning

# load the ecosystem
library(tidymodels)
library(ranger)
library(dplyr)
set.seed(0)

#---- data partitioning ----

# read in training data
ml_df <- readRDS(
  here::here("data/machine_learning_training_data.rds")
  ) |>
  dplyr::select(
    -date,
    -year,
    -doy,
    -cluster,
    -site
  ) |>
  mutate(
    is_flue_drought = as.factor(is_flue_drought)
  ) |>
  na.omit()

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
  metric = "rmse"
)

# cook up a model using finalize_workflow
# combining best parameters with a workflow
best_wflow <- tune::finalize_workflow(
  wflow,
  best
)

# run (consolidate) fit on best hyperparameters
best_model <- fit(best_wflow, train)
print(best_model)
saveRDS(best_model, "data/classification_model.rds", compress = "xz")

