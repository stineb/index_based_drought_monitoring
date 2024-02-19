# Basic xgboost model with limited
# hyperparameter tuning with
# leave site out cross validation

# load the ecosystem
library(tidymodels)
library(dplyr)
library(caret)
source("R/read_ml_data.R")
set.seed(0)

# read in training data
ml_df <- read_ml_data(
  here::here("data/machine_learning_training_data.rds"),
  spatial = TRUE
)

# Leave-Site-Out cross validation loop
results <- lapply(unique(ml_df$site), function(site){

  #---- data partitioning ----

  # create a data split across
  # across both drought and non-drought days
  ml_df_split <- ml_df |>
    filter(
      site != !!site
    ) |>
    rsample::initial_split(
      strata = is_flue_drought,
      prop = 0.8
    )

  # select training and testing
  # data based on this split
  train <- rsample::training(ml_df_split) |>
    dplyr::select(
      -site,
      -is_flue_drought,
      -date
      )

  test <- rsample::testing(ml_df_split) |>
    dplyr::select(
      -site,
      -is_flue_drought,
      -date
      )

  #---- model definition and tuning ----

  # setup model
  model <- parsnip::boost_tree(
    trees = 50,
    min_n = tune()
  ) |>
    set_engine("xgboost") |>
    set_mode("regression")

  # create workflow
  wflow <-
    workflows::workflow() |>
    workflows::add_model(model) |>
    workflows::add_formula(flue ~ .)

  # set hyperparameter selection settings
  hp_settings <- dials::grid_latin_hypercube(
    tune::extract_parameter_set_dials(wflow),
    size = 3
  )

  # cross-validation settings
  folds <- rsample::vfold_cv(
    train,
    v = 2
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

  # grab out of sample test data
  LSO_test <- ml_df |>
    filter(
      site == !!site
    )

  # run the model on our test data
  # using predict()
  test_results <- predict(best_model, LSO_test)$.pred
  test_results <- bind_cols(
    LSO_test,
    flue_predicted = test_results
    )

  return(test_results)
})

# collapse list to data frame
results <- bind_rows(results)

# write summary results to file
saveRDS(
  results,
  "data/LSO_results.rds",
  compress = "xz"
)
