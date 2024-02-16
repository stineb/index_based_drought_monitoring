# Basic xgboost model with limited
# hyperparameter tuning

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

results <- lapply(unique(ml_df$site)[1:2], function(site){

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
    dplyr::select(-is_flue_drought)
  test <- rsample::testing(ml_df_split) |>
    dplyr::select(-is_flue_drought)

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
    v = 10
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

  # run the model on our test data
  # using predict()
  test_results <- predict(best_model, test)
  test_results <- bind_cols(flue = test$flue, test_results)

  # grab test metrics
  tm <- test_results |>
    metrics(truth = flue, estimate = .pred)

  # create output dir if required
  if(!dir.exists(here::here("/data/LSO"))){
    message("Output directory exists")
  } else {
    dir.create(here::here("data/LSO"))
  }

  # save best model
  # saveRDS(
  #   best_model,
  #   here::here(sprintf("data/LSO/%s_regression_model_spatial.rds", site)),
  #   compress = "xz"
  # )

  return(tm)
})


