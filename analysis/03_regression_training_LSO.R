# Basic xgboost model with limited
# hyperparameter tuning with
# leave site out cross validation

# load the ecosystem
library(tidymodels)
library(dplyr)
library(here)
library(xgboost)
library(ggplot2)
library(tictoc)
# remotes::install_github("geco-bern/rgeco")
library(rgeco)
# remotes::install_github("geco-bern/FluxDataKit")
library(FluxDataKit)

## Read data -------------------------------------------------------------------
ml_df <- read_ml_data(
  here::here("data/machine_learning_training_data.rds"),
  spatial = TRUE
  ) |>
  rename(
    NR_B1 = Nadir_Reflectance_Band1,
    NR_B2 = Nadir_Reflectance_Band2,
    NR_B3 = Nadir_Reflectance_Band3,
    NR_B4 = Nadir_Reflectance_Band4,
    NR_B5 = Nadir_Reflectance_Band5,
    NR_B6 = Nadir_Reflectance_Band6,
    NR_B7 = Nadir_Reflectance_Band7,
    LST = LST_Day_1km
  )

## Leave-Site-Out cross validation loop ----------------
results <- lapply(unique(ml_df$site), function(site, ml_df){

  ### data partitioning ----

  # Take all data for training except for designated site
  # note: this is different from Koen's original code where data was split again
  # and only 80% was used for training, but remainder 20% was not used at all.
  train <- ml_df |>
    filter(
      site != !!site
    )

  ### model definition and tuning ----

  # Define recipe ----
  rec <- recipe(
    flue ~ NR_B1 + NR_B2 + NR_B3 + NR_B4 + NR_B5 + NR_B6 + NR_B7 + LST, # + vegtype,
    data = train
    )
    # step_dummy(vegtype)

  ### setup model ----
  # # xgboost
  # model <- parsnip::boost_tree(
  #   trees = 50,
  #   min_n = tune()
  #   ) |>
  #   set_engine("xgboost") |>
  #   set_mode("regression")

  # random forest
  model <- parsnip::rand_forest(
    mtry = tune(),         # number of predictors to consider at each split
    min_n = tune(),        # minimum number of data points in a node
    trees = 500            # number of trees (fixed here)
  ) %>%
    set_engine("ranger") %>%
    set_mode("regression")


  ### create workflow ----
  wflow <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(model)

  ### hyperparameter tuning ----
  # set hyperparameter selection settings - used in Koen's version, outdated now
  # hp_settings <- dials::grid_latin_hypercube(
  #   tune::extract_parameter_set_dials(wflow),
  #   size = 3
  # )

  set.seed(1)
  hp_settings <- grid_space_filling(
    min_n(range = c(10, 60)),
    size = 5
  )

  ### Cross-validation folds ----
  # cross-validation settings - used in Koen's version (with v = 2)
  folds <- rsample::vfold_cv(
    train,
    v = 5
  )

  # # Cross-validation by group of sites (5 folds)
  # folds <- rsample::group_vfold_cv(
  #   train,
  #   group = site,
  #   v = 5,
  #   balance = "groups"
  #   )

  # # Cross-validation by site (number of folds corresponds to number of sites)
  # folds <- rsample::group_vfold_cv(
  #   df,
  #   group = "site",
  #   v = length(unique(df$site))
  #   )

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
    metrics = metric_set(rmse),
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
