# xgboost with leave-group of sites-out CV, with site-stratified initial split, comprehensive hp tuning (to be done by Yousra)

# todo:
#   - hyperparameter tuning
#   - add (ERA5) temperature and VPD as predictors
#   - leave-single site-out CV instead of leave-group of sites-out CV
#   - try out without initial split, train on all data, consider only "Within CV" results

# load the ecosystem
library(tidymodels)
library(dplyr)
library(caret)
library(tidyverse)
library(tictoc)
# remotes::install_github("geco-bern/rgeco")
library(rgeco)
# remotes::install_github("geco-bern/FluxDataKit")
library(FluxDataKit)

# source("R/read_ml_data.R")

set.seed(0)

## Read data -------------------------------------------------------------------
# exclude meteo data TA and VPD
ml_df <- readRDS(
  here::here("data/machine_learning_training_data.rds")
  # here::here("data/machine_learning_training_data_meteo.rds")
) |>
  # select (-date, -doy, -year, -VPD_DAY_F_MDS, -TA_DAY_F_MDS) |>
  select (-date, -doy, -year) |>
  rename(
    NR_B1 = Nadir_Reflectance_Band1,
    NR_B2 = Nadir_Reflectance_Band2,
    NR_B3 = Nadir_Reflectance_Band3,
    NR_B4 = Nadir_Reflectance_Band4,
    NR_B5 = Nadir_Reflectance_Band5,
    NR_B6 = Nadir_Reflectance_Band6,
    NR_B7 = Nadir_Reflectance_Band7,
    LST = LST_Day_1km) |>
  na.omit()

## Initial data split ----------------------------------------------------------
### By site, stratified ------------
# split by site, stratified by climate zone and vegetation type
siteinfo <- readr::read_csv("~/Downloads/site_info.csv") |>
  filter(sitename %in% sites) |>
  mutate(strata = interaction(koeppen_code_beck, igbp_land_use, drop = TRUE)) |>
  left_join(
    ml_df |>
      select(sitename = site, cluster) |>
      distinct(),
    by = join_by(sitename)
  )

set.seed(1)

sites_test <- siteinfo |>
  # group_by(strata) |>
  group_by(cluster) |>
  sample_n(size = 4, replace = FALSE) |>
  ungroup()

train <- ml_df |>
  filter(!(site %in% sites_test$sitename))
test <- ml_df |>
  filter(site %in% sites_test$sitename)

# ### Mixing up sites --------------------
# # create a data split across
# # across both drought and non-drought days
# ml_df_split <- ml_df |>
#   rsample::initial_split(
#     strata = "is_flue_drought",
#     prop = 0.7
#   )

# # select training and testing
# # data based on this split
# train <- rsample::training(ml_df_split) |>
#   dplyr::select(-is_flue_drought)
# test <- rsample::testing(ml_df_split) |>
#   dplyr::select(-is_flue_drought)


## Common training setup -------------------------------------------------------
# Cross-validation by site (1 fold per site)
# Set the factor levels for 'site'
# train$site <- factor(train$site, levels = levels(factor(ml_df$site)))

# # leave-single site-out CV
# num_sites <- length(unique(train$site))
# folds <- rsample::group_vfold_cv(
#   data = train,
#   group = "site",
#   v = num_sites
# )

# 5-fold leave-group of sites-out CV
folds <- group_vfold_cv(train, group = "site", v = 5, balance = "groups")

# # leave-single sites-out CV
# folds <- group_vfold_cv(train, group = "site", v = length(unique(train$site)))

# Define recipe
rec <- recipe(
  flue ~ NR_B1 + NR_B2 + NR_B3 + NR_B4 + NR_B5 + NR_B6 + NR_B7 + LST,
  data = train
  )

## xgboost  --------------------------------------------------------------------
### Model spec ------------
xgb_spec <- parsnip::boost_tree(
  trees = 500,
  min_n = tune(),
  # tree_depth = tune(),
  # learn_rate = tune(),
  # loss_reduction = tune()
) |>
  set_engine("xgboost") |>
  set_mode("regression")

### Workflow --------
wf_xgb <- workflow() %>%
  add_recipe(rec) %>%
  add_model(xgb_spec)

### Tuning grid -----------
loss_reduction_raw <- loss_reduction(range = c(0, 10), trans = NULL)
grid_xgb <- grid_space_filling(
  min_n(range = c(5, 50)),
  # tree_depth(range = c(3, 12)),
  # learn_rate(range = c(0.01, 0.3)),
  # loss_reduction_raw,
  size = 5  # 30
)

### Model tuning --------------
tic()
tune_res_xgb <- tune::tune_grid(
  wf_xgb,
  resamples = folds,
  grid = grid_xgb,
  control = tune::control_grid(
    save_pred = TRUE,
    save_workflow = TRUE
  )
)
toc()

saveRDS(tune_res_xgb, file = here::here("data/tune_res_xgb_YOUSRA.rds"))

### Plot results ---------
# select the best hyperparameter combination
best_config_xgb <- tune::select_best(
  tune_res_xgb,
  metric = "rmse"
)

#### Within CV ------------
# extract predictions on the held-out folds
cv_predictions_best_xgb <- collect_predictions(tune_res_xgb) %>%
  filter(.config == best_config_xgb$.config)

# inspect visually
out <- analyse_modobs2(
  cv_predictions_best_xgb,
  mod = ".pred",
  obs = "flue",
  type = "hex",
  pal = "magma",
  shortsubtitle = TRUE
)

out$gg

#### On held-out data ---------------
# cook up a model using finalize_workflow
# combining best parameters with a workflow
final_wf_xgb <- tune::finalize_workflow(
  wf_xgb,
  best_config_xgb
)

# run (consolidate) fit on best hyperparameters
best_model <- fit(final_wf_xgb, train)

# run the model on our test data
# using predict()
test_results <- predict(best_model, test)
test_results <- bind_cols(flue = test$flue, test_results)

# grab test metrics
tm <- test_results |>
  metrics(truth = flue, estimate = .pred)

# inspect visually
out <- analyse_modobs2(
  test_results,
  mod = ".pred",
  obs = "flue",
  type = "hex",
  pal = "magma",
  shortsubtitle = TRUE
)

out$gg

# # save best model
# saveRDS(
#   best_model,
#   "vignettes/regression-models-LSO-GKF/regression_model_spatial_MODIS.rds",
#   compress = "xz"
# )
