# xgboost with leave-group of sites-out CV, no initial split, comprehensive hp tuning, alternatively testing Random Forest

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

source("R/read_ml_data.R")

## Read data -------------------------------------------------------------------
df <- read_ml_data(
  here::here("data/machine_learning_training_data.rds"),
  spatial = TRUE
) |>
  dplyr::select(
    -date, -is_flue_drought
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

# add vegetation type as predictor
sites <- df |>
  select(site) |>
  distinct() |>
  left_join(
    fdk_site_info |>
      select(site = sitename, vegtype = igbp_land_use),
    by = join_by(site)
  )

# manually add missing info to site info
sites$vegtype[which(sites$site == "AR-Vir")] <- "ENF"
sites$vegtype[which(sites$site == "AU-Ade")] <- "WSA"
sites$vegtype[which(sites$site == "AU-Fog")] <- "WET"
sites$vegtype[which(sites$site == "AU-Wom")] <- "EBF"
sites$vegtype[which(sites$site == "DE-Spw")] <- "WET"
sites$vegtype[which(sites$site == "DK-NuF")] <- "WET"
sites$vegtype[which(sites$site == "SN-Dhr")] <- "SAV"
sites$vegtype[which(sites$site == "US-Wi4")] <- "ENF"

# add vegetation type
df <- df |>
  left_join(
    sites,
    by = join_by(site)
  )

## Common training setup -------------------------------------------------------
# Cross-validation by group of sites (5 folds)
# folds <- group_vfold_cv(df, group = site, v = 5, balance = "groups")

# Cross-validation by site (number of folds corresponds to number of sites)
folds <- group_vfold_cv(df, group = "site", v = length(unique(df$site)))

# Define recipe
rec <- recipe(
  flue ~ NR_B1 + NR_B2 + NR_B3 + NR_B4 + NR_B5 + NR_B6 + NR_B7 + LST + vegtype,
  data = df
  )

## xgboost  --------------------------------------------------------------------
### Model spec ------------
xgb_spec <- boost_tree(
  trees = 500,
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

### Workflow --------
wf_xgb <- workflow() %>%
  add_recipe(rec) %>%
  add_model(xgb_spec)

### Tuning grid -----------
set.seed(1)
loss_reduction_raw <- loss_reduction(range = c(0, 10), trans = NULL)
grid_xgb <- grid_space_filling(
  min_n(range = c(10, 60)),
  tree_depth(range = c(3, 12)),
  learn_rate(range = c(0.01, 0.3)),
  loss_reduction_raw,
  size = 30
)

### Model tuning --------------
# 7 min for 1 CV (one hyperparam combination), ~4 h for grid search size = 30
tic()
tune_res_xgb <- tune_grid(
  wf_xgb,
  resamples = folds,
  grid = grid_xgb,
  metrics = metric_set(rmse),
  control = control_grid(save_pred = TRUE)
)
toc()

saveRDS(tune_res_xgb, file = here::here("data/tune_res_xgb.rds"))

### Plot results ---------
#### min_n ----------
tune_res_xgb %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(x = min_n, y = mean)) +
  geom_point() +
  geom_line() +
  labs(title = "RMSE by min_n", y = "RMSE")

# #### tree_depth ----------
# tune_res_xgb %>%
#   collect_metrics() %>%
#   filter(.metric == "rmse") %>%
#   ggplot(aes(x = tree_depth, y = mean)) +
#   geom_point() +
#   geom_line() +
#   labs(title = "RMSE by tree_depth", y = "RMSE")

# select the best hyperparameter combination
best_config_xgb <- select_best(tune_res_xgb, metric = "rmse")
saveRDS(best_config_xgb, file = here::here("data/best_config_xgb.rds"))

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
ggsave(here::here("fig/modobs_cv_xgb.pdf"), width = 5, height = 4)

### Final fit ------------
# Select best model and finalize (takes about 5 min on Beni's Mac M1)
final_wf_xgb <- finalize_workflow(wf_xgb, best_config_xgb)

tic()
final_fit_xgb <- fit(final_wf_xgb, data = df)
toc()

saveRDS(final_fit_xgb, file = here::here("data/final_fit_xgb.rds"))


## Random Forest ---------------------------------------------------------------
### Model spec ------------
rf_spec <- rand_forest(
  mtry = tune(),         # number of predictors to consider at each split
  min_n = tune(),        # minimum number of data points in a node
  trees = 500            # number of trees (fixed here)
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

### Workflow --------
wf_rf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(rf_spec)

### Tuning grid -----------
# for random forest (~2.5 h for 30)
mtry_typical <- floor((length(names(df))-2)/3)
grid_rf <- grid_space_filling(
  mtry(range = c(mtry_typical - 1, mtry_typical + 1)),     # since we have only 2 predictors
  min_n(range = c(5, 50)),
  size = 30  # more xxx
)

### Model tuning --------------
tune_res_rf <- tune_grid(
  wf_rf,
  resamples = folds,
  grid = grid_rf,
  metrics = metric_set(rmse),
  control = control_grid(save_pred = TRUE)
)

saveRDS(tune_res_rf, file = here::here("data/tune_res_rf.rds"))

### Plot results ---------
# select the best hyperparameter combination
best_config_rf <- select_best(tune_res_rf, metric = "rmse")
saveRDS(best_config_rf, file = here::here("data/best_config_rf.rds"))

# extract predictions on the held-out folds
cv_predictions_best_rf <- collect_predictions(tune_res_rf) %>%
  filter(.config == best_config_rf$.config)

# inspect visually
out <- analyse_modobs2(
  cv_predictions_best_rf,
  mod = ".pred",
  obs = "flue",
  type = "hex",
  pal = "magma",
  shortsubtitle = TRUE
)

out$gg
ggsave(here::here("fig/modobs_cv_rf.pdf"), width = 5, height = 4)

### Final fit ------------
# Select best model and finalize (takes about 5 min on Beni's Mac M1)
final_wf_rf <- finalize_workflow(wf_rf, best_config_rf)

tic()
final_fit_rf <- fit(final_wf_rf, data = df)
toc()

saveRDS(final_fit_rf, file = here::here("data/final_fit_rf.rds"))

