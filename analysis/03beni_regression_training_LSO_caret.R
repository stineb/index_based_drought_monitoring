# xgboost and random forest with leave-group of sites-out CV, no initial split, comprehensive hp tuning, alternatively testing Random Forest

# load the ecosystem
library(caret)
library(rlang)  # ← required for := and sym()
library(purrr)
library(dplyr)
library(here)
library(ggplot2)
library(tictoc)
library(recipes)
library(vip)
# remotes::install_github("geco-bern/rgeco")
library(rgeco)
# remotes::install_github("geco-bern/FluxDataKit")
library(FluxDataKit)

source("R/read_ml_data.R")

## Read data -------------------------------------------------------------------
df <- read_ml_data(
  here::here("data/machine_learning_training_data.rds")
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
# KNN imputation of missing values with the following predictors
# note: this is used if `df` contains missing values and if air temperature and
# shortwave radiation (tair, r_sw) are added as predictors from local measurements.
# construct normalised difference indices
band_names <- df |>
  select(starts_with("NR_B")) |>
  names()
band_pairs <- combn(band_names, 2, simplify = FALSE)

# Build named list of expressions using set_names()
nd_exprs <- band_pairs |>
  map(~ expr((!!sym(.x[1]) - !!sym(.x[2])) / (!!sym(.x[1]) + !!sym(.x[2]) + 1e-8))) |>
  set_names(map_chr(band_pairs, ~ paste0("nd_", .x[1], "_", .x[2])))

nd_names <- names(nd_exprs)

impute_only_vars <- c("tair", "r_sw")
predict_only_vars <- c("tair_era5", "r_sw_era5", "pcwd_era5", nd_names)
impute_vars <- c(impute_only_vars, band_names)
impute_and_predict_vars <- c(band_names, predict_only_vars)

# Define recipe
rec <- recipe(
  flue ~ .,
  data = df
) |>

  # Role handling
  update_role(site, date, is_flue_drought, new_role = "ID") |>
  update_role(all_of(impute_only_vars), new_role = "impute") |>

  # KNN imputation
  step_impute_knn(
    all_of(band_names),
    impute_with = all_of(impute_vars),
    neighbors = 5
    ) |>

  # Feature engineering
  step_mutate(!!!nd_exprs) |>

  # Preprocessing (only model-predictors!)
  step_normalize(all_numeric_predictors(), -all_of(impute_only_vars)) |>
  step_novel() |>
  step_dummy(vegtype)

# check roles
summary(rec)

# Cross-validation by site (number of folds corresponds to number of sites) or group of sites
folds <- caret::groupKFold(
  df$site,
  k = 5 # length(unique(df$site))
  )

traincotrlParams <- caret::trainControl(
  index = folds,
  method = "cv",
  savePredictions = "final"   # predictions on each validation resample are then available as modl$pred$Resample
)

tune_grid <- expand.grid(
  .mtry = 3, # c(3, 5, 7),
  .min.node.size = 15, # c(5, 15, 25),
  .splitrule = "variance"
)

model <- train(
  rec,
  data            = df,
  metric          = "RMSE",
  method          = "ranger",
  tuneGrid        = tune_grid,
  trControl       = traincotrlParams,
  replace         = TRUE,
  sample.fraction = 0.5,
  num.trees       = 500,         # to be boosted to 2000 for the final model
  importance      = "impurity"   # for variable importance analysis, alternative: "permutation"
)


# inspect out-of-sample validation results visually
preds <- model$pred
preds$site <- df$site[preds$rowIndex]

out <- rgeco::analyse_modobs2(
  preds,
  mod = "pred",
  obs = "obs",
  type = "hex",
  pal = "magma",
  shortsubtitle = TRUE
)

out$gg
