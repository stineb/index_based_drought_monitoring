# Convert to a wide format, calculate
# additional vegetation indices and
# interpolate to daily values (with
# gap filling)

# load libraries
library(tidyverse)
library(FluxDataKit)
library(VIM)
library(here)
library(recipes)
library(caret)
library(visdat)

source(here("R/impute_one.R"))

# read in raw data (long format)
site_data <- readRDS(here("data/MODIS_reflectance_data.rds"))

# read in flue data
flue <- readr::read_csv(here("data/flue_stocker18nphyt.csv"))

# pivot wider for easier band math
site_data_wide <- site_data |>
  dplyr::select(
    site,
    band,
    date,
    value
  ) |>
  tidyr::pivot_wider(
    names_from = band,
    values_from = value,
    values_fn = mean
  )

# merge with flue
site_data_wide <- left_join(
  flue,
  site_data_wide,
  by = join_by(site, date)
  )

# reduce data by missing flue
site_data_wide <- site_data_wide |>
  drop_na(flue)

# interpolate missing values and daily
# data for the full time series

# 1. create long term mean
# 2. use long term mean weights in spline based gap filling
# (see phenocam routine)

# beni: avoided linear interpolation to fill gaps, therefore commented out
# # for now do a quick linear interpolation
# site_data_wide <- site_data_wide |>
#   mutate(
#     across(
#       where(is.double),
#       \(x) zoo::na.approx(x, na.rm = FALSE)
#       )
#   )

# join with fluxnet observations from FluxDataKit to add covariates for KNN imputation as part of the modelling workflow
# xxx: use real data for air temperature and shortwave radiation, to be used as predictors for KNN imputation
site_data_wide <- site_data_wide |>
  mutate(
    tair = rnorm(nrow(site_data_wide)),  # just for demo
    r_sw = rnorm(nrow(site_data_wide))   # just for demo
    )

# visdat::vis_miss(site_data_wide, warn_large_data = F)

# perform the KNN imputation here. This should be permissible because
# - KNN imputation as part of the resampling is too slow.
# - we're not imputinging missingness caused by the resampling
# - no structured missingness across sites

## Imputation step 1 -----------------
# Paticular order of variable imputation because bands 1, 3, 5, and 7 have
# most gaps.
vis_miss(site_data_wide, warn_large_data = FALSE)
df <- site_data_wide

vars_to_impute_1 <- site_data_wide |>
  select(starts_with("Nadir_") & contains(c("1", "3", "5", "7"))) |>
  names()

impute_predictors_1 <- site_data_wide |>
  select(all_of(c("tair", "r_sw")), starts_with("Nadir_") & contains(c("2", "4", "6"))) |>
  names()

for (target_var in vars_to_impute_1){
  message(paste0("Imputing ", target_var, " ..."))
  df <- impute_one(target_var, df, impute_predictors_1)
}

## Imputation step 2 -----------------
vars_to_impute_2 <- site_data_wide |>
  select(starts_with("LST")) |>  # important to start with filling LST as it has the most gaps
  names()

impute_predictors_2 <- site_data_wide |>
  select(all_of(c("tair", "r_sw")), starts_with("Nadir_")) |>
  names()

for (target_var in vars_to_impute_2){
  message(paste0("Imputing ", target_var, " ..."))
  df <- impute_one(target_var, df, impute_predictors_2)
}

df <- df |>
  select(-ends_with("_filled"))

vis_miss(df, warn_large_data = FALSE)

# xxx: add ERA5 climate data to be used as model predictors (not for imputation, therefore not from site measurements)
# extract time series of daily values from tidy ERA5-Land data and join into site_data_wide. Also add potential cumulative
# water deficit (PCWD) data.
df <- df |>
  mutate(
    tair_era5 = rnorm(nrow(df)),  # just for demo
    r_sw_era5 = rnorm(nrow(df)),  # just for demo
    pcwd_era5 = rnorm(nrow(df))   # just for demo
  )

# save the data with the indices and flue values
# for machine learning training
saveRDS(
  df,
  here("data/machine_learning_training_data.rds"),
  compress = "xz"
)
