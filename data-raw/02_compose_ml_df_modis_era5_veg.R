

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

#add sr and temp from fluxnet data for imputation

install.packages("devtools")
devtools::install_github("geco-bern/FluxDataKit")
library(FluxDataKit)
#path to fluxdatakit
path <- "/home/yousra/index-based-extra-files/FLUXDATAKIT_FLUXNET/"

# print(file.exists(path))
# print(list.files(path))
# list.files(path = path, pattern = "FLX_", full.names = FALSE)


sites <- FluxDataKit::fdk_site_info |>
  left_join(
    fdk_site_fullyearsequence,
    by = "sitename"
  )

#function to select sites in fluxdatakit files
read_onesite <- function(site, path){
  filename <- list.files(path,
                         pattern = paste0("FLX_", site, "_FLUXDATAKIT_FULLSET_DD"),
                         full.names = TRUE
  )
  print(filename)
  if (length(filename) == 0) {
    message("No file found for site: ", site)
    return(tibble())  # Return an empty tibble if no file is found
  }
  out <- read_csv(filename) |>
    mutate(sitename = site)
  return(out)
}

# read all daily data for the selected sites
ddf <- purrr::map_dfr(
  sites$sitename,
  ~read_onesite(., path)
)

colnames(ddf)

#select only maximum daily AT and SW incoming radiation

data_tm_sw <- ddf |>
  dplyr::rename(
    date = TIMESTAMP,
    site = sitename
  ) |>
  dplyr::select(
    date,
    site,
    TMAX_F_MDS,
    SW_IN_F_MDS
  )


flue <- readr::read_csv("data/flue_stocker18nphyt.csv")
info <- readr::read_csv("data/site_info.csv")
info <- info |>
  dplyr::rename(
      site = sitename
  )

flue <- merge(flue, info, by = c('site'))


# pivot wider
data_tm_sw <- ddf |>
  dplyr::select(
    sitename,
    TMAX_F_MDS,
    SW_IN_F_MDS,
    TIMESTAMP,
  ) |>
  rename(site = sitename,
         date = TIMESTAMP)

# merge with flue
site_data_wide <- left_join(
  flue,
  data_tm_sw,
  by = join_by(site, date)
)

# read in raw data (long format)
site_data <- readRDS(here("data/MODIS_reflectance_data.rds"))

# pivot wider
site_data_modis<- site_data |>
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

#merge modis bands with sr and tm from fluxnet
site_data_mds_flx <- left_join(
  site_data_modis,
  site_data_wide,
  by = join_by(site, date)
)
# |>
#   drop_na(flue)

#save the data
saveRDS(
   site_data_mds_flx,
   here("data/imput_data_modis_sw_tm_fluxnet.rds"),
   compress = "xz"
)

# interpolate missing values and daily
# data for the full time series

# 1. create long term mean
# 2. use long term mean weights in spline based gap filling
# (see phenocam routine)

# for now do a quick linear interpolation
site_data_wide <- site_data_wide |>
  mutate(
    across(
      where(is.double),
      \(x) zoo::na.approx(x, na.rm = FALSE)
    )
  )






source(here("R/impute_one.R"))

# read in raw data (long format)
# site_data <- readRDS(here("data/MODIS_reflectance_data.rds"))
#
# # read in flue data
# flue <- readr::read_csv(here("data/flue_stocker18nphyt.csv"))
#
# # pivot wider for easier band math
# site_data_wide <- site_data |>
#   dplyr::select(
#     site,
#     band,
#     date,
#     value
#   ) |>
#   tidyr::pivot_wider(
#     names_from = band,
#     values_from = value,
#     values_fn = mean
#   )
#
# # merge with flue
# site_data_wide <- left_join(
#   flue,
#   site_data_wide,
#   by = join_by(site, date)
# )
#
# # reduce data by missing flue
# site_data_wide <- site_data_wide |>
#   drop_na(flue)

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

#read data for imputation
site_data_wide <- readRDS(here("data/imput_data_modis_sw_tm_fluxnet.rds"))

# join with fluxnet observations from FluxDataKit to add covariates for KNN imputation as part of the modelling workflow
# xxx: use real data for air temperature and shortwave radiation, to be used as predictors for KNN imputation
site_data_wide <- site_data_wide |>
  rename(tair = TMAX_F_MDS,
        r_sw = SW_IN_F_MDS)|>
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
  select(-ends_with("_filled"), -any_of(c("tair", "r_sw")))

vis_miss(df, warn_large_data = FALSE)

# xxx: add ERA5 climate data to be used as model predictors (not for imputation, therefore not from site measurements)
# extract time series of daily values from tidy ERA5-Land data and join into site_data_wide. Also add potential cumulative
# water deficit (PCWD) data.

#read files
tm_ssrd <- read.csv(here::here("data/era5_data_tm_ssrd_sites.csv"))
pcwd <- read.csv(here::here("data/era5_pcwd_sites.csv"))
#add vegetation type as predictor
# modis_veg <- readRDS(here::here("data/machine_learning_training_data_veg_type.rds"))

df1<-  df |>
  dplyr::select(
    -elv,
    -product,
    -koeppen_code_beck,
    -koeppen_code,
    -whc
  )

tm_ssrd$date <- as.Date(tm_ssrd$date)
pcwd$date <- as.Date(pcwd$date)

ml_df <- left_join(
  df1,
  tm_ssrd,
  by = join_by(site, date)
)

ml_df <- left_join(
  ml_df,
  pcwd,
  by = join_by(site,date)
)|>
  select(
    -rounded_lon,
    -rounded_lat)|>
    rename(
      NR_B1 = Nadir_Reflectance_Band1,
      NR_B2 = Nadir_Reflectance_Band2,
      NR_B3 = Nadir_Reflectance_Band3,
      NR_B4 = Nadir_Reflectance_Band4,
      NR_B5 = Nadir_Reflectance_Band5,
      NR_B6 = Nadir_Reflectance_Band6,
      NR_B7 = Nadir_Reflectance_Band7,
      LST = LST_Day_1km,
      igbp = igbp_land_use)

colnames(ml_df)


ml_df <- ml_df |>
  mutate(
    t2m = rnorm(nrow(df)),
    ssrd = rnorm(nrow(df)),
    deficit = rnorm(nrow(df))
  )


# save the data with the indices and flue values
# for machine learning training
saveRDS(
  ml_df,
  here("data/machine_learning_training_data_imput_era5.rds"),
  compress = "xz"
)
