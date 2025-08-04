# Convert to a wide format, calculate
# additional vegetation indices and
# interpolate to daily values (with
# gap filling)

# load libraries
library(tidyverse)
library(here)
library(recipes)
library(caret)
library(visdat)

# install.packages("remotes")
# remotes::install_github("geco-bern/FluxDataKit")
library(FluxDataKit)

source(here("R/impute_one.R"))
source(here("R/fdk_read_onesite.R"))

# path <- "/home/yousra/index-based-extra-files/FLUXDATAKIT_FLUXNET/"
path <- "~/data_2/FluxDataKit/v3.4/zenodo_upload/fluxnet/"

## Get reflectance and fLUE data --------------------------------------------------
# prepared in previous steps

# read in raw data (long format)
site_data <- readRDS(here("data/MODIS_reflectance_data.rds"))

# read in flue data
flue <- readr::read_csv(here("data/flue_stocker18nphyt.csv"))

# pivot wider for easier band math
df <- site_data |>
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
df <- left_join(
  flue,
  df,
  by = join_by(site, date)
  )

# reduce data by missing flue
df <- df |>
  drop_na(flue) |>
  select(-year, -doy)

# visdat::vis_miss(df, warn_large_data = F)

## Add FluxDataKit data for imputation ---------------------------------------------
# Add daytime mean air temperature and shortwave radiation to be used as predictors
# for KNN imputation below.

ddf_fdk <- purrr::map_dfr(
  unique(df$site),
  ~fdk_read_onesite(., path)) |>
  dplyr::rename(
    date = TIMESTAMP,
    site = sitename
  ) |>
  dplyr::select(
    site,
    date,
    tair = TA_F_MDS,
    r_sw = SW_IN_F_MDS
  )

df <- df |>
  left_join(
    ddf_fdk,
    by = join_by(site, date)
    )

# visdat::vis_miss(df, warn_large_data = F)

## KNN imputation ------------------------------------------------------------------
# perform the KNN imputation here. This should be permissible because
# - KNN imputation as part of the resampling is too slow.
# - we're not imputinging missingness caused by the resampling
# - no structured missingness across sites

### Imputation step 1 -----------------
# Impute bands 1, 3, 5, and 7 with bands tair, r_sw, and reflectances of bands 2, 4, and 6.
# Particular order of variable imputation because bands 1, 3, 5, and 7 have
# most gaps.
# vis_miss(df, warn_large_data = FALSE)

df_imputed1 <- df

vars_to_impute_1 <- df_imputed1 |>
  select(starts_with("Nadir_") & contains(c("1", "3", "5", "7"))) |>
  names()

impute_predictors_1 <- df_imputed1 |>
  select(all_of(c("tair", "r_sw")), starts_with("Nadir_") & contains(c("2", "4", "6"))) |>
  names()

results_imputation_step1 <- list()
for (target_var in vars_to_impute_1){
  message(paste0("Imputing ", target_var, " ..."))
  out <- impute_one(target_var, df_imputed1, impute_predictors_1)
  df_imputed1 <- out$df
  results_imputation_step1[[target_var]] <- out$model
}

saveRDS(
  results_imputation_step1,
  here("data/results_imputation_step1.rds")
  )

### Imputation step 2 -----------------
# Impute LST with all reflectances, tair, and r_sw

df_imputed2 <- df_imputed1

vars_to_impute_2 <- df_imputed2 |>
  select(starts_with("LST")) |>  # important to start with filling LST as it has the most gaps
  names()

impute_predictors_2 <- df_imputed2 |>
  select(all_of(c("tair", "r_sw")), starts_with("Nadir_"), -ends_with("_filled")) |>
  names()

results_imputation_step2 <- list()
for (target_var in vars_to_impute_2){
  message(paste0("Imputing ", target_var, " ..."))
  out <- impute_one(target_var, df_imputed2, impute_predictors_2)
  df_imputed2 <- out$df
  results_imputation_step2[[target_var]] <- out$model
}

saveRDS(
  results_imputation_step2,
  here("data/results_imputation_step2.rds")
  )

vis_miss(df_imputed2, warn_large_data = FALSE)

## Add ERA5 data for fLUE prediction ------------------------------------------------
df <- df_imputed2 |>
  # remove tair and r_sw again as they are only used for imputation
  select(-ends_with("_filled"), -any_of(c("tair", "r_sw")))

# xxx: add ERA5 climate data to be used as model predictors (not for imputation, therefore not from site measurements)
# Extraction for sites is done in analysis/01_extract_er5_t2m_ssrd_data.R and in analysis/01_extract_pcwd_data.R
ddf_tm_ssrd <- read_csv(here::here("data/era5_data_tm_ssrd_sites.csv"))
ddf_pcwd <- read_csv(here::here("data/era5_pcwd_sites.csv"))

df <- df |>
  left_join(
    ddf_tm_ssrd |>
      select(-latitude, -longitude),
    by = join_by(site, date)
    ) |>
  left_join(
    ddf_pcwd |>
      select(site, date, pcwd = deficit),
    by = join_by(site, date)
    )

## More preparations ------------------------------------------------
# Rename band names
df <- df |>
  rename(
    NR_B1 = Nadir_Reflectance_Band1,
    NR_B2 = Nadir_Reflectance_Band2,
    NR_B3 = Nadir_Reflectance_Band3,
    NR_B4 = Nadir_Reflectance_Band4,
    NR_B5 = Nadir_Reflectance_Band5,
    NR_B6 = Nadir_Reflectance_Band6,
    NR_B7 = Nadir_Reflectance_Band7,
    LST = LST_Day_1km,
    t2m_era5 = t2m,
    ssrd_era5 = ssrd,
    pcwd_era5 = pcwd
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

# save the data with the indices and flue values
# for machine learning training
saveRDS(
  df,
  here("data/machine_learning_training_data.rds"),
  compress = "xz"
)
