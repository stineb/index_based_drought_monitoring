# Convert to a wide format, calculate
# additional vegetation indices and
# interpolate to daily values (with
# gap filling)

# load libraries
library(tidyverse)

# read in raw data (long format)
site_data <- readRDS("data/MODIS_reflectance_data.rds")

# read in flue data
flue <- readr::read_csv("data/flue_stocker18nphyt.csv")

# pivot wider for easier band math
site_data_wide <- site_data |>
  select(
    site,
    band,
    date,
    value
  ) |>
  pivot_wider(
    names_from = band,
    values_from = value
  )

# merge with flue
site_data_wide <- left_join(
  flue,
  site_data_wide
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

# save the data with the indices and flue values
# for machine learning training
saveRDS(
  site_data_wide,
  "data/machine_learning_training_data.rds",
  compress = "xz"
)
