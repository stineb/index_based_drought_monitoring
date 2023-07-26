# Convert to a wide format, calculate
# additional vegetation indices and
# interpolate to daily values (with
# gap filling)

# load libraries
library(tidyverse)

# pivot wider for easier band math
site_data_wide <- site_data |>
  select(
    site,
    date,
    band,
    value
  ) |>
  pivot_wider(
    names_from = band,
    values_from = value
  )

# calculate vegetation indices
site_data_wide <- site_data_wide |>
  mutate(
    ndvi = (sur_refl_b02 - sur_refl_b01)/(sur_refl_b02  + sur_refl_b01),
    evi = 2.5 * (sur_refl_b02  - sur_refl_b01 ) / (sur_refl_b02  + 6 * sur_refl_b01  - 7.5 * sur_refl_b03 + 1),
    NIRv = ndvi * (sur_refl_b02),
    cci = (sur_refl_b11  - sur_refl_b01 )/(sur_refl_b11  + sur_refl_b01 ),
    pri = (sur_refl_b11  - sur_refl_b12 )/(sur_refl_b11  + sur_refl_b12 )
  )

# interpolate missing values and daily
# data for the full time series






# save the data with the indices (as a wide format)
# saveRDS(
#   site_data,
#   "data/MODIS_reflectance_data_indices.rds",
#   compress = "xz"
# )
