# Merge all modis products into one dataset
# while removing spurious values (set to NA)

library(dplyr)
library(readr)

# list all sites (folder)
sites <- list.dirs("data-raw/modis_data/")

files <- list.files(
  "data-raw/modis_data/AR-Vir/",
  "*.csv",
  full.names = TRUE
  )

test <- readr::read_csv(files[2])

#test <- read_data(files[1])
test <- read_data(
  files[3],
  name = "Nadir_Refl",
  filter_bands = FALSE
  )

print(head(test))


mutate(ndvi = (sur_refl_b02 * 0.0001 - sur_refl_b01 * 0.0001)/(sur_refl_b02 * 0.0001 + sur_refl_b01 * 0.0001)) %>%
  mutate(evi = 2.5 * (sur_refl_b02 * 0.0001 - sur_refl_b01 * 0.0001) / (sur_refl_b02 * 0.0001 + 6 * sur_refl_b01 * 0.0001 - 7.5 * sur_refl_b03* 0.0001 + 1)) %>%
  mutate(NIRv = ndvi * (sur_refl_b02 * 0.0001)) %>%  #NIR
  mutate(cci = (sur_refl_b11 * 0.0001 - sur_refl_b01 * 0.0001)/(sur_refl_b11 * 0.0001 + sur_refl_b01 * 0.0001)) %>%
  mutate(pri = (sur_refl_b11 * 0.0001 - sur_refl_b12 * 0.0001)/(sur_refl_b11 * 0.0001 + sur_refl_b12 * 0.0001))
