# Merge all modis products into one dataset
# while removing spurious values (set to NA)

library(dplyr)
library(readr)
source("R/read_appeears.R")

# list all sites (folder)
sites <- list.dirs("data-raw/modis_data/")

files <- list.files(
  "data-raw/modis_data/AR-Vir/",
  "*.csv",
  full.names = TRUE
  )

readr::read_csv(files[1])

test <- read_appeears(
  files[1],
  name = "Nadir_Ref"
  )

print(test, n = 20)
test$value[test$value > 5000] <- NA

plot(test$date, test$value)

# mutate(
#   ndvi = (sur_refl_b02 - sur_refl_b01)/(sur_refl_b02  + sur_refl_b01 ),
#   evi = 2.5 * (sur_refl_b02  - sur_refl_b01 ) / (sur_refl_b02  + 6 * sur_refl_b01  - 7.5 * sur_refl_b03 + 1),
#   NIRv = ndvi * (sur_refl_b02 ),
#   cci = (sur_refl_b11  - sur_refl_b01 )/(sur_refl_b11  + sur_refl_b01 ),
#   pri = (sur_refl_b11  - sur_refl_b12 )/(sur_refl_b11  + sur_refl_b12 )
# )
