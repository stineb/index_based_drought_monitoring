# Merge all modis products into one dataset
# while removing spurious values (set to NA)

library(dplyr)
library(readr)
source("R/read_appeears.R")

# list all sites (folder)
sites <- list.dirs("data-raw/modis_data/")

files <- list.files(
  "data-raw/modis_data/",
  "*.csv",
  recursive = TRUE,
  full.names = TRUE
  )

site_data <- lapply(files, function(file) {

  # split out site name
  site <- paste(
    stringr::str_split(basename(file),"-")[[1]][1:2],
    collapse = "-"
  )

  message(sprintf("---- Processing site: %s", site))

  # split out product name
  product <- stringr::str_split(basename(file), "-")[[1]][3]

  if(product == "MCD43A4"){
    df <- read_appeears(
      file,
      name = "Nadir_Reflectance"
    )
  }

  if(product == "MOD11A1"){
    df <- read_appeears(
      file,
      name = "LST_Day_1km"
    )
  }

  if(product == "MODOCGA"){
    df <- read_appeears(
      file,
      name = "sur_refl_"
    )
  }

  if(product == "MOD09GA"){
    df <- read_appeears(
      file,
      name = "sur_refl_"
    )
  }

  # append site name
  df$site <- site

  return(df)
})

site_data <- bind_rows(site_data)
saveRDS(site_data, "data/MODIS_reflectance_data.rds", compress = "xz")

# mutate(
#   ndvi = (sur_refl_b02 - sur_refl_b01)/(sur_refl_b02  + sur_refl_b01 ),
#   evi = 2.5 * (sur_refl_b02  - sur_refl_b01 ) / (sur_refl_b02  + 6 * sur_refl_b01  - 7.5 * sur_refl_b03 + 1),
#   NIRv = ndvi * (sur_refl_b02),
#   cci = (sur_refl_b11  - sur_refl_b01 )/(sur_refl_b11  + sur_refl_b01 ),
#   pri = (sur_refl_b11  - sur_refl_b12 )/(sur_refl_b11  + sur_refl_b12 )
# )
