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

  # split out product name
  product <- stringr::str_split(basename(file), "-")[[1]][3]
  message(sprintf("---- Processing site %s and product %s", site, product))

  if(product == "MCD43A4"){
    df <- read_appeears(
      file,
      name = "Nadir_Reflectance"
    )
  }

  if (grepl("11A1", product)) {
    df <- read_appeears(
      file,
      name = "LST_Day_1km"
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

# combine all sites
site_data <- bind_rows(site_data)

# rename band (only retain band information
# no product prefix)
site_data$band <- unlist(
  lapply(
    stringr::str_split(site_data$band,"_"),
    function(x){paste(x[3:length(x)], collapse = "_")
    }
  )
)

# rename bands ending with _1
site_data <- site_data |>
  mutate(
    # note the ends with $ character
    band = gsub("_1$", "",  band)
  )

# kick out the surface reflectance values
# might be of value later so I won't
# alter the above routines
site_data <- site_data |>
  filter(
    !grepl("sur_refl*", band)
  )

# save full data set
saveRDS(
  site_data,
  "data/MODIS_reflectance_data.rds",
  compress = "xz"
  )
