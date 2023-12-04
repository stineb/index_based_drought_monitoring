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
      name = "LST_Daykm"
    ) |>
    rename(
        "LST_terra" = "LST_Daykm"
    )
  }

  if(product == "MYD11A1"){
    df <- read_appeears(
      file,
      name = "LST_Daykm"
    ) |>
    rename(
      "LST_aqua" = "LST_Daykm"
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

# save full data set
saveRDS(
  site_data,
  "data/MODIS_reflectance_data.rds",
  compress = "xz"
  )
