# Download all remote sensing data using
# appeears, takes about 8h to complete

library(appeears)
library(dplyr)

# load fLUE data from previous paper
df <- read.csv("data/flue_stocker18nphyt.csv")

# load site info for downloading data
# only retain relevant site locations
site_info <- read.csv("data/site_info.csv") |>
  filter(
    sitename %in% unique(df$site)
  )

# create a base query / locations + time frame
base_query <- site_info |>
  select(
    sitename,
    lat,
    lon,
    date_start,
    date_end
  ) |>
  rename(
    "task" = "sitename",
    "latitude"= "lat",
    "longitude" = "lon",
    "start" = "date_start",
    "end" = "date_end",
  ) |>
  mutate(
    start = ifelse(start < "2001-01-01", "2001-01-01", start)
  )

# list products to download
product_subset <- c(
  "MOD09GA.061",
  "MODOCGA.006",
  "MOD11A1.006",
  "MCD43A4.061"
  )

# list appeears meta-data and subset
# only the above products
products <- appeears::rs_products() |>
  filter(
    ProductAndVersion %in% product_subset
  )

# loop over all products and create
# basic query data
full_queries <- lapply(
  products$ProductAndVersion,
  function(product){

  # specify the products / bands required
  # grab all non-QA bands
  bands <- rs_layers(product) |>
    filter(
      IsQA == FALSE
    )

  if(product != "MOD11A1.006" ) {
    bands <- bands |>
      filter(
        grepl("refl", Layer, ignore.case = TRUE)
      )
  } else {
    bands <- bands |>
      filter(
        grepl("LST_Day", Layer)
      )
  }

  bands <- bands |>
    select(
      Layer
    ) |>
    unlist()

  base_query <- base_query |>
    rowwise() |>
    do({
      data.frame(
        subtask = product,
        task = .$task,
        latitude = .$latitude,
        longitude = .$longitude,
        start = .$start,
        end = .$end,
        product = product,
        layer = as.character(bands)
      )
    })
})

tasks <- full_queries |>
  bind_rows() |>
  group_by(task, subtask) |>
  group_split()

tasks <- lapply(
  tasks, function(task){
    appeears::rs_build_task(task)
})

# request the task to be executed
# don't download, just return
# the task ID / request calls
requests <- rs_request_batch(
    request = tasks,
    user = "khufkens",
    workers = 10,
    path = "data-raw/modis_data/"
  )

# clean up the files which are not required
# first list all files, then select the
# ones to remove (kick those out)
downloaded_files <- list.files(
  "data-raw/modis_data/",
  "*",
  full.names = TRUE,
  recursive = TRUE
  )

files_to_remove <- downloaded_files[!grepl("results", downloaded_files)]

# remove files
file.remove(files_to_remove)

