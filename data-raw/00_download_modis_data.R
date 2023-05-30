# Download all remote sensing data using
# appeears

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
    lon
  ) |>
  rename(
    "subtask" = "sitename",
    "latitude"= "lat",
    "longitude" = "lon"
  ) |>
  mutate(
    start = "2001-01-01",
    end = "2022-12-31"
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
    ) |>
    select(
      Layer
    ) |>
    unlist()

  base_query <- base_query |>
    rowwise() |>
    do({
      data.frame(
        task = product,
        subtask = .$subtask,
        latitude = .$latitude,
        longitude = .$longitude,
        start = .$start,
        end = .$end,
        product = product,
        layer = as.character(bands)
      )
    }
    )
})

# from the basic query build a set of tasks
tasks <- lapply(full_queries, function(query){
  appeears::rs_build_task(df = query)
})

# request the task to be executed
# don't download, just return
# the task ID / request calls
requests <- lapply(tasks,function(task){
  rs_request(
    request = task,
    user = "khufkens",
    transfer = FALSE,
    path = "data/",
    verbose = TRUE
  )
})

message(
" Downloads might take a while to compile,
 check back manually using rs_list_task()!"
)



