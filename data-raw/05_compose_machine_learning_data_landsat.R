# process the Landsat data into a machine learning
# data frame consistent with the one for MODIS

library(tidyverse)

# read landsat data
df <- readRDS("data-raw/landsat7_data.rds") |>
  mutate(
    date = as.Date(date),
    doy = as.numeric(format(date, "%j")),
    year = as.numeric(format(date, "%Y"))
  ) |>
  rename(
    site = sitename
  )

# read in flue data
flue <- readr::read_csv("data/flue_stocker18nphyt.csv")

# merge with flue
df <- left_join(
  flue,
  df
)

df <- df |>
  mutate(
    across(starts_with("SR"),
    function(x){0.0000275* x + -0.2}
    )
  )

df <- df |>
  mutate(
    across(starts_with("ST"),
           function(x){0.00341802* x + -149.0}
    )
  )

# TODO
# Proper QA screening on the bitlevel of the QA_PIXEL data
# There also duplicate acquisitions in the dataset (sort out source
# of this additional data - not sure if this is an artifact of sorts)

df <- df |>
  filter(
    QA_PIXEL == 5440
  ) |>
  select(
    -QA_PIXEL,
    -id,
    -product,
    -latitude,
    -longitude
  ) |>
  na.omit()

# save data
saveRDS(
  df,
  "data/machine_learning_training_data_landsat.rds",
  compress = "xz"
  )

