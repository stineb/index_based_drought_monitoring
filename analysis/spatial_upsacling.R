# Basic xgboost model with limited
# hyperparameter tuning

# load the ecosystem
library(tidymodels)
library(ranger)
library(dplyr)
library(terra)
set.seed(0)

files <- data.frame(
  file = list.files("data-raw/modis_data_spatial/","*.tif", full.names = TRUE)
)

files <- files |>
  dplyr::filter(
    grepl("doy2018210",file)
  )

r <- terra::rast(files$file)


# the model only works when variable names
# are consistent we therefore rename them
band_names <- data.frame(
  name = names(r)
) |>
  mutate(
    date = as.Date(substr(name, 40, 46), format = "%Y%j"),
    name = substr(name, 13, 35)
  )

# reassign the names of the terra image stack
names(r) <- band_names$name

# read in precompiled model
classification_model <- readRDS(
  here::here("data/classification_model.rds")
)

# return probabilities, where each class is
# associated with a layer in an image stack
# and the probabilities reflect the probabilities
# of the classification for said layer
probabilities <- terra::predict(
  r,
  classification_model,
  type = "prob"
)

plot(probabilities$.pred_TRUE > 0.8)
