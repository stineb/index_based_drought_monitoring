# Basic xgboost model with limited
# hyperparameter tuning

# load the ecosystem
library(tidymodels)
library(ranger)
library(vip)
library(dplyr)
library(ggplot2)

# read in precompiled model
model <- readRDS(
  here::here("data/classification_model.rds")
)

p <- model |>
  vip(geom = "point") +
  labs(title = "Random forest variable importance")

print(p)
