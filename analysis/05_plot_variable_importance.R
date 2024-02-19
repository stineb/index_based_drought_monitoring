# Basic xgboost model with limited
# hyperparameter tuning

# load the ecosystem
library(tidymodels)
library(ranger)
library(vip)
library(dplyr)
library(ggplot2)
library(patchwork)

# read in precompiled model
model <- readRDS(
  here::here("data/regression_model_spatial.rds")
)

p1 <- model |>
  vip(geom = "point") +
  labs(title = "Regression")

# read in precompiled model
model <- readRDS(
  here::here("data/classification_model_spatial.rds")
)

p2 <- model |>
  vip(geom = "point") +
  labs(title = "Classification")


print(p1|p2)
