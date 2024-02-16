library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
source("R/calc_VI.R")
source("R/index_flue.R")

# read the data
df <- readRDS(here::here("data/machine_learning_training_data.rds"))
vi <- calc_VI(df)
df <- bind_cols(df, vi)
df <- index_flue(df)

VI <- "NDVI"

# summary stats across
# clusters and time steps
df2 <- df |>
  filter(
    (n > -20 & n < 100)
  ) |>
  select(
    cluster,
    n,
    flue,
    !!VI
  ) |>
  group_by(cluster, n) |>
  summarize(
    flue_median = median(flue),
    flue_qt_25 = quantile(flue,0.25),
    flue_qt_75 = quantile(flue,0.75),
    flue_qt_10 = quantile(flue,0.10),
    flue_qt_90 = quantile(flue,0.90),

    # VI
    VI_median = median(!!sym(VI)),
    VI_qt_25 = quantile(!!sym(VI), 0.25),
    VI_qt_75 = quantile(!!sym(VI), 0.75),
    VI_qt_10 = quantile(!!sym(VI), 0.10),
    VI_qt_90 = quantile(!!sym(VI), 0.90)
  ) |>
  ungroup()


#---- figure ----

p <- ggplot(df2) +
  geom_ribbon(
    aes(
    x = n,
    ymin = flue_qt_25,
    ymax = flue_qt_75
    ),
    fill = "lightblue",
    alpha = 0.2
  ) +
  geom_ribbon(
    aes(
      x = n,
      ymin = flue_qt_10,
      ymax = flue_qt_90
    ),
    fill = "lightblue",
    alpha = 0.2
  ) +
  geom_line(
    aes(
      n,
      flue_median
    ),
    colour = "blue"
  ) +
  geom_ribbon(
    aes(
      x = n,
      ymin = VI_qt_25,
      ymax = VI_qt_75
    ),
    fill = "lightgreen",
    alpha = 0.2
  ) +
  geom_ribbon(
    aes(
      x = n,
      ymin = VI_qt_10,
      ymax = VI_qt_90
    ),
    fill = "lightgreen",
    alpha = 0.2
  ) +
  geom_line(
    aes(
      n,
      VI_median
    ),
    colour = "darkgreen"
  ) +
  theme_minimal() +
  facet_wrap(cluster~.)

print(p)
