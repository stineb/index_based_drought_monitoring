library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
source("R/calc_VI.R")

# read the data
df <- readRDS(here::here("data/machine_learning_training_data.rds"))
vi <- calc_VI(df)
df <- bind_cols(df, vi)

# stray NA in the labelling?
df$is_flue_drought[is.na(df$is_flue_drought)] <- FALSE

df <- df |>
  group_by(site) |>
  do({

    x <- .
    x$idx <- NA

    # find breaks between TRUE 1 and FALSE 0
    breaks <- diff(x$is_flue_drought)

    # if not breaks return original
    # else continue
    if(!all(breaks == 0)){

    # if start position is TRUE
    # set start posiiton as 1
    # else not
    if(x$is_flue_drought[1]){
      start <- c(1, which(breaks == 1) - 20)
    } else {
      start <- which(breaks == 1) - 20
    }

    # bottom out
    start[start < 1] <- 1

    # which breaks transition from TRUE
    # to FALSE and truncate on start
    # length
    end <- which(breaks == -1)
    if(length(end) < length(start)){
      end <- c(end, nrow(x))
    }

    # combine in one location matrix
    # and exclude chunks which are shorter
    # than 7 values (days)
    locs <- cbind(start, end, end - start)
    locs <- locs[locs[,3] > 27,1:2]

    # trap instances when locs is null or 0
    if (!is.null(nrow(locs))){
      if(nrow(locs) > 0){
        for (i in 1:nrow(locs)){
          x$idx[locs[i,1]:locs[i,2]] <- i
        }
      }
    }

    } else {
      x$idx <- NA
    }

    x
  })

# only retain cDD / cGR
df <- df |>
  filter(
    !is.na(idx),
    cluster %in% c("cDD","cGR")
  ) |>
  group_by(site, idx) |>
  mutate(
    n = 1:n()
  ) |>
  filter(
    n < 100
  ) |>
  ungroup()

VI <- "NDVI"

# summary stats across
# clusters and time steps
df2 <- df |>
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
    x = n - 20,
    ymin = flue_qt_25,
    ymax = flue_qt_75
    ),
    fill = "lightblue",
    alpha = 0.2
  ) +
  geom_ribbon(
    aes(
      x = n - 20,
      ymin = flue_qt_10,
      ymax = flue_qt_90
    ),
    fill = "lightblue",
    alpha = 0.2
  ) +
  geom_line(
    aes(
      n - 20,
      flue_median
    ),
    colour = "blue"
  ) +
  geom_ribbon(
    aes(
      x = n - 20,
      ymin = VI_qt_25,
      ymax = VI_qt_75
    ),
    fill = "lightgreen",
    alpha = 0.2
  ) +
  geom_ribbon(
    aes(
      x = n - 20,
      ymin = VI_qt_10,
      ymax = VI_qt_90
    ),
    fill = "lightgreen",
    alpha = 0.2
  ) +
  geom_line(
    aes(
      n - 20,
      VI_median
    ),
    colour = "darkgreen"
  ) +
  theme_minimal() +
  facet_wrap(cluster~.)

print(p)
