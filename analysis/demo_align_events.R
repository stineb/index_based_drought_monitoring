library(readr)
library(ggplot2)
library(dplyr)

# source(paste0(here::here(), "/R/align_events.R"))

# use flue data for demo
flue <- readr::read_csv("data/flue_stocker18nphyt.csv") |>
  dplyr::filter(site == "FR-Pue")

# this data has column is_flue_drought which is a binary classification
# of whether the respective date is a drought or not.
# Drought is defined here by fLUE being below 1 (with a certain threshold, see
# Stocker et al., 2018).

# determine events as consecutive time periods of dates classified as 'flue drought'
# this returns the event start and length index of the time series of the binary
# classification, here is_flue_drought
df_idx_events <- get_consecutive(
  dry = flue$is_flue_drought,
  leng_threshold = 5,
  do_merge = FALSE
  ) %>%
  mutate(date_start = flue$date[.$idx_start],
         date_end = flue$date[.$idx_start + .$len - 1])

# plot flue and events
ggplot() +
  geom_line(
    data = flue |>
      filter(site == "FR-Pue"),
    aes(date, flue),
    color = "tomato"
  ) +
  geom_rect(
    data = df_idx_events,
    aes(xmin = date_start, xmax = date_end, ymin = 0, ymax = 1.2),
    color = NA,
    fill = "black",
    alpha = 0.3
  ) +
  theme_classic() +
  ylim(0,1.2)


# align data by event (function get_consecutive is called again inside align_events() - not nice)
flue_aligned <- align_events(
  df = flue |>
    rename(isevent = is_flue_drought),
  df_isevent = NA,
  dovars = "flue",
  leng_threshold = 5, # require at least five consecutive days to create an 'event'
  before = 5,
  after = 150,
  nbins = 10 # for data normalisation
)

# plot flue data aligned by event (dday is the day into the event, inst is the event number)
ggplot(
  data = flue_aligned$df_dday,
  aes(x = dday, y = flue, group = inst, color = isevent)
  ) +
  geom_line() +
  theme_classic() +
  scale_color_manual(values = c("grey80", "tomato"))


