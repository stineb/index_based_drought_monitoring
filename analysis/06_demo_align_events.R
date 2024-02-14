library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)


df <- readRDS(here::here("data/machine_learning_training_data.rds"))
#
# df <- df |>
#   dplyr::mutate(
#     N = Nadir_Reflectance_Band2,
#     S1 = Nadir_Reflectance_Band6,
#     S2 = Nadir_Reflectance_Band7,
#     swir_diff = S1 - S2,
#     NBR = (S1-S2)/(S1+S2),
#     NMDI = (N-(S1-S2))/(N+(S1-S2)), # normalized multiband drought index
#     GVMI = ((N+0.1)-(S2+0.02))/((N+0.1)+(S2+0.02)) # global vegetation moisture index
#   ) |>
#   dplyr::select(
#     is_flue_drought,
#     flue,
#     GVMI,
#     NMDI,
#     evi,
#     site
#   )

df$is_flue_drought[is.na(df$is_flue_drought)] <- FALSE
df <- df |>
  select(
    site,
    is_flue_drought
  )

bla <- df |>
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
      start <- c(1, which(breaks == 1))
    } else {
      start <- which(breaks == 1)
    }

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
    locs <- locs[locs[,3] > 7,1:2]
    print(locs)

    if (nrow(locs) > 0){
      for (i in 1:nrow(locs)){
        x$idx[locs[i,1]:locs[i,2]] <- i
      }
    }

    } else {
      x$idx <- NA
    }

    x

  })

