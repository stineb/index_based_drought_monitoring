#' Creates fLUE drought index values
#'
#' Indexes periods of drought consecutively
#' per site (idx), and provides an a numeric
#' value in days that the drought lasted
#' preceding days are also counted (if not
#' overlapping with a previous drought for
#' comparison purposes)
#'
#' @param df a machine learning data frame
#'
#' @return the same data frame with numbered
#'  drought instances per site, as well as the consecutive
#'  days the drought lasted, as well as the preceding
#'  20 days if no overlap with other droughts is present
#' @export

index_flue <- function(df){

  # stray NA in the labelling?
  df$is_flue_drought[is.na(df$is_flue_drought)] <- FALSE

  # cleaned up align events code
  # more concise
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
        # set start position as -20 spaces
        # from the true start
        if(x$is_flue_drought[1]){
          start <- c(1, which(breaks == 1) - 1)
        } else {
          start <- which(breaks == 1) - 1
        }

        # bottom out at 1 at start of the series
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

      # return the index labels
      # should be a data frame
      # in a do call
      x
    }) |>
    ungroup()

  # for each indexed chunk
  # number the elements
  df <- df |>
    group_by(site, idx) |>
    mutate(
      n = 1:n(),
      n = ifelse(is.na(idx),NA, n)
    ) |>
    ungroup()

  df <- df |>
    group_by(site) |>
    do({

      # grab whole frame
      x <- .

      # find breaks (i.e. chunks)
      # for loops are needed as
      # the whole series needs
      # to be considered
      breaks <- which(x$n == 1) - 1

      # Note: values which are fLUE
      # positive are never counted
      # in the offset (-) index
      for(i in breaks){
        for(j in (i-20):i){
          if(j >= 1 && is.na(x$n[j])){
            x$n[j] <- x$date[j] - x$date[i]
          }
        }
      }

      x
    }) |>
    ungroup()

  # return labelled data frame
  # data is ungrouped to avoid
  # grouping mess
  return(df)
}
