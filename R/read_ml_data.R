#' Read in machine learning dataframe
#'
#' subsets data for spatial scaling or not
#'
#' @param path path to the raw machine learing RDS file
#' @param spatial TRUE or FALSE, restrict output for spatial scaling
#'
#' @return data frame with machine learning features and targets
#' @export

read_ml_data <- function(
    path,
    spatial = FALSE
    ){

  # read in training data
  ml_df <- readRDS(
    path
  ) |>
    dplyr::select(
      -date,
      -year,
      -doy,
      -cluster
    ) |>
    na.omit()

  if (spatial) {
    ml_df <- ml_df |>
      dplyr::select(
        site,
        flue,
        is_flue_drought,
        starts_with("Nadir"),
        starts_with("LST")
      )
  }

  # convert factor
  ml_df$is_flue_drought <- as.factor(ml_df$is_flue_drought)

  return(ml_df)
}
