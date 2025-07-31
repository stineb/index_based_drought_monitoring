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
      -year,
      -doy,
      -cluster
    ) |>
    dplyr::mutate(
      is_flue_drought = as.factor(is_flue_drought)
    )

  if (spatial) {
    ml_df <- ml_df |>
      dplyr::select(
        site,
        date,
        flue,
        is_flue_drought,
        starts_with("Nadir"),
        starts_with("LST")
      )
  }

  return(ml_df)
}
