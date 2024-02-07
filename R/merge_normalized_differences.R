
#' Merge / calculate difference ratios
#'
#' Calculate normalized difference ratios
#' for all possible "sur_refl" band combinations.
#'
#' This is a naive approach and a more tailored / specific methodology
#' can use only select normalized difference band combinations by specifying
#' which columns to consider in the lapply() routine in this function.
#'
#' Additional improvements can probably be made by using {purrr} in some
#' exotic combination.
#'
#' @param ml_df a machine learning data frame with "sur_refl" prefix column
#'  names apply the band ratios to
#'
#' @return A machine learning data frame with normalized band ratios appended
#' @export

merge_normalized_differences <- function(ml_df){

  # split out surface reflectance values
  sur_refl <- ml_df |>
    dplyr::select(
      starts_with("sur_refl")
    )

  # calculate all normalized difference ratios
  ndr <- lapply(names(sur_refl), function(band){

    # split out reference band
    band_1 <- sur_refl |>
      dplyr::select(
        all_of(band)
      )

    # mutate over all other collumns
    df <- sur_refl |>
      dplyr::select(
        !all_of(band)
      )

    # wrangle all column names
    cols <- gsub("sur_refl_", "", names(df))
    ref_col <- gsub("sur_refl_", "", names(band_1))

    df <- df |>
      dplyr::transmute_all(
        function(x){as.vector(normalized_difference(band_1, x))}
      )

    # add clean column names
    colnames(df) <- paste(ref_col, cols, sep = "_")
    return(df)
  })

  # bind columns in one big tibble
  ndr <- dplyr::bind_cols(ndr)

  # merge with original data
  ml_df <- bind_cols(ml_df, ndr)

  # convert logical to factor (classes git changed during
  # processing)
  ml_df <- ml_df |>
    mutate_if(is.logical, as.factor)
}


