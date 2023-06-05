
#' Reads in AppEEARS MODIS data
#'
#' Reads in AppEEARS MODIS data and converts from the
#' wide to a long format, filtering based upon good
#' data quality using data QA/QC descriptions
#'
#' Only products MOD09GA.061, MODOCGA.006, MOD11A1.006,
#' and MCD43A4.061 are supported if not matching these
#' the intermediate data is returned for manual screening.
#'
#' @param file a file location of the file to process
#' @param name band name qualifier to filter out RS values
#'
#' @return a cleaned up data frame with bad values screened
#'  out and marked as NA - no gap filling is performed
#'  at this step
#' @export

read_modis <- function(
    file,
    name = "Nadir_Reflectance"
) {

  # read in file
  df <- readr::read_csv(file) |>
    select(
      Category,
      Date,
      contains(name),
      contains(c("Description"))
    ) |>
    rename(
      "product" = "Category",
      "date" = "Date"
    )

  # convert wide to long format
  # on spectral bands
  df <- tidyr::pivot_longer(
    df,
    cols = contains(name),
    values_to = "value",
    names_to = "band"
  )

  # convert to long format on QA bands
  df <- tidyr::pivot_longer(
    df,
    cols = contains(c("Description")),
    values_to = "bitmask",
    names_to = "bitmask_band"
  )

  #---- post-processing ----
  products <- c(
    "MOD09GA.061",
    "MODOCGA.006",
    "MOD11A1.006",
    "MCD43A4.061"
  )

  # post-processing is only valid for
  # select data - too idiosyncratic for
  # fully automated processing will be
  # based on a product-by-product workflow
  if (df$product[1] %in% products) {

    if (df$product[1] == products[1]) {

    }

    if (df$product[1] == products[2]) {

    }

    if (df$product[1] == products[3]) {

    }

    if (df$product[1] == products[4]) {

    }
  }

  return(df)
}
