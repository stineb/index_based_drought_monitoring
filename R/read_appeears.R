
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

read_appeears <- function(
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
      message("processing MOD09GA")

      # only retain true QA/QC data
      df <- df |>
        filter(
          grepl("bit_range", bitmask_band)
        )

      # convert bands to numbers for
      # filtering
      df <- df |>
        rowwise() |>
        mutate(
          band_nr =
            as.numeric(gsub("b","",nth(unlist(str_split(band, "_")),5))),
          bitmask_band_nr =
            as.numeric(nth(unlist(str_split(bitmask_band, "_")),7))
        ) |>
        filter(
          band_nr == bitmask_band_nr
        ) |>
        select(
          -bitmask_band_nr
        ) |>
        ungroup()

      df <- df |>
        mutate(
          value = ifelse(bitmask == "highest quality", value, NA)
        )
    }

    if (df$product[1] == products[2]) {
      message("processing MODOCGA")

      # convert bands to numbers for
      # filtering
      df <- df |>
        rowwise() |>
        mutate(
          band_nr =
            as.numeric(gsub("b","",nth(unlist(str_split(band, "_")),5))),
          bitmask_band_nr =
            as.numeric(nth(unlist(str_split(bitmask_band, "_")),10))
        ) |>
        filter(
          band_nr == bitmask_band_nr
        ) |>
        select(
          -bitmask_band_nr
        ) |>
        ungroup()

      df <- df |>
        mutate(
          value = ifelse(bitmask == "highest quality", value, NA)
        )
    }

    if (df$product[1] == products[3]) {
      message("processing MOD11A1")

      # only retain true QA/QC data
      df <- df |>
        filter(
          #grepl("LST_Error", bitmask_band),
          grepl("MODLAND", bitmask_band)
        )

      # convert bands to numbers for
      # filtering
      df <- df |>
        rowwise() |>
        mutate(
          cloud = grepl("not produced", bitmask)
        ) |>
        ungroup()

      df <- df |>
        mutate(
          value = ifelse(!cloud, value, NA)
        )
    }

    if (df$product[1] == products[4]) {
      message("processing MCD43A4")

      # only retain true QA/QC data
      df <- df |>
        filter(
          grepl("MODLAND", bitmask_band)
        )

      # convert bands to numbers for
      # filtering
      df <- df |>
        rowwise() |>
        mutate(
          band_nr =
            as.numeric(gsub("Band","",nth(unlist(str_split(band, "_")),5))),
          bitmask_band_nr =
            as.numeric(
              gsub("Band","",nth(unlist(str_split(bitmask_band, "_")),8)))
        ) |>
        filter(
          band_nr == bitmask_band_nr
        ) |>
        select(
          -bitmask_band_nr
        ) |>
        ungroup()

      # only retain best quality data
      df <- df |>
        mutate(
          value = ifelse(
            grepl("good quality",
                 bitmask),
            value, NA)
        )
    }
  }

  return(df)
}
