
#' Normalized difference
#'
#' Calculates normalized difference for two (arbitrary) bands. The difference
#' is taken as band 1 - band 2
#'
#' @param band_1 first band
#' @param band_2 second band
#'
#' @return normalized difference index for two bands
#' @export

normalized_difference <- function(band_1, band_2){
  ndr <- (band_1 - band_2)/(band_1 + band_2)
  as.matrix(ndr)
}
