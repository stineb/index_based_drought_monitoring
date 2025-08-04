#function to select sites in fluxdatakit files
fdk_read_onesite <- function(site, path){
  filename <- list.files(
    path,
    pattern = paste0("FLX_", site, "_FLUXDATAKIT_FULLSET_DD"),
    full.names = TRUE
  )
  print(filename)
  if (length(filename) == 0) {
    message("No file found for site: ", site)
    return(tibble())  # Return an empty tibble if no file is found
  }
  out <- read_csv(filename) |>
    mutate(sitename = site)
  return(out)
}
