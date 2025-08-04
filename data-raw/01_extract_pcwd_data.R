

#read pcwd file
pcwd <- readRDS("/media/yousra/T7/pwcd/ERA5Land_pcwd_LON_+001.875.rds")

#data inspection
head(pcwd)
str(pcwd)
list.files("/media/yousra/T7/pwcd")
str(pcwd, 1)
# list.files("/media/yousra/T7/pwcd")

#load packacges
library(dplyr)
library(purrr)
library(lubridate)
library(tidyverse)

#function that look at all the rds files and choose the closest lon to the site longitude in "site" file
#coarse resolution of pcwd data >> rounded lon and lat to the closest values of sites locations

extract_for_site <- function(site_row, pcwd_dir) {
  # lon_round <- round(site_row$longitude / 1.875) * 1.875
  # lon_str <- sprintf("%+08.3f", lon_round)
  lon_round <- round(site_row$longitude / 1.875) * 1.875
  if (abs(lon_round) < 1e-6) lon_round <- 0  # Avoid -0
  lon_str <- sprintf("%+08.3f", lon_round)
  file_path <- file.path(pcwd_dir, paste0("ERA5Land_pcwd_LON_", lon_str, ".rds"))

  if (!file.exists(file_path)) {
    warning(paste("File not found for site", site_row$site, ":", basename(file_path)))
    return(NULL)
  }

  pcwd_tbl <- tryCatch(readRDS(file_path), error = function(e) {
    warning(paste("Could not read RDS for", site_row$site))
    return(NULL)
  })

  if (!"lat" %in% names(pcwd_tbl) || !"data" %in% names(pcwd_tbl)) {
    warning(paste("Bad file format for", basename(file_path)))
    return(NULL)
  }

  # Find the closest latitude
  closest_row <- pcwd_tbl[which.min(abs(pcwd_tbl$lat - site_row$latitude)), ]
  ts_data_raw <- closest_row$data[[1]]

  if (!is.list(ts_data_raw) || !"df" %in% names(ts_data_raw)) {
    warning(paste("No 'df' found in data for", site_row$site))
    return(NULL)
  }

  ts_data <- ts_data_raw$df

  if (!all(c("date", "deficit") %in% names(ts_data))) {
    warning(paste("Missing 'deficit' or 'date' in ts_data for", site_row$site))
    return(NULL)
  }

  # Filter by start and end dates
  ts_data <- ts_data %>%
    dplyr::filter(date >= site_row$start, date <= site_row$end) %>%
    dplyr::select(date, deficit) %>%
    dplyr::mutate(site = site_row$site)

  return(ts_data)
}


#apply function
extracted_deficit <- purrr::map_dfr(1:nrow(site), ~ extract_for_site(site[.x, , drop = FALSE], pcwd_dir))

head(extracted_deficit)

#this function version store also rounded lon and lat

extract_for_site <- function(site_row, pcwd_dir) {
  # Round coordinates
  lon_round <- round(site_row$longitude / 1.875) * 1.875
  lat_round <- round(site_row$latitude / 1.875) * 1.875
  if (abs(lon_round) < 1e-6) lon_round <- 0

  lon_str <- sprintf("%+08.3f", lon_round)
  file_path <- file.path(pcwd_dir, paste0("ERA5Land_pcwd_LON_", lon_str, ".rds"))

  if (!file.exists(file_path)) {
    warning(glue::glue("File not found for site {site_row$site} : {basename(file_path)}"))
    return(NULL)
  }

  pcwd_data <- readRDS(file_path)
  lat_idx <- which.min(abs(pcwd_data$lat - lat_round))
  if (length(lat_idx) == 0 || is.null(pcwd_data$data[[lat_idx]])) {
    warning(glue::glue("No matching latitude for site {site_row$site}"))
    return(NULL)
  }

  # Filter the date range
  df <- pcwd_data$data[[lat_idx]]$df
  filtered_df <- df |>
    dplyr::filter(date >= site_row$start, date <= site_row$end) |>
    dplyr::select(date, deficit) |>
    dplyr::mutate(
      site = site_row$site,
      rounded_lon = lon_round,
      rounded_lat = lat_round
    )

  return(filtered_df)
}

# pcwd_dir <- "/media/yousra/T7/pwcd"
# extracted_pcwd <- extract_pcwd_for_sites(site, pcwd_dir)
#
# head(extracted_deficit)
#
# head(site)
# unique(extracted_pcwd$site)
# list.files("/media/yousra/T7/pwcd")

#save file

write.csv(extracted_deficit1, "/home/yousra/index_based_physiological_drought/data/era5_pcwd_sites.csv", row.names = FALSE)


