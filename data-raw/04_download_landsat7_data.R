# Download Landsat 7 data to validate
# the approach if possible with independent
# remote sensing data - with the
# caveat that the frequency is temporally
# less dense

# change this depending on system settings
python_path = "/usr/local/bin/python"

library(dplyr)

# site info
sites <- readr::read_csv("data/flue_stocker18nphyt.csv") |>
  select(site) |>
  unique()
info <- readr::read_csv("data/site_info.csv") |>
  filter(
    sitename %in% !!sites$site
    ) |>
  select(
    sitename,
    lat,
    lon,
    date_start,
    date_end
  )

# clone the gee_subset project
# relies on git being installed
# and will work out of the box for most
# on OSX or Linux.

# basic gee_subset requirements apply (working GEE install)
setwd(here::here("src"))
system("git clone https://github.com/khufkens/google_earth_engine_subsets.git")
path = here::here("src/google_earth_engine_subsets/gee_subset/")

# set product parameters, such as
# product name, band(s) to query, start and end date of the range
# and the lcoation
product = 'LANDSAT/LE07/C02/T1_L2'
band = "SR_B1 SR_B2 SR_B3 SR_B4 SR_B5 ST_B6 SR_B7 QA_PIXEL"

info |>
  group_by(sitename) |>
  do({

    x <- .

    # location
    location = sprintf("%s	%s", x$lat, x$lon)

    # store output in the R temporary directory
    directory = tempdir()

    # make the gee_subset.py python call
    # time the duration of the call for reporting
    system(
      sprintf(
        "%s %s/gee_subset.py -p %s -b %s -s %s -e %s -l %s -d %s -sc 30",
        python_path,
        path,
        product,
        band,
        x$date_start,
        x$date_end,
        location,
        directory
      ),
      wait = TRUE
    )

    # read in the data stored in the temporary directory
    df <- read.table(
      paste0(
        directory, "/site_",
        tail( unlist( strsplit( product, "[/]" ) ), n=1 ), "_gee_subset.csv" ),
      sep = ",",
      header = TRUE,
      stringsAsFactors = FALSE
    )

   df
  })
