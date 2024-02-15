library(dplyr)

vi <- readr::read_csv("data/spectral-indices-table.csv") |>
  dplyr::filter(
    application_domain == "vegetation",
    #grepl("drought|water", tolower(long_name)),
    short_name != "MNLI",
    !grepl(
      "nexp|sla|slb|gamma|
       epsilon|alpha|
       beta|omega|A|fdelta|
       cexp|G1|RE1|RE2|PAR|lambda*",
           formula)
  )

# read data and rename columns
df <- readRDS("data/machine_learning_training_data.rds") |>
  dplyr::rename(
    R = Nadir_Reflectance_Band1,
    N = Nadir_Reflectance_Band2,
    B = Nadir_Reflectance_Band3,
    G = Nadir_Reflectance_Band4,
    S1 = Nadir_Reflectance_Band6,
    S2 = Nadir_Reflectance_Band7
  ) |>
  dplyr::select(
    site, date, year, doy,
    flue, is_flue_drought, cluster,
    R, N, B, G, S1, S2, LST_Day_1km
  )

# loop over all indices
df_vi <- lapply(1:nrow(vi), function(i){

  # create temporary object
  tmp <- df

  # set constants dynamically
  if(grepl("EVI",vi$short_name[i])){
    tmp$L <- 1
    tmp$C1 <- 6
    tmp$C2 <- 7.5
    tmp$g <- 2.5
  }

  if(grepl("SAVI|SARVI",vi$short_name[i])){
    tmp$L <- 0.5
  }

  # calculate and return index
  tmp |>
    mutate(
      !!vi$short_name[i] := eval(rlang::parse_expr(vi$formula[i]))
    ) |>
    select(
      !!vi$short_name[i]
    )
  }) |>
  bind_cols()

df <- bind_cols(df, df_vi)
