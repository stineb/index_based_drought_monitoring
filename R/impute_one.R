# --- Helper function to impute one variable ---
impute_one <- function(target_var, df, impute_predictors) {

  # remove taget variable from set of predictors
  use_impute_predictors <- setdiff(impute_predictors, target_var)

  # Select target + predictors
  d <- df |>
    select(all_of(c(target_var, use_impute_predictors))) |>
    drop_na(all_of(use_impute_predictors))  # drop rows missing predictors

  # Separate complete and incomplete rows
  d_train <- d |>
    filter(!is.na(!!sym(target_var)))

  d_missing <- df |>
    filter(is.na(!!sym(target_var))) |>
    drop_na(use_impute_predictors)

  if (nrow(d_missing) == 0) return(NULL)  # no rows to impute

  # Recipe: target ~ predictors, with normalization
  rec <- recipe(as.formula(paste(target_var, "~ .")), data = d_train) |>

    # Role handling
    update_role(all_of(use_impute_predictors), new_role = "predictor") |>
    step_normalize(all_predictors())

  # Train control + tuning grid
  ctrl <- trainControl(method = "cv", number = 5)
  grid <- expand.grid(k = c(5, 15, 25))

  # Train the KNN model
  model <- train(
    rec,
    data = d_train,
    method = "knn",
    trControl = ctrl,
    tuneGrid = grid
  )

  message("KNN modelling result:")
  print(model)

  # Predict missing values
  preds <- predict(model, newdata = d_missing)

  varnam_filled <- paste0(target_var, "_filled")
  d_missing[[varnam_filled]] <- preds

  # fill missing values
  out <- df |>
    left_join(
      d_missing |>
        select(all_of(c("site", "date", varnam_filled))),
      by = join_by(site, date)
    ) |>
    mutate(!!target_var := ifelse(is.na(!!sym(target_var)), !!sym(varnam_filled), !!sym(target_var)))

  return(out)
}
