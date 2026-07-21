default_struktur_covariates <- c(
  "arbeitslosigkeit_2023",
  "auslaenderanteil_2023",
  "kaufkraft_2023"
)

tidy_lm <- function(fit) {
  coefficient_table <- summary(fit)$coefficients

  tibble::tibble(
    term = rownames(coefficient_table),
    estimate = coefficient_table[, "Estimate"],
    std.error = coefficient_table[, "Std. Error"],
    statistic = coefficient_table[, "t value"],
    p.value = coefficient_table[, "Pr(>|t|)"]
  )
}

standardize_covariates <- function(data, covariates = default_struktur_covariates) {
  missing_covariates <- setdiff(covariates, names(data))

  if (length(missing_covariates) > 0) {
    stop("Folgende Kovariaten fehlen: ", paste(missing_covariates, collapse = ", "))
  }

  for (covariate in covariates) {
    data[[paste0(covariate, "_z")]] <- as.numeric(scale(data[[covariate]]))
  }

  data
}

fit_one_transition_lm <- function(
    data,
    response_col,
    weight_col,
    covariates_z = paste0(default_struktur_covariates, "_z")) {
  if (
    nrow(data) < 30 ||
      stats::sd(data[[response_col]], na.rm = TRUE) < 1e-8 ||
      sum(data[[weight_col]], na.rm = TRUE) <= 0
  ) {
    return(tibble::tibble(
      term = character(),
      estimate = numeric(),
      std.error = numeric(),
      statistic = numeric(),
      p.value = numeric()
    ))
  }

  fit <- stats::lm(
    stats::reformulate(
      covariates_z,
      response = response_col
    ),
    data = data,
    weights = data[[weight_col]]
  )

  tidy_lm(fit)
}

make_afd_source_summary <- function(transitions) {
  transitions %>%
    dplyr::filter(.data$to == "AfD") %>%
    dplyr::group_by(.data$from) %>%
    dplyr::summarise(
      n_agg = dplyr::n(),
      estimated_transition_count = sum(.data$estimated_transition_count, na.rm = TRUE),
      origin_count = sum(.data$origin_count, na.rm = TRUE),
      mean_transition_probability = mean(.data$transition_probability, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      source_type = dplyr::if_else(.data$from == "AfD", "AfD_Bestand", "AfD_Zufluss"),
      share_of_estimated_afd_2025 = .data$estimated_transition_count /
        sum(.data$estimated_transition_count, na.rm = TRUE),
      share_of_estimated_afd_zufluss = dplyr::if_else(
        .data$from == "AfD",
        NA_real_,
        .data$estimated_transition_count /
          sum(.data$estimated_transition_count[.data$from != "AfD"], na.rm = TRUE)
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$estimated_transition_count))
}

prepare_afd_model_data <- function(
    transitions,
    struktur,
    covariates = default_struktur_covariates) {
  transitions %>%
    dplyr::filter(.data$to == "AfD", .data$from != "AfD") %>%
    dplyr::left_join(struktur, by = "agg_schluessel") %>%
    dplyr::filter(
      dplyr::if_all(dplyr::all_of(covariates), ~ !is.na(.x)),
      .data$origin_count > 0,
      .data$destination_count > 0
    ) %>%
    dplyr::mutate(
      afd_2025_source_share = .data$estimated_transition_count / .data$destination_count,
      afd_inflow_label = paste0(.data$from, "_to_AfD")
    ) %>%
    standardize_covariates(covariates)
}

fit_grouped_transition_models <- function(
    model_data,
    response_col,
    weight_col,
    model_target,
    covariates_z) {
  model_data %>%
    dplyr::group_by(.data$from, .data$to) %>%
    dplyr::group_modify(~ fit_one_transition_lm(.x, response_col, weight_col, covariates_z)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(model_target = model_target) %>%
    dplyr::arrange(.data$from, .data$to, .data$term)
}

make_transition_model_outputs <- function(
    transitions,
    struktur,
    covariates = default_struktur_covariates) {
  covariates_z <- paste0(covariates, "_z")

  afd_source_summary <- make_afd_source_summary(transitions)
  model_data <- prepare_afd_model_data(transitions, struktur, covariates)

  model_coefficients <- fit_grouped_transition_models(
    model_data,
    response_col = "transition_probability",
    weight_col = "origin_count",
    model_target = "origin_to_AfD_probability",
    covariates_z = covariates_z
  )

  afd_source_share_coefficients <- fit_grouped_transition_models(
    model_data,
    response_col = "afd_2025_source_share",
    weight_col = "destination_count",
    model_target = "share_of_AfD_2025_by_source",
    covariates_z = covariates_z
  )

  model_checks <- model_data %>%
    dplyr::group_by(.data$from, .data$to) %>%
    dplyr::summarise(
      n_agg = dplyr::n(),
      sum_origin_count = sum(.data$origin_count, na.rm = TRUE),
      sum_estimated_transition_count = sum(.data$estimated_transition_count, na.rm = TRUE),
      sum_destination_count = sum(.data$destination_count, na.rm = TRUE),
      mean_transition_probability = mean(.data$transition_probability, na.rm = TRUE),
      mean_afd_2025_source_share = mean(.data$afd_2025_source_share, na.rm = TRUE),
      sd_transition_probability = stats::sd(.data$transition_probability, na.rm = TRUE),
      sd_afd_2025_source_share = stats::sd(.data$afd_2025_source_share, na.rm = TRUE),
      .groups = "drop"
    )

  all_model_coefficients <- dplyr::bind_rows(
    model_coefficients,
    afd_source_share_coefficients
  ) %>%
    dplyr::arrange(.data$model_target, .data$from, .data$to, .data$term)

  legacy_model_data <- transitions %>%
    dplyr::left_join(struktur, by = "agg_schluessel") %>%
    dplyr::filter(
      dplyr::if_all(dplyr::all_of(covariates), ~ !is.na(.x)),
      .data$origin_count > 0
    ) %>%
    standardize_covariates(covariates)

  legacy_model_coefficients <- legacy_model_data %>%
    dplyr::group_by(.data$from, .data$to) %>%
    dplyr::group_modify(~ fit_one_transition_lm(.x, "transition_probability", "origin_count", covariates_z)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$from, .data$to, .data$term)

  legacy_model_checks <- legacy_model_data %>%
    dplyr::group_by(.data$from, .data$to) %>%
    dplyr::summarise(
      n_agg = dplyr::n(),
      sum_origin_count = sum(.data$origin_count, na.rm = TRUE),
      sd_response = stats::sd(.data$transition_probability, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    model_data = model_data,
    model_coefficients = model_coefficients,
    afd_source_share_coefficients = afd_source_share_coefficients,
    all_model_coefficients = all_model_coefficients,
    model_checks = model_checks,
    afd_source_summary = afd_source_summary,
    legacy_model_data = legacy_model_data,
    legacy_model_coefficients = legacy_model_coefficients,
    legacy_model_checks = legacy_model_checks
  )
}

write_transition_model_outputs <- function(outputs, write_csv = TRUE) {
  saveRDS(outputs$model_data, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_zufluss_daten.rds"))
  saveRDS(outputs$model_coefficients, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_zufluss_coefficients.rds"))
  saveRDS(outputs$afd_source_share_coefficients, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_source_share_coefficients.rds"))
  saveRDS(outputs$all_model_coefficients, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_all_coefficients.rds"))
  saveRDS(outputs$model_checks, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_zufluss_checks.rds"))
  saveRDS(outputs$afd_source_summary, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_source_summary.rds"))
  saveRDS(outputs$legacy_model_data, file.path(data_dir_model_regression, "vorlaeufig_modell_daten.rds"))
  saveRDS(outputs$legacy_model_coefficients, file.path(data_dir_model_regression, "vorlaeufig_modell_coefficients.rds"))
  saveRDS(outputs$legacy_model_checks, file.path(data_dir_model_regression, "vorlaeufig_modell_checks.rds"))

  if (isTRUE(write_csv)) {
    utils::write.csv(outputs$model_data, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_zufluss_daten.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(outputs$model_coefficients, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_zufluss_coefficients.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(outputs$afd_source_share_coefficients, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_source_share_coefficients.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(outputs$all_model_coefficients, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_all_coefficients.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(outputs$model_checks, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_zufluss_checks.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(outputs$afd_source_summary, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_source_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(outputs$legacy_model_data, file.path(data_dir_model_regression, "vorlaeufig_modell_daten.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(outputs$legacy_model_coefficients, file.path(data_dir_model_regression, "vorlaeufig_modell_coefficients.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(outputs$legacy_model_checks, file.path(data_dir_model_regression, "vorlaeufig_modell_checks.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }

  invisible(outputs)
}
