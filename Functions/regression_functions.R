# Funktionen zur Modellierung der geschaetzten Uebergangswahrscheinlichkeiten.

# Kovariatenspalten anhand des Strukturjahres aus einem aufbereiteten Datensatz bestimmen.
get_structure_covariates <- function(data) {
  if (!"struktur_jahr" %in% names(data)) {
    stop("Der Strukturdatensatz enthaelt keine Spalte struktur_jahr.")
  }

  years <- unique(stats::na.omit(data$struktur_jahr))

  if (length(years) != 1) {
    stop("Der Strukturdatensatz muss genau ein eindeutiges struktur_jahr enthalten.")
  }

  suffix <- paste0("_", years[[1]])
  covariates <- names(data)[endsWith(names(data), suffix)]
  covariates <- setdiff(covariates, paste0("gewicht_summe", suffix))

  if (length(covariates) == 0) {
    stop("Im Strukturdatensatz wurden keine Kovariatenspalten fuer ", years[[1]], " gefunden.")
  }

  non_numeric <- covariates[!vapply(data[covariates], is.numeric, logical(1))]

  if (length(non_numeric) > 0) {
    stop("Folgende Strukturkovariaten sind nicht numerisch: ", paste(non_numeric, collapse = ", "))
  }

  covariates
}

# Koeffizienten und Konfidenzinformationen eines linearen Modells als Tabelle ausgeben.
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

# Kovariaten z-standardisieren und dabei die urspruenglichen Zeilen beibehalten.
standardize_covariates <- function(data, covariates) {
  missing_covariates <- setdiff(covariates, names(data))

  if (length(missing_covariates) > 0) {
    stop("Folgende Kovariaten fehlen: ", paste(missing_covariates, collapse = ", "))
  }

  for (covariate in covariates) {
    data[[paste0(covariate, "_z")]] <- as.numeric(scale(data[[covariate]]))
  }

  data
}

# Ein gewichtetes lineares Modell fuer genau einen Uebergang schaetzen.
fit_one_transition_lm <- function(
    data,
    response_col,
    weight_col,
    covariates_z) {
  if (
    nrow(data) < 30 ||
      stats::sd(data[[response_col]], na.rm = TRUE) < 1e-8 ||
      sum(data[[weight_col]], na.rm = TRUE) <= 0
  ) {
    return(NULL)
  }

  # Herkunfts- oder Zielmasse als Fallgewicht verwenden, damit groessere Einheiten staerker eingehen.
  stats::lm(
    stats::reformulate(
      covariates_z,
      response = response_col
    ),
    data = data,
    weights = data[[weight_col]],
    model = TRUE,
    x = TRUE,
    y = TRUE
  )
}

# AfD-Zufluesse nach Herkunftsgruppe deskriptiv zusammenfassen.
make_afd_source_summary <- function(transitions) {
  transitions %>%
    dplyr::filter(.data$to == "AfD", .data$from != "AfD") %>%
    dplyr::group_by(.data$from) %>%
    dplyr::summarise(
      n_agg = dplyr::n(),
      estimated_transition_count = sum(.data$estimated_transition_count, na.rm = TRUE),
      origin_count = sum(.data$origin_count, na.rm = TRUE),
      mean_transition_probability = mean(.data$transition_probability, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      share_of_estimated_afd_zufluss = .data$estimated_transition_count /
        sum(.data$estimated_transition_count, na.rm = TRUE)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$estimated_transition_count))
}

# Lokale AfD-Uebergaenge mit den 2023-Strukturmerkmalen derselben Einheit verbinden.
prepare_afd_model_data <- function(
    transitions,
    struktur,
    covariates) {
  transitions %>%
    dplyr::filter(.data$to == "AfD", .data$from != "AfD") %>%
    dplyr::left_join(struktur, by = "agg_schluessel") %>%
    dplyr::filter(
      dplyr::if_all(dplyr::all_of(covariates), ~ !is.na(.x)),
      .data$origin_count > 0
    ) %>%
    dplyr::mutate(
      afd_inflow_label = paste0(.data$from, "_to_AfD")
    ) %>%
    standardize_covariates(covariates)
}

# Fuer jede vorhandene Herkunftsgruppe ein eigenes Uebergangsmodell schaetzen.
fit_grouped_transition_models <- function(
    model_data,
    response_col,
    weight_col,
    model_target,
    covariates_z) {
  model_data <- model_data %>%
    dplyr::arrange(.data$from, .data$agg_schluessel)

  grouped_data <- split(model_data, model_data$from)
  model_fits <- lapply(
    grouped_data,
    fit_one_transition_lm,
    response_col = response_col,
    weight_col = weight_col,
    covariates_z = covariates_z
  )

  model_coefficients <- dplyr::bind_rows(lapply(names(model_fits), function(origin) {
    fit <- model_fits[[origin]]

    if (is.null(fit)) {
      return(NULL)
    }

    tidy_lm(fit) %>%
      dplyr::mutate(
        from = origin,
        to = unique(grouped_data[[origin]]$to),
        .before = 1
      )
  })) %>%
    dplyr::mutate(model_target = model_target) %>%
    dplyr::arrange(.data$from, .data$to, .data$term)

  list(
    model_fits = model_fits,
    model_coefficients = model_coefficients
  )
}

# Modelldaten, Fits, Koeffizienten und Plausibilitaetschecks gemeinsam erzeugen.
make_transition_model_outputs <- function(
    transitions,
    struktur,
    covariates) {
  covariates_z <- paste0(covariates, "_z")

  afd_source_summary <- make_afd_source_summary(transitions)
  model_data <- prepare_afd_model_data(transitions, struktur, covariates)

  # Fuer jede Herkunftsgruppe erklaeren, welcher Anteil 2025 zur AfD wechselt.
  fitted_models <- fit_grouped_transition_models(
    model_data,
    response_col = "transition_probability",
    weight_col = "origin_count",
    model_target = "origin_to_AfD_probability",
    covariates_z = covariates_z
  )
  model_fits <- fitted_models$model_fits
  model_coefficients <- fitted_models$model_coefficients

  # Beobachtungszahl und Streuung je Herkunft fuer die Modellierbarkeit dokumentieren.
  model_checks <- model_data %>%
    dplyr::group_by(.data$from, .data$to) %>%
    dplyr::summarise(
      n_agg = dplyr::n(),
      sum_origin_count = sum(.data$origin_count, na.rm = TRUE),
      sum_estimated_transition_count = sum(.data$estimated_transition_count, na.rm = TRUE),
      sum_destination_count = sum(.data$destination_count, na.rm = TRUE),
      mean_transition_probability = mean(.data$transition_probability, na.rm = TRUE),
      sd_transition_probability = stats::sd(.data$transition_probability, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    model_data = model_data,
    model_fits = model_fits,
    model_coefficients = model_coefficients,
    model_checks = model_checks,
    afd_source_summary = afd_source_summary
  )
}

# Relevante Regressionsoutputs als RDS-Dateien in den gewaehlten Modellordner schreiben.
write_transition_model_outputs <- function(outputs, output_dir = data_dir_model_regression) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  saveRDS(outputs$model_data, file.path(output_dir, "modell_afd_zufluss_daten.rds"))
  saveRDS(outputs$model_fits, file.path(output_dir, "modell_afd_zufluss_fits.rds"))
  saveRDS(outputs$model_coefficients, file.path(output_dir, "modell_afd_zufluss_coefficients.rds"))
  saveRDS(outputs$model_checks, file.path(output_dir, "modell_afd_zufluss_checks.rds"))
  saveRDS(outputs$afd_source_summary, file.path(output_dir, "modell_afd_source_summary.rds"))

  invisible(outputs)
}
