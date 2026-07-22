draw_bootstrap_sample <- function(
    input2021,
    input2025,
    iteration,
    sample_size = 2000L,
    seed = NULL) {
  stopifnot(identical(input2021$agg_schluessel, input2025$agg_schluessel))
  stopifnot(identical(names(input2021), names(input2025)))

  sample_size <- as.integer(sample_size)
  if (is.na(sample_size) || sample_size <= 0) {
    stop("sample_size muss eine positive ganze Zahl sein.")
  }

  if (!is.null(seed)) {
    set.seed(seed + as.integer(iteration))
  }

  drawn_ids <- sample(input2021$agg_schluessel, size = sample_size, replace = TRUE)

  sample_map <- tibble::tibble(
    bootstrap_id = iteration,
    bootstrap_row = seq_len(sample_size),
    boot_agg_schluessel = sprintf("boot%04d_%05d", iteration, seq_len(sample_size)),
    agg_schluessel_original = drawn_ids
  )

  input_groups <- setdiff(names(input2021), "agg_schluessel")

  bootstrap_input2021 <- sample_map %>%
    dplyr::left_join(input2021, by = c("agg_schluessel_original" = "agg_schluessel")) %>%
    dplyr::transmute(
      agg_schluessel = .data$boot_agg_schluessel,
      dplyr::across(dplyr::all_of(input_groups))
    )

  bootstrap_input2025 <- sample_map %>%
    dplyr::left_join(input2025, by = c("agg_schluessel_original" = "agg_schluessel")) %>%
    dplyr::transmute(
      agg_schluessel = .data$boot_agg_schluessel,
      dplyr::across(dplyr::all_of(input_groups))
    )

  if (any(is.na(bootstrap_input2021[input_groups])) || any(is.na(bootstrap_input2025[input_groups]))) {
    stop("Mindestens eine gezogene Bootstrap-Zeile konnte nicht auf Inputdaten gemappt werden.")
  }

  stopifnot(all(rowSums(bootstrap_input2021[input_groups]) == rowSums(bootstrap_input2025[input_groups])))

  sample_summary <- tibble::tibble(
    bootstrap_id = iteration,
    n_population = nrow(input2021),
    n_draw = sample_size,
    n_unique_original = dplyr::n_distinct(sample_map$agg_schluessel_original),
    resampling = "bundesweit mit Zuruecklegen, ohne nslphom-Bloecke"
  )

  list(
    input2021 = bootstrap_input2021,
    input2025 = bootstrap_input2025,
    sample_map = sample_map,
    sample_summary = sample_summary
  )
}

map_bootstrap_transitions_to_original_ids <- function(transitions, sample_map) {
  mapped_transitions <- transitions %>%
    dplyr::rename(bootstrap_agg_schluessel = agg_schluessel) %>%
    dplyr::left_join(
      sample_map %>%
        dplyr::select(
          boot_agg_schluessel,
          agg_schluessel_original
        ) %>%
        dplyr::rename(
          bootstrap_agg_schluessel = boot_agg_schluessel,
          agg_schluessel = agg_schluessel_original
        ),
      by = "bootstrap_agg_schluessel"
    ) %>%
    dplyr::select(
      bootstrap_agg_schluessel,
      agg_schluessel,
      dplyr::everything()
    )

  mapping_check <- mapped_transitions %>%
    dplyr::summarise(
      n_transition_rows = dplyr::n(),
      n_bootstrap_ids = dplyr::n_distinct(.data$bootstrap_agg_schluessel),
      n_original_ids = dplyr::n_distinct(.data$agg_schluessel),
      n_missing_original_id = sum(is.na(.data$agg_schluessel)),
      .groups = "drop"
    )

  if (mapping_check$n_missing_original_id > 0) {
    stop("Mindestens eine Bootstrap-Uebergangsmatrix konnte nicht auf einen originalen agg_schluessel gemappt werden.")
  }

  list(
    transitions = mapped_transitions,
    mapping_check = mapping_check
  )
}

check_bootstrap_regression_mapping <- function(
    transitions,
    struktur,
    covariates = default_struktur_covariates) {
  check_data <- transitions %>%
    dplyr::distinct(.data$bootstrap_agg_schluessel, .data$agg_schluessel) %>%
    dplyr::left_join(
      struktur %>%
        dplyr::select(agg_schluessel, dplyr::all_of(covariates)),
      by = "agg_schluessel"
    )

  mapping_check <- check_data %>%
    dplyr::summarise(
      n_bootstrap_ids = dplyr::n_distinct(.data$bootstrap_agg_schluessel),
      n_original_ids = dplyr::n_distinct(.data$agg_schluessel),
      dplyr::across(
        dplyr::all_of(covariates),
        ~ sum(is.na(.x)),
        .names = "n_missing_{.col}"
      ),
      .groups = "drop"
    )

  missing_covariates <- mapping_check %>%
    dplyr::select(dplyr::starts_with("n_missing_")) %>%
    unlist(use.names = FALSE)

  if (any(missing_covariates > 0)) {
    stop("Mindestens eine Bootstrap-Zeile hat keine vollstaendigen Strukturkovariaten fuer die Regression.")
  }

  mapping_check
}

run_bootstrap_iteration <- function(
    iteration,
    input2021,
    input2025,
    struktur,
    sample_size = 2000L,
    seed = 20260721L,
    iter_max = getOption("waehlendenwanderung.bootstrap_nslphom_iter_max", 10L),
    tol = getOption("waehlendenwanderung.bootstrap_nslphom_tol", 1e-5),
    threshold = 0.12,
    covariates = default_struktur_covariates) {
  sample_data <- draw_bootstrap_sample(
    input2021 = input2021,
    input2025 = input2025,
    iteration = iteration,
    sample_size = sample_size,
    seed = seed
  )

  origin_counts <- make_count_matrix(sample_data$input2021)
  destination_counts <- make_count_matrix(sample_data$input2025)

  fit <- fit_nslphom_model(
    origin_counts,
    destination_counts,
    iter_max = iter_max,
    tol = tol,
    verbose = FALSE,
    method = "lphom::nslphom_bootstrap_unblocked"
  )

  transitions_boot <- local_matrices_to_long(
    fit,
    sample_data$input2021$agg_schluessel,
    method = "lphom::nslphom_bootstrap_unblocked"
  )

  mapped <- map_bootstrap_transitions_to_original_ids(
    transitions = transitions_boot,
    sample_map = sample_data$sample_map
  )

  regression_mapping_check <- check_bootstrap_regression_mapping(
    transitions = mapped$transitions,
    struktur = struktur,
    covariates = covariates
  )

  model_outputs <- make_transition_model_outputs(
    transitions = mapped$transitions,
    struktur = struktur,
    covariates = covariates
  )

  beta_draws <- model_outputs$all_model_coefficients %>%
    dplyr::mutate(
      bootstrap_id = iteration,
      sample_size = sample_size,
      iter_max = iter_max,
      tol = tol,
      .before = 1
    )

  checks <- make_nslphom_checks(
    fit,
    transitions_boot,
    block_id = NA_character_,
    method = "lphom::nslphom_bootstrap_unblocked",
    threshold = threshold,
    blocked = FALSE
  ) %>%
    dplyr::mutate(
      bootstrap_id = iteration,
      sample_size = sample_size,
      .before = 1
    )

  mapping_checks <- dplyr::bind_cols(
    tibble::tibble(bootstrap_id = iteration),
    mapped$mapping_check,
    regression_mapping_check %>%
      dplyr::select(-n_bootstrap_ids, -n_original_ids)
  )

  list(
    beta_draws = beta_draws,
    checks = checks,
    mapping_checks = mapping_checks,
    sample_summary = sample_data$sample_summary
  )
}

summarise_bootstrap_betas <- function(beta_draws) {
  beta_draws %>%
    dplyr::filter(.data$term != "(Intercept)") %>%
    dplyr::group_by(.data$model_target, .data$from, .data$to, .data$term) %>%
    dplyr::summarise(
      n_success = sum(!is.na(.data$estimate)),
      mean_estimate = mean(.data$estimate, na.rm = TRUE),
      median_estimate = stats::median(.data$estimate, na.rm = TRUE),
      sd_estimate = stats::sd(.data$estimate, na.rm = TRUE),
      ci_lower = stats::quantile(.data$estimate, 0.025, na.rm = TRUE),
      ci_upper = stats::quantile(.data$estimate, 0.975, na.rm = TRUE),
      significant_95 = .data$ci_lower > 0 | .data$ci_upper < 0,
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$model_target, .data$from, .data$to, .data$term)
}

plot_bootstrap_beta_distributions <- function(beta_draws) {
  beta_draws %>%
    dplyr::filter(.data$term != "(Intercept)") %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$estimate)) +
    ggplot2::geom_vline(xintercept = 0, color = "grey35", linewidth = 0.3) +
    ggplot2::geom_density(fill = "grey65", color = "grey30", alpha = 0.7) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$model_target, .data$term),
      cols = ggplot2::vars(.data$from),
      scales = "free"
    ) +
    ggplot2::labs(
      title = "Bootstrap-Verteilungen der Regressionskoeffizienten",
      x = "Geschaetzter Beta-Koeffizient",
      y = "Dichte"
    ) +
    ggplot2::theme_minimal()
}

write_bootstrap_outputs <- function(
    beta_draws,
    beta_intervals,
    checks,
    mapping_checks,
    sample_summary,
    settings,
    output_dir = data_dir_model_bootstrap) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  saveRDS(beta_draws, file.path(output_dir, "vorlaeufig_bootstrap_beta_draws.rds"))
  saveRDS(beta_intervals, file.path(output_dir, "vorlaeufig_bootstrap_beta_intervals.rds"))
  saveRDS(checks, file.path(output_dir, "vorlaeufig_bootstrap_nslphom_checks.rds"))
  saveRDS(mapping_checks, file.path(output_dir, "vorlaeufig_bootstrap_mapping_checks.rds"))
  saveRDS(sample_summary, file.path(output_dir, "vorlaeufig_bootstrap_sample_summary.rds"))
  saveRDS(settings, file.path(output_dir, "vorlaeufig_bootstrap_settings.rds"))

  invisible(list(
    beta_draws = beta_draws,
    beta_intervals = beta_intervals,
    checks = checks,
    mapping_checks = mapping_checks,
    sample_summary = sample_summary,
    settings = settings
  ))
}
