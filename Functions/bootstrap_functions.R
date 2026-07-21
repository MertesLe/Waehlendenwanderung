allocate_stratified_sample_sizes <- function(block_index, sample_size = 2000L) {
  block_sizes <- block_index %>%
    dplyr::count(.data$nslphom_block, name = "n_block") %>%
    dplyr::arrange(.data$nslphom_block)

  sample_size <- as.integer(sample_size)
  if (is.na(sample_size) || sample_size <= 0) {
    stop("sample_size muss eine positive ganze Zahl sein.")
  }

  min_draws <- if (sample_size >= nrow(block_sizes)) rep(1L, nrow(block_sizes)) else rep(0L, nrow(block_sizes))
  remaining <- sample_size - sum(min_draws)

  if (remaining > 0) {
    raw_extra <- remaining * block_sizes$n_block / sum(block_sizes$n_block)
    extra <- floor(raw_extra)
    rest <- remaining - sum(extra)

    if (rest > 0) {
      add_order <- order(raw_extra - extra, decreasing = TRUE)
      extra[add_order[seq_len(rest)]] <- extra[add_order[seq_len(rest)]] + 1L
    }
  } else {
    extra <- rep(0L, nrow(block_sizes))
  }

  block_sizes %>%
    dplyr::mutate(n_draw = min_draws + extra) %>%
    dplyr::select(nslphom_block, n_block, n_draw)
}

draw_stratified_bootstrap_sample <- function(
    input2021,
    input2025,
    block_index,
    iteration,
    sample_size = 2000L,
    seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed + as.integer(iteration))
  }

  sample_sizes <- allocate_stratified_sample_sizes(block_index, sample_size)

  sample_map <- dplyr::bind_rows(lapply(seq_len(nrow(sample_sizes)), function(i) {
    current_block <- sample_sizes$nslphom_block[[i]]
    current_ids <- block_index$agg_schluessel[block_index$nslphom_block == current_block]
    drawn_ids <- sample(current_ids, size = sample_sizes$n_draw[[i]], replace = TRUE)

    tibble::tibble(
      nslphom_block = current_block,
      agg_schluessel_original = drawn_ids
    )
  })) %>%
    dplyr::mutate(
      bootstrap_id = iteration,
      bootstrap_row = dplyr::row_number(),
      boot_agg_schluessel = sprintf("boot%04d_%05d", .data$bootstrap_id, .data$bootstrap_row)
    )

  bootstrap_input2021 <- sample_map %>%
    dplyr::left_join(input2021, by = c("agg_schluessel_original" = "agg_schluessel")) %>%
    dplyr::transmute(
      agg_schluessel = boot_agg_schluessel,
      dplyr::across(dplyr::all_of(setdiff(names(input2021), "agg_schluessel")))
    )

  bootstrap_input2025 <- sample_map %>%
    dplyr::left_join(input2025, by = c("agg_schluessel_original" = "agg_schluessel")) %>%
    dplyr::transmute(
      agg_schluessel = boot_agg_schluessel,
      dplyr::across(dplyr::all_of(setdiff(names(input2025), "agg_schluessel")))
    )

  bootstrap_block_index <- sample_map %>%
    dplyr::transmute(
      agg_schluessel = boot_agg_schluessel,
      nslphom_block = nslphom_block
    )

  list(
    input2021 = bootstrap_input2021,
    input2025 = bootstrap_input2025,
    block_index = bootstrap_block_index,
    sample_map = sample_map,
    sample_sizes = sample_sizes
  )
}

run_bootstrap_iteration <- function(
    iteration,
    input2021,
    input2025,
    block_index,
    struktur,
    sample_size = 2000L,
    seed = 20260721L,
    iter_max = getOption("waehlendenwanderung.bootstrap_nslphom_iter_max", 10L),
    tol = getOption("waehlendenwanderung.bootstrap_nslphom_tol", 1e-5),
    threshold = 0.12,
    covariates = default_struktur_covariates) {
  sample_data <- draw_stratified_bootstrap_sample(
    input2021 = input2021,
    input2025 = input2025,
    block_index = block_index,
    iteration = iteration,
    sample_size = sample_size,
    seed = seed
  )

  block_results <- fit_nslphom_blocks(
    block_index = sample_data$block_index,
    input2021 = sample_data$input2021,
    input2025 = sample_data$input2025,
    iter_max = iter_max,
    tol = tol,
    verbose = FALSE,
    threshold = threshold
  )

  transitions <- dplyr::bind_rows(lapply(block_results, `[[`, "transition_long")) %>%
    dplyr::rename(bootstrap_agg_schluessel = agg_schluessel) %>%
    dplyr::left_join(
      sample_data$sample_map %>%
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

  model_outputs <- make_transition_model_outputs(
    transitions = transitions,
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

  checks <- dplyr::bind_rows(lapply(block_results, `[[`, "checks")) %>%
    dplyr::mutate(
      bootstrap_id = iteration,
      sample_size = sample_size,
      .before = 1
    )

  list(
    beta_draws = beta_draws,
    checks = checks,
    sample_sizes = sample_data$sample_sizes %>%
      dplyr::mutate(bootstrap_id = iteration, .before = 1)
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
    sample_sizes,
    settings,
    output_dir = data_dir_model_bootstrap,
    write_csv = TRUE) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  saveRDS(beta_draws, file.path(output_dir, "vorlaeufig_bootstrap_beta_draws.rds"))
  saveRDS(beta_intervals, file.path(output_dir, "vorlaeufig_bootstrap_beta_intervals.rds"))
  saveRDS(checks, file.path(output_dir, "vorlaeufig_bootstrap_nslphom_checks.rds"))
  saveRDS(sample_sizes, file.path(output_dir, "vorlaeufig_bootstrap_sample_sizes.rds"))
  saveRDS(settings, file.path(output_dir, "vorlaeufig_bootstrap_settings.rds"))

  if (isTRUE(write_csv)) {
    utils::write.csv(beta_draws, file.path(output_dir, "vorlaeufig_bootstrap_beta_draws.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(beta_intervals, file.path(output_dir, "vorlaeufig_bootstrap_beta_intervals.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(checks, file.path(output_dir, "vorlaeufig_bootstrap_nslphom_checks.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(sample_sizes, file.path(output_dir, "vorlaeufig_bootstrap_sample_sizes.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(settings, file.path(output_dir, "vorlaeufig_bootstrap_settings.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }

  invisible(list(
    beta_draws = beta_draws,
    beta_intervals = beta_intervals,
    checks = checks,
    sample_sizes = sample_sizes,
    settings = settings
  ))
}
