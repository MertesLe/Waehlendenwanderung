library(dplyr)

source("paths.R", encoding = "UTF-8")

ensure_data_dirs()

validation_output_dir <- file.path(data_dir_validation, "large_unblocked")
dir.create(validation_output_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("lphom", quietly = TRUE)) {
  stop(
    "Das Paket 'lphom' ist nicht installiert. ",
    "Installiere es mit install.packages('lphom'), damit lphom::nslphom() laufen kann."
  )
}

settings <- list(
  n_sim = getOption("waehlendenwanderung.large_validation_n_sim", 50L),
  n_units = getOption("waehlendenwanderung.large_validation_n_units", 3000L),
  seed = getOption("waehlendenwanderung.large_validation_seed", 20260720L),
  iter_max = getOption("waehlendenwanderung.large_validation_iter_max", 10L),
  electorate_min = getOption("waehlendenwanderung.large_validation_electorate_min", 350L),
  electorate_max = getOption("waehlendenwanderung.large_validation_electorate_max", 1800L),
  covariate_year = getOption("waehlendenwanderung.large_validation_covariate_year", 2023L),
  progress_every = getOption("waehlendenwanderung.large_validation_progress_every", 1L)
)

origin_betas <- c(
  intercept = 0.10,
  x_binary = 0.65,
  x_continuous = -0.45
)

transition_betas <- tibble(
  transition = rep(c("A_to_A", "B_to_A"), each = 3),
  term = rep(c("(Intercept)", "x_binary", "x_continuous"), times = 2),
  true_beta = c(
    1.20, -0.55, 0.75,
    -1.10, 0.65, -0.70
  )
)

clamp_probability <- function(x, eps = 1e-6) {
  pmin(pmax(x, eps), 1 - eps)
}

safe_cor <- function(x, y) {
  ok <- stats::complete.cases(x, y)

  if (
    sum(ok) < 2 ||
      stats::sd(x[ok], na.rm = TRUE) < 1e-12 ||
      stats::sd(y[ok], na.rm = TRUE) < 1e-12
  ) {
    return(NA_real_)
  }

  stats::cor(x[ok], y[ok])
}

extract_unit_values <- function(array, from, to) {
  as.numeric(array[from, to, ])
}

make_transition_long <- function(sim_id, unit_data, transition_counts, p_a_to_a, p_b_to_a, fit) {
  prop_units <- fit[["VTM.prop.units"]]
  votes_units <- fit[["VTM.votes.units"]]

  bind_rows(
    tibble(
      sim_id = sim_id,
      unit_id = unit_data$unit_id,
      x_binary = unit_data$x_binary,
      x_continuous = unit_data$x_continuous,
      electorate = unit_data$electorate,
      from = "A",
      to = "A",
      transition = "A_to_A",
      origin_count = unit_data$origin_A,
      destination_count = unit_data$destination_A,
      true_probability = p_a_to_a,
      realized_probability = transition_counts$A_to_A / unit_data$origin_A,
      estimated_probability = extract_unit_values(prop_units, "A", "A"),
      true_transition_count = transition_counts$A_to_A,
      estimated_transition_count = extract_unit_values(votes_units, "A", "A")
    ),
    tibble(
      sim_id = sim_id,
      unit_id = unit_data$unit_id,
      x_binary = unit_data$x_binary,
      x_continuous = unit_data$x_continuous,
      electorate = unit_data$electorate,
      from = "A",
      to = "B",
      transition = "A_to_B",
      origin_count = unit_data$origin_A,
      destination_count = unit_data$destination_B,
      true_probability = 1 - p_a_to_a,
      realized_probability = transition_counts$A_to_B / unit_data$origin_A,
      estimated_probability = extract_unit_values(prop_units, "A", "B"),
      true_transition_count = transition_counts$A_to_B,
      estimated_transition_count = extract_unit_values(votes_units, "A", "B")
    ),
    tibble(
      sim_id = sim_id,
      unit_id = unit_data$unit_id,
      x_binary = unit_data$x_binary,
      x_continuous = unit_data$x_continuous,
      electorate = unit_data$electorate,
      from = "B",
      to = "A",
      transition = "B_to_A",
      origin_count = unit_data$origin_B,
      destination_count = unit_data$destination_A,
      true_probability = p_b_to_a,
      realized_probability = transition_counts$B_to_A / unit_data$origin_B,
      estimated_probability = extract_unit_values(prop_units, "B", "A"),
      true_transition_count = transition_counts$B_to_A,
      estimated_transition_count = extract_unit_values(votes_units, "B", "A")
    ),
    tibble(
      sim_id = sim_id,
      unit_id = unit_data$unit_id,
      x_binary = unit_data$x_binary,
      x_continuous = unit_data$x_continuous,
      electorate = unit_data$electorate,
      from = "B",
      to = "B",
      transition = "B_to_B",
      origin_count = unit_data$origin_B,
      destination_count = unit_data$destination_B,
      true_probability = 1 - p_b_to_a,
      realized_probability = transition_counts$B_to_B / unit_data$origin_B,
      estimated_probability = extract_unit_values(prop_units, "B", "B"),
      true_transition_count = transition_counts$B_to_B,
      estimated_transition_count = extract_unit_values(votes_units, "B", "B")
    )
  ) %>%
    mutate(
      covariate_year = settings$covariate_year,
      error_vs_true_probability = estimated_probability - true_probability,
      error_vs_realized_probability = estimated_probability - realized_probability,
      abs_error_vs_true_probability = abs(error_vs_true_probability),
      abs_error_vs_realized_probability = abs(error_vs_realized_probability),
      failed = FALSE,
      error_message = NA_character_
    )
}

estimate_beta_model <- function(data, probability_col, model_label) {
  bind_rows(lapply(c("A_to_A", "B_to_A"), function(current_transition) {
    model_data <- data %>%
      filter(
        transition == current_transition,
        origin_count > 0,
        !is.na(.data[[probability_col]])
      ) %>%
      mutate(
        logit_probability = stats::qlogis(clamp_probability(.data[[probability_col]]))
      )

    if (
      nrow(model_data) < 5 ||
        stats::sd(model_data$logit_probability, na.rm = TRUE) < 1e-10
    ) {
      return(tibble(
        sim_id = unique(data$sim_id),
        transition = current_transition,
        probability_source = model_label,
        term = c("(Intercept)", "x_binary", "x_continuous"),
        estimate = NA_real_
      ))
    }

    fit <- stats::lm(
      logit_probability ~ x_binary + x_continuous,
      data = model_data,
      weights = origin_count
    )

    tibble(
      sim_id = unique(data$sim_id),
      transition = current_transition,
      probability_source = model_label,
      term = names(stats::coef(fit)),
      estimate = as.numeric(stats::coef(fit))
    )
  }))
}

summarise_local_errors <- function(local_errors) {
  local_errors %>%
    filter(!failed, origin_count > 0) %>%
    group_by(transition) %>%
    summarise(
      n_sim = n_distinct(sim_id),
      n_rows = n(),
      mean_true_probability = mean(true_probability, na.rm = TRUE),
      mean_estimated_probability = mean(estimated_probability, na.rm = TRUE),
      bias_vs_true_probability = mean(error_vs_true_probability, na.rm = TRUE),
      mae_vs_true_probability = mean(abs_error_vs_true_probability, na.rm = TRUE),
      rmse_vs_true_probability = sqrt(mean(error_vs_true_probability^2, na.rm = TRUE)),
      cor_vs_true_probability = safe_cor(estimated_probability, true_probability),
      bias_vs_realized_probability = mean(error_vs_realized_probability, na.rm = TRUE),
      mae_vs_realized_probability = mean(abs_error_vs_realized_probability, na.rm = TRUE),
      rmse_vs_realized_probability = sqrt(mean(error_vs_realized_probability^2, na.rm = TRUE)),
      cor_vs_realized_probability = safe_cor(estimated_probability, realized_probability),
      .groups = "drop"
    )
}

calculate_estimation_error <- function(data) {
  data %>%
    filter(
      !failed,
      !is.na(true_transition_count),
      !is.na(estimated_transition_count)
    ) %>%
    group_by(
      sim_id,
      unit_id
    ) %>%
    summarise(
      total_true_count = sum(true_transition_count, na.rm = TRUE),
      absolute_joint_count_error = sum(
        abs(true_transition_count - estimated_transition_count),
        na.rm = TRUE
      ),
      estimation_error = 100 * 0.5 * absolute_joint_count_error / total_true_count,
      .groups = "drop"
    )
}

summarise_estimation_error <- function(estimation_error) {
  estimation_error %>%
    summarise(
      n_sim = n_distinct(sim_id),
      n_units = n(),
      mean_ei = mean(estimation_error, na.rm = TRUE),
      median_ei = median(estimation_error, na.rm = TRUE),
      p90_ei = as.numeric(stats::quantile(estimation_error, 0.90, na.rm = TRUE)),
      p95_ei = as.numeric(stats::quantile(estimation_error, 0.95, na.rm = TRUE)),
      max_ei = max(estimation_error, na.rm = TRUE),
      mean_total_true_count = mean(total_true_count, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_beta_recovery <- function(beta_estimates) {
  beta_estimates %>%
    left_join(
      transition_betas,
      by = c("transition", "term")
    ) %>%
    mutate(
      beta_error = estimate - true_beta,
      abs_beta_error = abs(beta_error)
    ) %>%
    group_by(probability_source, transition, term) %>%
    summarise(
      n_sim = n_distinct(sim_id),
      true_beta = first(true_beta),
      mean_estimate = mean(estimate, na.rm = TRUE),
      bias = mean(beta_error, na.rm = TRUE),
      mae = mean(abs_beta_error, na.rm = TRUE),
      rmse = sqrt(mean(beta_error^2, na.rm = TRUE)),
      .groups = "drop"
    )
}

run_one_simulation <- function(sim_id) {
  unit_id <- sprintf("large_%04d_unit_%04d", sim_id, seq_len(settings$n_units))
  x_binary <- stats::rbinom(settings$n_units, size = 1, prob = 0.45)
  x_continuous <- as.numeric(scale(stats::rnorm(settings$n_units)))
  electorate <- sample(
    seq.int(settings$electorate_min, settings$electorate_max),
    size = settings$n_units,
    replace = TRUE
  )

  p_origin_a <- stats::plogis(
    origin_betas[["intercept"]] +
      origin_betas[["x_binary"]] * x_binary +
      origin_betas[["x_continuous"]] * x_continuous
  )
  origin_A <- stats::rbinom(settings$n_units, size = electorate, prob = p_origin_a)
  origin_B <- electorate - origin_A

  p_a_to_a <- stats::plogis(
    1.20 - 0.55 * x_binary + 0.75 * x_continuous
  )
  p_b_to_a <- stats::plogis(
    -1.10 + 0.65 * x_binary - 0.70 * x_continuous
  )

  A_to_A <- stats::rbinom(settings$n_units, size = origin_A, prob = p_a_to_a)
  B_to_A <- stats::rbinom(settings$n_units, size = origin_B, prob = p_b_to_a)

  transition_counts <- tibble(
    A_to_A = A_to_A,
    A_to_B = origin_A - A_to_A,
    B_to_A = B_to_A,
    B_to_B = origin_B - B_to_A
  )

  unit_data <- tibble(
    unit_id = unit_id,
    x_binary = x_binary,
    x_continuous = x_continuous,
    electorate = electorate,
    origin_A = origin_A,
    origin_B = origin_B,
    destination_A = transition_counts$A_to_A + transition_counts$B_to_A,
    destination_B = transition_counts$A_to_B + transition_counts$B_to_B
  )

  election_1 <- data.frame(A = unit_data$origin_A, B = unit_data$origin_B)
  election_2 <- data.frame(A = unit_data$destination_A, B = unit_data$destination_B)
  rownames(election_1) <- unit_id
  rownames(election_2) <- unit_id

  fit <- tryCatch(
    lphom::nslphom(
      votes_election1 = election_1,
      votes_election2 = election_2,
      new_and_exit_voters = "regular",
      iter.max = settings$iter_max,
      verbose = FALSE,
      solver = "lp_solve",
      distance.local = "abs"
    ),
    error = function(error) error
  )

  if (inherits(fit, "error")) {
    error_message <- conditionMessage(fit)

    return(list(
      local_errors = tibble(
        sim_id = sim_id,
        failed = TRUE,
        error_message = error_message
      ),
      beta_estimates = tibble(
        sim_id = sim_id,
        failed = TRUE,
        error_message = error_message
      ),
      run_status = tibble(
        sim_id = sim_id,
        failed = TRUE,
        error_message = error_message,
        iter = NA_integer_,
        HETe = NA_real_,
        n_origin_groups = NA_integer_,
        n_destination_groups = NA_integer_
      )
    ))
  }

  local_errors <- make_transition_long(
    sim_id = sim_id,
    unit_data = unit_data,
    transition_counts = transition_counts,
    p_a_to_a = p_a_to_a,
    p_b_to_a = p_b_to_a,
    fit = fit
  )

  beta_estimates <- bind_rows(
    estimate_beta_model(local_errors, "true_probability", "true_probability"),
    estimate_beta_model(local_errors, "realized_probability", "realized_probability"),
    estimate_beta_model(local_errors, "estimated_probability", "estimated_probability")
  ) %>%
    mutate(
      failed = FALSE,
      error_message = NA_character_
    )

  run_status <- tibble(
    sim_id = sim_id,
    failed = FALSE,
    error_message = NA_character_,
    iter = fit[["iter"]],
    HETe = fit[["HETe"]],
    n_origin_groups = dim(fit[["VTM.prop.units"]])[[1]],
    n_destination_groups = dim(fit[["VTM.prop.units"]])[[2]]
  )

  list(
    local_errors = local_errors,
    beta_estimates = beta_estimates,
    run_status = run_status
  )
}

set.seed(settings$seed)

simulation_results <- vector("list", settings$n_sim)

for (sim_id in seq_len(settings$n_sim)) {
  if (
    settings$progress_every > 0 &&
      (sim_id == 1 || sim_id %% settings$progress_every == 0 || sim_id == settings$n_sim)
  ) {
    message("Large-unblocked-Validierung ", sim_id, " von ", settings$n_sim, ".")
  }

  simulation_results[[sim_id]] <- run_one_simulation(sim_id)
  gc()
}

local_errors <- bind_rows(lapply(simulation_results, `[[`, "local_errors"))
beta_estimates <- bind_rows(lapply(simulation_results, `[[`, "beta_estimates"))
run_status <- bind_rows(lapply(simulation_results, `[[`, "run_status"))

settings_table <- tibble(
  parameter = names(settings),
  value = vapply(settings, as.character, character(1))
)

if (all(run_status$failed)) {
  saveRDS(run_status, file.path(validation_output_dir, "vorlaeufig_nslphom_large_unblocked_run_status.rds"))
  saveRDS(settings_table, file.path(validation_output_dir, "vorlaeufig_nslphom_large_unblocked_settings.rds"))

  stop(
    "Alle large-unblocked-Validierungssimulationen sind fehlgeschlagen. ",
    "Details stehen in vorlaeufig_nslphom_large_unblocked_run_status.rds."
  )
}

validation_summary <- summarise_local_errors(local_errors)
estimation_error <- calculate_estimation_error(local_errors)
estimation_error_summary <- summarise_estimation_error(estimation_error)
beta_summary <- summarise_beta_recovery(
  beta_estimates %>%
    filter(!failed)
)

saveRDS(local_errors, file.path(validation_output_dir, "vorlaeufig_nslphom_large_unblocked_local_errors.rds"))
saveRDS(validation_summary, file.path(validation_output_dir, "vorlaeufig_nslphom_large_unblocked_summary.rds"))
saveRDS(estimation_error, file.path(validation_output_dir, "vorlaeufig_nslphom_large_unblocked_ei_unit.rds"))
saveRDS(estimation_error_summary, file.path(validation_output_dir, "vorlaeufig_nslphom_large_unblocked_ei_summary.rds"))
saveRDS(beta_estimates, file.path(validation_output_dir, "vorlaeufig_nslphom_large_unblocked_beta_estimates.rds"))
saveRDS(beta_summary, file.path(validation_output_dir, "vorlaeufig_nslphom_large_unblocked_beta_summary.rds"))
saveRDS(run_status, file.path(validation_output_dir, "vorlaeufig_nslphom_large_unblocked_run_status.rds"))
saveRDS(settings_table, file.path(validation_output_dir, "vorlaeufig_nslphom_large_unblocked_settings.rds"))
saveRDS(transition_betas, file.path(validation_output_dir, "vorlaeufig_nslphom_large_unblocked_true_betas.rds"))

message("Fertig. Ergebnisse gespeichert unter: ", validation_output_dir)
