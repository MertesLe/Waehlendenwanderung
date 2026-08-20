# Finale Methodenvalidierung: 3-Parteien-Simulation mit nslphom_dual und OSQP.

library(dplyr)
library(tidyr)

source("paths.R", encoding = "UTF-8")
source("Functions/nslphom_functions.R", encoding = "UTF-8")
source("Functions/regression_functions.R", encoding = "UTF-8")

ensure_data_dirs()
check_lphom_available()
check_osqp_available()

validation_output_dir <- file.path(data_dir_validation, "final_method_dual_osqp")
dir.create(validation_output_dir, recursive = TRUE, showWarnings = FALSE)

settings <- list(
  n_sim = getOption("waehlendenwanderung.final_validation_n_sim", 100L),
  n_units = getOption("waehlendenwanderung.final_validation_n_units", 120L),
  seed = getOption("waehlendenwanderung.final_validation_seed", 20260820L),
  iter_max = getOption("waehlendenwanderung.final_validation_iter_max", getOption("waehlendenwanderung.nslphom_iter_max", 10L)),
  tol = getOption("waehlendenwanderung.final_validation_tol", getOption("waehlendenwanderung.nslphom_tol", 1e-5)),
  electorate_min = getOption("waehlendenwanderung.final_validation_electorate_min", 500L),
  electorate_max = getOption("waehlendenwanderung.final_validation_electorate_max", 2500L),
  progress_every = getOption("waehlendenwanderung.final_validation_progress_every", 10L)
)

parties <- c("Union", "SPD", "AfD")
covariates <- c("x_binary_2023", "x_continuous_2023")

# Bekannte lineare Modelle fuer Zufluesse zur AfD. Genau diese Betas sollen die
# spaetere lineare Regression aus den geschaetzten Matrizen wiederfinden.
true_afd_betas <- tibble::tribble(
  ~from,    ~term,                ~true_beta,
  "Union", "(Intercept)",          0.10,
  "Union", "x_binary_2023_z",      0.06,
  "Union", "x_continuous_2023_z",  0.04,
  "SPD",   "(Intercept)",          0.08,
  "SPD",   "x_binary_2023_z",      0.05,
  "SPD",   "x_continuous_2023_z", -0.03
)

# Zeilenweise Multinomialwahrscheinlichkeiten fuer das 2021-Ergebnis erzeugen.
softmax_rows <- function(eta) {
  shifted <- eta - apply(eta, 1, max)
  exp_eta <- exp(shifted)
  exp_eta / rowSums(exp_eta)
}

# Wahrscheinlichkeiten vom Rand 0/1 fernhalten, damit alle Uebergaenge modellierbar bleiben.
clamp_probability <- function(x, lower = 0.01, upper = 0.85) {
  pmin(pmax(x, lower), upper)
}

# Einen Wert aus dem Beta-Table holen.
beta_value <- function(from, term) {
  true_afd_betas %>%
    filter(.data$from == from, .data$term == term) %>%
    pull(.data$true_beta)
}

# Eine Simulationswahl mit bekannten lokalen Uebergangsmatrizen erzeugen.
simulate_three_party_election <- function(sim_id) {
  unit_id <- sprintf("sim_%04d_unit_%03d", sim_id, seq_len(settings$n_units))
  x_binary <- stats::rbinom(settings$n_units, size = 1, prob = 0.45)
  x_continuous <- as.numeric(scale(stats::rnorm(settings$n_units)))
  x_binary_z <- as.numeric(scale(x_binary))
  x_continuous_z <- as.numeric(scale(x_continuous))
  electorate <- sample(
    seq.int(settings$electorate_min, settings$electorate_max),
    size = settings$n_units,
    replace = TRUE
  )

  origin_prob <- softmax_rows(cbind(
    Union = 0.40 + 0.25 * x_binary_z - 0.15 * x_continuous_z,
    SPD = 0.25 - 0.20 * x_binary_z + 0.20 * x_continuous_z,
    AfD = 0
  ))

  origin_counts <- t(vapply(
    seq_len(settings$n_units),
    function(i) as.integer(stats::rmultinom(1, size = electorate[[i]], prob = origin_prob[i, ])),
    integer(length(parties))
  ))
  colnames(origin_counts) <- parties
  rownames(origin_counts) <- unit_id

  p_union_to_afd <- clamp_probability(
    beta_value("Union", "(Intercept)") +
      beta_value("Union", "x_binary_2023_z") * x_binary_z +
      beta_value("Union", "x_continuous_2023_z") * x_continuous_z
  )
  p_spd_to_afd <- clamp_probability(
    beta_value("SPD", "(Intercept)") +
      beta_value("SPD", "x_binary_2023_z") * x_binary_z +
      beta_value("SPD", "x_continuous_2023_z") * x_continuous_z
  )
  p_afd_to_afd <- clamp_probability(0.76 + 0.02 * x_binary_z + 0.02 * x_continuous_z)

  transition_prob <- array(
    0,
    dim = c(settings$n_units, length(parties), length(parties)),
    dimnames = list(unit_id, parties, parties)
  )

  # Restwahrscheinlichkeiten werden fest auf die Nicht-AfD-Ziele verteilt.
  transition_prob[, "Union", "AfD"] <- p_union_to_afd
  transition_prob[, "Union", "Union"] <- 0.82 * (1 - p_union_to_afd)
  transition_prob[, "Union", "SPD"] <- 0.18 * (1 - p_union_to_afd)

  transition_prob[, "SPD", "AfD"] <- p_spd_to_afd
  transition_prob[, "SPD", "Union"] <- 0.22 * (1 - p_spd_to_afd)
  transition_prob[, "SPD", "SPD"] <- 0.78 * (1 - p_spd_to_afd)

  transition_prob[, "AfD", "AfD"] <- p_afd_to_afd
  transition_prob[, "AfD", "Union"] <- 0.55 * (1 - p_afd_to_afd)
  transition_prob[, "AfD", "SPD"] <- 0.45 * (1 - p_afd_to_afd)

  transition_counts <- array(
    0L,
    dim = c(settings$n_units, length(parties), length(parties)),
    dimnames = list(unit_id, parties, parties)
  )

  for (i in seq_len(settings$n_units)) {
    for (from_party in parties) {
      transition_counts[i, from_party, ] <- as.integer(stats::rmultinom(
        1,
        size = origin_counts[i, from_party],
        prob = transition_prob[i, from_party, ]
      ))
    }
  }

  destination_counts <- t(vapply(
    seq_len(settings$n_units),
    function(i) colSums(transition_counts[i, , ]),
    numeric(length(parties))
  ))
  colnames(destination_counts) <- parties
  rownames(destination_counts) <- unit_id

  struktur <- tibble::tibble(
    agg_schluessel = unit_id,
    struktur_jahr = 2023L,
    x_binary_2023 = x_binary,
    x_continuous_2023 = x_continuous
  )

  list(
    unit_id = unit_id,
    origin_counts = origin_counts,
    destination_counts = destination_counts,
    transition_prob = transition_prob,
    transition_counts = transition_counts,
    struktur = struktur
  )
}

# Wahre, realisierte und geschaetzte lokale Matrizen in Zellform vergleichen.
make_local_error_table <- function(sim_id, simulated, fit, estimated_long) {
  true_cells <- bind_rows(lapply(parties, function(from_party) {
    bind_rows(lapply(parties, function(to_party) {
      origin_count <- simulated$origin_counts[, from_party]
      true_count <- simulated$transition_counts[, from_party, to_party]

      tibble::tibble(
        sim_id = sim_id,
        agg_schluessel = simulated$unit_id,
        from = from_party,
        to = to_party,
        true_probability = simulated$transition_prob[, from_party, to_party],
        realized_probability = true_count / origin_count,
        true_transition_count = true_count
      )
    }))
  }))

  true_cells %>%
    left_join(
      estimated_long %>%
        select(
          agg_schluessel,
          from,
          to,
          origin_count,
          destination_count,
          estimated_probability = transition_probability,
          estimated_transition_count
        ),
      by = c("agg_schluessel", "from", "to")
    ) %>%
    mutate(
      error_vs_true_probability = estimated_probability - true_probability,
      error_vs_realized_probability = estimated_probability - realized_probability,
      abs_error_vs_true_probability = abs(error_vs_true_probability),
      abs_error_vs_realized_probability = abs(error_vs_realized_probability)
    )
}

# EI auf der gemeinsamen Verteilung je Einheit berechnen.
calculate_ei <- function(local_errors) {
  local_errors %>%
    group_by(.data$sim_id, .data$agg_schluessel) %>%
    summarise(
      total_true_count = sum(.data$true_transition_count, na.rm = TRUE),
      absolute_joint_count_error = sum(
        abs(.data$true_transition_count - .data$estimated_transition_count),
        na.rm = TRUE
      ),
      estimation_error = 100 * 0.5 * absolute_joint_count_error / total_true_count,
      .groups = "drop"
    )
}

# Lineare Regression wie im Hauptlauf auf wahre, realisierte oder geschaetzte Matrizen anwenden.
fit_validation_regression <- function(local_errors, simulated, probability_col, model_label) {
  transitions <- local_errors %>%
    transmute(
      agg_schluessel,
      from,
      to,
      transition_probability = .data[[probability_col]],
      origin_count,
      destination_count,
      estimated_transition_count = .data[[probability_col]] * origin_count,
      method = model_label
    )

  make_transition_model_outputs(
    transitions = transitions,
    struktur = simulated$struktur,
    covariates = covariates
  )$model_coefficients %>%
    mutate(model_input = model_label)
}

# Eine komplette Simulation schaetzen und alle Validierungsdaten zurueckgeben.
run_one_final_validation <- function(sim_id) {
  simulated <- simulate_three_party_election(sim_id)

  fit <- tryCatch(
    fit_nslphom_dual_model(
      origin_counts = simulated$origin_counts,
      destination_counts = simulated$destination_counts,
      iter_max = settings$iter_max,
      tol = settings$tol,
      solver = "osqp",
      verbose = FALSE,
      method = "validation_final_nslphom_dual_osqp"
    ),
    error = function(error) error
  )

  if (inherits(fit, "error")) {
    return(list(
      local_errors = tibble::tibble(),
      beta_estimates = tibble::tibble(),
      run_status = tibble::tibble(
        sim_id = sim_id,
        failed = TRUE,
        error_message = conditionMessage(fit)
      )
    ))
  }

  estimated_long <- local_matrices_to_long(
    fit,
    ids = simulated$unit_id,
    method = "validation_final_nslphom_dual_osqp"
  )

  local_errors <- make_local_error_table(
    sim_id = sim_id,
    simulated = simulated,
    fit = fit,
    estimated_long = estimated_long
  )

  beta_estimates <- bind_rows(
    fit_validation_regression(local_errors, simulated, "true_probability", "true_probability"),
    fit_validation_regression(local_errors, simulated, "realized_probability", "realized_probability"),
    fit_validation_regression(local_errors, simulated, "estimated_probability", "estimated_probability")
  ) %>%
    mutate(sim_id = sim_id, .before = 1)

  list(
    local_errors = local_errors,
    beta_estimates = beta_estimates,
    run_status = tibble::tibble(
      sim_id = sim_id,
      failed = FALSE,
      error_message = NA_character_
    )
  )
}

set.seed(settings$seed)
simulation_results <- vector("list", settings$n_sim)

for (sim_id in seq_len(settings$n_sim)) {
  if (sim_id == 1 || sim_id %% settings$progress_every == 0 || sim_id == settings$n_sim) {
    message("Finale Methodenvalidierung ", sim_id, " von ", settings$n_sim, ".")
  }

  simulation_results[[sim_id]] <- run_one_final_validation(sim_id)
}

local_errors <- bind_rows(lapply(simulation_results, `[[`, "local_errors"))
beta_estimates <- bind_rows(lapply(simulation_results, `[[`, "beta_estimates"))
run_status <- bind_rows(lapply(simulation_results, `[[`, "run_status"))

if (nrow(local_errors) == 0L) {
  saveRDS(run_status, file.path(validation_output_dir, "vorlaeufig_final_validation_run_status.rds"))
  stop("Alle finalen Validierungssimulationen sind fehlgeschlagen.")
}

ei_unit <- calculate_ei(local_errors)
ei_summary <- ei_unit %>%
  summarise(
    n_sim = n_distinct(.data$sim_id),
    n_units = n(),
    mean_ei = mean(.data$estimation_error, na.rm = TRUE),
    median_ei = median(.data$estimation_error, na.rm = TRUE),
    p90_ei = as.numeric(stats::quantile(.data$estimation_error, 0.90, na.rm = TRUE)),
    p95_ei = as.numeric(stats::quantile(.data$estimation_error, 0.95, na.rm = TRUE)),
    max_ei = max(.data$estimation_error, na.rm = TRUE)
  )

validation_summary <- local_errors %>%
  group_by(.data$from, .data$to) %>%
  summarise(
    n_sim = n_distinct(.data$sim_id),
    n_cells = n(),
    mean_true_probability = mean(.data$true_probability, na.rm = TRUE),
    mean_estimated_probability = mean(.data$estimated_probability, na.rm = TRUE),
    bias_vs_true_probability = mean(.data$error_vs_true_probability, na.rm = TRUE),
    mae_vs_true_probability = mean(.data$abs_error_vs_true_probability, na.rm = TRUE),
    rmse_vs_true_probability = sqrt(mean(.data$error_vs_true_probability^2, na.rm = TRUE)),
    .groups = "drop"
  )

beta_summary <- beta_estimates %>%
  filter(
    .data$model_target == "origin_to_AfD_probability",
    .data$to == "AfD",
    .data$from %in% c("Union", "SPD")
  ) %>%
  left_join(true_afd_betas, by = c("from", "term")) %>%
  mutate(
    beta_error = .data$estimate - .data$true_beta,
    abs_beta_error = abs(.data$beta_error)
  ) %>%
  group_by(.data$model_input, .data$from, .data$term) %>%
  summarise(
    n_success = sum(!is.na(.data$estimate)),
    true_beta = first(.data$true_beta),
    mean_estimate = mean(.data$estimate, na.rm = TRUE),
    median_estimate = median(.data$estimate, na.rm = TRUE),
    bias = mean(.data$beta_error, na.rm = TRUE),
    mae = mean(.data$abs_beta_error, na.rm = TRUE),
    rmse = sqrt(mean(.data$beta_error^2, na.rm = TRUE)),
    .groups = "drop"
  )

settings_table <- tibble::tibble(
  parameter = names(settings),
  value = vapply(settings, as.character, character(1))
) %>%
  bind_rows(
    tibble::tibble(parameter = "model", value = "nslphom_dual"),
    tibble::tibble(parameter = "solver", value = "osqp"),
    tibble::tibble(parameter = "party_system", value = paste(parties, collapse = ", ")),
    tibble::tibble(parameter = "regression", value = "lineares Hauptmodell: origin_to_AfD_probability")
  )

# Relevante Validierungsergebnisse nur als RDS speichern.
saveRDS(local_errors, file.path(validation_output_dir, "vorlaeufig_final_validation_local_errors.rds"))
saveRDS(validation_summary, file.path(validation_output_dir, "vorlaeufig_final_validation_summary.rds"))
saveRDS(ei_unit, file.path(validation_output_dir, "vorlaeufig_final_validation_ei_unit.rds"))
saveRDS(ei_summary, file.path(validation_output_dir, "vorlaeufig_final_validation_ei_summary.rds"))
saveRDS(beta_estimates, file.path(validation_output_dir, "vorlaeufig_final_validation_beta_estimates.rds"))
saveRDS(beta_summary, file.path(validation_output_dir, "vorlaeufig_final_validation_beta_summary.rds"))
saveRDS(run_status, file.path(validation_output_dir, "vorlaeufig_final_validation_run_status.rds"))
saveRDS(settings_table, file.path(validation_output_dir, "vorlaeufig_final_validation_settings.rds"))
saveRDS(true_afd_betas, file.path(validation_output_dir, "vorlaeufig_final_validation_true_betas.rds"))

message("Finale Methodenvalidierung gespeichert unter: ", validation_output_dir)
