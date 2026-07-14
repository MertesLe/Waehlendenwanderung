library(dplyr)
library(tidyr)

source("paths.R", encoding = "UTF-8")

ensure_data_dirs()
validation_output_dir <- getOption("waehlendenwanderung.validation_output_dir", data_dir_validation)
dir.create(validation_output_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("lphom", quietly = TRUE)) {
  stop(
    "Das Paket 'lphom' ist nicht installiert. ",
    "Installiere es mit install.packages('lphom'), damit lphom::nslphom() laufen kann."
  )
}

inv_logit <- function(x) {
  stats::plogis(x)
}

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

settings <- list(
  n_sim = getOption("waehlendenwanderung.validation_n_sim", 1000L),
  n_units = getOption("waehlendenwanderung.validation_n_units", 120L),
  seed = getOption("waehlendenwanderung.validation_seed", 20260713L),
  iter_max = getOption("waehlendenwanderung.validation_iter_max", 10L),
  electorate_min = getOption("waehlendenwanderung.validation_electorate_min", 350L),
  electorate_max = getOption("waehlendenwanderung.validation_electorate_max", 1800L),
  covariate_year = getOption("waehlendenwanderung.validation_covariate_year", 2023L),
  progress_every = getOption("waehlendenwanderung.validation_progress_every", 50L)
)

# Zwei-Parteien-Simulation:
# Zuerst wird das Wahlergebnis der ersten Wahl je Gebiet aus einer
# Multinomialverteilung gezogen. Danach erzeugen bekannte logit-Modelle mit
# statischen Strukturmerkmalen im Zwischenjahr 2023 die lokalen
# Uebergangswahrscheinlichkeiten A->A und B->A. Es werden hier bewusst keine
# Veraenderungswerte zwischen zwei Jahren simuliert. Die Gegenwahrscheinlichkeiten
# A->B und B->B ergeben sich daraus. Damit kennen wir die wahren lokalen Matrizen
# und koennen pruefen, ob nslphom sie aus den aggregierten Randdaten beider
# Wahlen wiederfindet.
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

simulate_origin_counts <- function(electorate, p_a) {
  counts <- t(vapply(
    seq_along(electorate),
    function(i) {
      as.integer(stats::rmultinom(1, size = electorate[[i]], prob = c(p_a[[i]], 1 - p_a[[i]])))
    },
    integer(2)
  ))

  colnames(counts) <- c("A", "B")
  counts
}

simulate_transition_counts <- function(origin_a, origin_b, p_a_to_a, p_b_to_a) {
  counts <- t(vapply(
    seq_along(origin_a),
    function(i) {
      from_a <- as.integer(stats::rmultinom(
        1,
        size = origin_a[[i]],
        prob = c(p_a_to_a[[i]], 1 - p_a_to_a[[i]])
      ))
      from_b <- as.integer(stats::rmultinom(
        1,
        size = origin_b[[i]],
        prob = c(p_b_to_a[[i]], 1 - p_b_to_a[[i]])
      ))

      c(
        A_to_A = from_a[[1]],
        A_to_B = from_a[[2]],
        B_to_A = from_b[[1]],
        B_to_B = from_b[[2]]
      )
    },
    integer(4)
  ))

  colnames(counts) <- c("A_to_A", "A_to_B", "B_to_A", "B_to_B")
  counts
}

make_transition_long <- function(
  sim_id,
  unit_data,
  transition_counts,
  p_a_to_a,
  p_b_to_a,
  fit
) {
  prop_units <- fit[["VTM.prop.units"]]
  votes_units <- fit[["VTM.votes.units"]]

  stopifnot(all(c("A", "B") %in% dimnames(prop_units)[[1]]))
  stopifnot(all(c("A", "B") %in% dimnames(prop_units)[[2]]))

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
      realized_probability = transition_counts[, "A_to_A"] / unit_data$origin_A,
      estimated_probability = extract_unit_values(prop_units, "A", "A"),
      true_transition_count = transition_counts[, "A_to_A"],
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
      realized_probability = transition_counts[, "A_to_B"] / unit_data$origin_A,
      estimated_probability = extract_unit_values(prop_units, "A", "B"),
      true_transition_count = transition_counts[, "A_to_B"],
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
      realized_probability = transition_counts[, "B_to_A"] / unit_data$origin_B,
      estimated_probability = extract_unit_values(prop_units, "B", "A"),
      true_transition_count = transition_counts[, "B_to_A"],
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
      realized_probability = transition_counts[, "B_to_B"] / unit_data$origin_B,
      estimated_probability = extract_unit_values(prop_units, "B", "B"),
      true_transition_count = transition_counts[, "B_to_B"],
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
      total_estimated_count = sum(estimated_transition_count, na.rm = TRUE),
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
  unit_id <- sprintf("sim_%04d_unit_%03d", sim_id, seq_len(settings$n_units))
  x_binary <- stats::rbinom(settings$n_units, size = 1, prob = 0.45)
  x_continuous <- as.numeric(scale(stats::rnorm(settings$n_units)))
  electorate <- sample(
    seq.int(settings$electorate_min, settings$electorate_max),
    size = settings$n_units,
    replace = TRUE
  )

  p_origin_a <- inv_logit(
    origin_betas[["intercept"]] +
      origin_betas[["x_binary"]] * x_binary +
      origin_betas[["x_continuous"]] * x_continuous
  )

  origin_counts <- simulate_origin_counts(electorate, p_origin_a)
  p_a_to_a <- inv_logit(
    transition_betas$true_beta[transition_betas$transition == "A_to_A" & transition_betas$term == "(Intercept)"] +
      transition_betas$true_beta[transition_betas$transition == "A_to_A" & transition_betas$term == "x_binary"] * x_binary +
      transition_betas$true_beta[transition_betas$transition == "A_to_A" & transition_betas$term == "x_continuous"] * x_continuous
  )
  p_b_to_a <- inv_logit(
    transition_betas$true_beta[transition_betas$transition == "B_to_A" & transition_betas$term == "(Intercept)"] +
      transition_betas$true_beta[transition_betas$transition == "B_to_A" & transition_betas$term == "x_binary"] * x_binary +
      transition_betas$true_beta[transition_betas$transition == "B_to_A" & transition_betas$term == "x_continuous"] * x_continuous
  )

  transition_counts <- simulate_transition_counts(
    origin_counts[, "A"],
    origin_counts[, "B"],
    p_a_to_a,
    p_b_to_a
  )

  destination_a <- transition_counts[, "A_to_A"] + transition_counts[, "B_to_A"]
  destination_b <- transition_counts[, "A_to_B"] + transition_counts[, "B_to_B"]

  unit_data <- tibble(
    unit_id = unit_id,
    x_binary = x_binary,
    x_continuous = x_continuous,
    electorate = electorate,
    origin_A = origin_counts[, "A"],
    origin_B = origin_counts[, "B"],
    destination_A = destination_a,
    destination_B = destination_b
  )

  election_1 <- data.frame(
    A = unit_data$origin_A,
    B = unit_data$origin_B
  )
  election_2 <- data.frame(
    A = unit_data$destination_A,
    B = unit_data$destination_B
  )
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
    return(list(
      local_errors = tibble(
        sim_id = sim_id,
        failed = TRUE,
        error_message = conditionMessage(fit)
      ),
      beta_estimates = tibble(
        sim_id = sim_id,
        failed = TRUE,
        error_message = conditionMessage(fit)
      ),
      run_status = tibble(
        sim_id = sim_id,
        failed = TRUE,
        error_message = conditionMessage(fit),
        iter = NA_integer_,
        HETe = NA_real_
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
    message("Validiere nslphom-Simulation ", sim_id, " von ", settings$n_sim, ".")
  }

  simulation_results[[sim_id]] <- run_one_simulation(sim_id)
}

local_errors <- bind_rows(lapply(simulation_results, `[[`, "local_errors"))
beta_estimates <- bind_rows(lapply(simulation_results, `[[`, "beta_estimates"))
run_status <- bind_rows(lapply(simulation_results, `[[`, "run_status"))
validation_summary <- summarise_local_errors(local_errors)
estimation_error <- calculate_estimation_error(local_errors)
estimation_error_summary <- summarise_estimation_error(estimation_error)
beta_summary <- summarise_beta_recovery(
  beta_estimates %>%
    filter(!failed)
)

settings_table <- tibble(
  parameter = names(settings),
  value = vapply(settings, as.character, character(1))
)

saveRDS(local_errors, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_local_errors.rds"))
saveRDS(validation_summary, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_summary.rds"))
saveRDS(estimation_error, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_ei_unit.rds"))
saveRDS(estimation_error_summary, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_ei_summary.rds"))
saveRDS(beta_estimates, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_beta_estimates.rds"))
saveRDS(beta_summary, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_beta_summary.rds"))
saveRDS(run_status, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_run_status.rds"))
saveRDS(settings_table, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_settings.rds"))
saveRDS(transition_betas, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_true_betas.rds"))

write.csv(local_errors, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_local_errors.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(validation_summary, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(estimation_error, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_ei_unit.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(estimation_error_summary, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_ei_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(beta_estimates, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_beta_estimates.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(beta_summary, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_beta_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(run_status, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_run_status.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(settings_table, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_settings.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(transition_betas, file.path(validation_output_dir, "vorlaeufig_nslphom_validation_true_betas.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# --- BEGIN Sensitivitaetschecks: andere Blockdefinitionen, Parteigruppierung und groessere Aggregationseinheiten ---
sensitivity_settings <- list(
  n_sim = getOption("waehlendenwanderung.sensitivity_n_sim", min(200L, settings$n_sim)),
  iter_max = getOption("waehlendenwanderung.sensitivity_iter_max", settings$iter_max),
  unit_order_block_size = getOption("waehlendenwanderung.sensitivity_unit_order_block_size", 20L),
  aggregation_group_size = getOption("waehlendenwanderung.sensitivity_aggregation_group_size", 2L),
  party_grouping_n_sim = getOption("waehlendenwanderung.sensitivity_party_grouping_n_sim", 50L),
  party_grouping_n_units = getOption("waehlendenwanderung.sensitivity_party_grouping_n_units", settings$n_units),
  covariate_year = settings$covariate_year
)

make_unit_data_from_local_errors <- function(data) {
  data %>%
    filter(!failed) %>%
    group_by(sim_id, unit_id) %>%
    summarise(
      x_binary = first(x_binary),
      x_continuous = first(x_continuous),
      electorate = first(electorate),
      origin_A = max(origin_count[from == "A"], na.rm = TRUE),
      origin_B = max(origin_count[from == "B"], na.rm = TRUE),
      destination_A = max(destination_count[to == "A"], na.rm = TRUE),
      destination_B = max(destination_count[to == "B"], na.rm = TRUE),
      p_A_to_A = true_probability[transition == "A_to_A"][[1]],
      p_B_to_A = true_probability[transition == "B_to_A"][[1]],
      count_A_to_A = true_transition_count[transition == "A_to_A"][[1]],
      count_A_to_B = true_transition_count[transition == "A_to_B"][[1]],
      count_B_to_A = true_transition_count[transition == "B_to_A"][[1]],
      count_B_to_B = true_transition_count[transition == "B_to_B"][[1]],
      .groups = "drop"
    ) %>%
    arrange(sim_id, unit_id)
}

make_two_party_truth_long <- function(unit_data, variant, sensitivity_type, block_id = "global") {
  bind_rows(
    tibble(
      sim_id = unit_data$sim_id,
      unit_id = unit_data$unit_id,
      block_id = block_id,
      x_binary = unit_data$x_binary,
      x_continuous = unit_data$x_continuous,
      electorate = unit_data$electorate,
      from = "A",
      to = "A",
      transition = "A_to_A",
      origin_count = unit_data$origin_A,
      destination_count = unit_data$destination_A,
      true_probability = unit_data$p_A_to_A,
      realized_probability = unit_data$count_A_to_A / unit_data$origin_A,
      true_transition_count = unit_data$count_A_to_A
    ),
    tibble(
      sim_id = unit_data$sim_id,
      unit_id = unit_data$unit_id,
      block_id = block_id,
      x_binary = unit_data$x_binary,
      x_continuous = unit_data$x_continuous,
      electorate = unit_data$electorate,
      from = "A",
      to = "B",
      transition = "A_to_B",
      origin_count = unit_data$origin_A,
      destination_count = unit_data$destination_B,
      true_probability = 1 - unit_data$p_A_to_A,
      realized_probability = unit_data$count_A_to_B / unit_data$origin_A,
      true_transition_count = unit_data$count_A_to_B
    ),
    tibble(
      sim_id = unit_data$sim_id,
      unit_id = unit_data$unit_id,
      block_id = block_id,
      x_binary = unit_data$x_binary,
      x_continuous = unit_data$x_continuous,
      electorate = unit_data$electorate,
      from = "B",
      to = "A",
      transition = "B_to_A",
      origin_count = unit_data$origin_B,
      destination_count = unit_data$destination_A,
      true_probability = unit_data$p_B_to_A,
      realized_probability = unit_data$count_B_to_A / unit_data$origin_B,
      true_transition_count = unit_data$count_B_to_A
    ),
    tibble(
      sim_id = unit_data$sim_id,
      unit_id = unit_data$unit_id,
      block_id = block_id,
      x_binary = unit_data$x_binary,
      x_continuous = unit_data$x_continuous,
      electorate = unit_data$electorate,
      from = "B",
      to = "B",
      transition = "B_to_B",
      origin_count = unit_data$origin_B,
      destination_count = unit_data$destination_B,
      true_probability = 1 - unit_data$p_B_to_A,
      realized_probability = unit_data$count_B_to_B / unit_data$origin_B,
      true_transition_count = unit_data$count_B_to_B
    )
  ) %>%
    mutate(
      variant = variant,
      sensitivity_type = sensitivity_type,
      .before = sim_id
    )
}

extract_two_party_fit <- function(fit, unit_ids) {
  prop_units <- fit[["VTM.prop.units"]]
  votes_units <- fit[["VTM.votes.units"]]

  bind_rows(
    tibble(
      unit_id = unit_ids,
      transition = "A_to_A",
      estimated_probability = extract_unit_values(prop_units, "A", "A"),
      estimated_transition_count = extract_unit_values(votes_units, "A", "A")
    ),
    tibble(
      unit_id = unit_ids,
      transition = "A_to_B",
      estimated_probability = extract_unit_values(prop_units, "A", "B"),
      estimated_transition_count = extract_unit_values(votes_units, "A", "B")
    ),
    tibble(
      unit_id = unit_ids,
      transition = "B_to_A",
      estimated_probability = extract_unit_values(prop_units, "B", "A"),
      estimated_transition_count = extract_unit_values(votes_units, "B", "A")
    ),
    tibble(
      unit_id = unit_ids,
      transition = "B_to_B",
      estimated_probability = extract_unit_values(prop_units, "B", "B"),
      estimated_transition_count = extract_unit_values(votes_units, "B", "B")
    )
  )
}

fit_two_party_block <- function(unit_data, variant, sensitivity_type, block_id) {
  truth <- make_two_party_truth_long(unit_data, variant, sensitivity_type, block_id)

  if (nrow(unit_data) < 2) {
    return(truth %>%
      mutate(
        estimated_probability = NA_real_,
        estimated_transition_count = NA_real_,
        failed = TRUE,
        error_message = "Block contains fewer than two units."
      ))
  }

  election_1 <- data.frame(A = unit_data$origin_A, B = unit_data$origin_B)
  election_2 <- data.frame(A = unit_data$destination_A, B = unit_data$destination_B)
  rownames(election_1) <- unit_data$unit_id
  rownames(election_2) <- unit_data$unit_id

  fit <- tryCatch(
    lphom::nslphom(
      votes_election1 = election_1,
      votes_election2 = election_2,
      new_and_exit_voters = "regular",
      iter.max = sensitivity_settings$iter_max,
      verbose = FALSE,
      solver = "lp_solve",
      distance.local = "abs"
    ),
    error = function(error) error
  )

  if (inherits(fit, "error")) {
    return(truth %>%
      mutate(
        estimated_probability = NA_real_,
        estimated_transition_count = NA_real_,
        failed = TRUE,
        error_message = conditionMessage(fit)
      ))
  }

  truth %>%
    left_join(
      extract_two_party_fit(fit, unit_data$unit_id),
      by = c("unit_id", "transition")
    ) %>%
    mutate(
      failed = FALSE,
      error_message = NA_character_
    )
}

fit_two_party_variant <- function(unit_data, variant, sensitivity_type, block_col) {
  bind_rows(lapply(split(unit_data, unit_data[[block_col]]), function(block_data) {
    fit_two_party_block(
      unit_data = block_data,
      variant = variant,
      sensitivity_type = sensitivity_type,
      block_id = unique(block_data[[block_col]])[[1]]
    )
  }))
}

make_aggregated_unit_data <- function(unit_data, group_size) {
  unit_data %>%
    arrange(unit_id) %>%
    mutate(
      unit_group = ceiling(row_number() / group_size),
      unit_id = paste0("sim_", sprintf("%04d", first(sim_id)), "_agg_", sprintf("%03d", unit_group))
    ) %>%
    group_by(sim_id, unit_id) %>%
    summarise(
      x_binary = mean(x_binary),
      x_continuous = mean(x_continuous),
      electorate = sum(electorate),
      p_A_to_A = sum(p_A_to_A * origin_A) / sum(origin_A),
      p_B_to_A = sum(p_B_to_A * origin_B) / sum(origin_B),
      origin_A = sum(origin_A),
      origin_B = sum(origin_B),
      destination_A = sum(destination_A),
      destination_B = sum(destination_B),
      count_A_to_A = sum(count_A_to_A),
      count_A_to_B = sum(count_A_to_B),
      count_B_to_A = sum(count_B_to_A),
      count_B_to_B = sum(count_B_to_B),
      block_id = "larger_units",
      .groups = "drop"
    )
}

make_existing_global_reference <- function(data) {
  data %>%
    filter(!failed) %>%
    transmute(
      variant = "global_all_units_existing",
      sensitivity_type = "block_definition",
      sim_id,
      unit_id,
      block_id = "global",
      x_binary,
      x_continuous,
      electorate,
      from,
      to,
      transition,
      origin_count,
      destination_count,
      true_probability,
      realized_probability,
      true_transition_count,
      estimated_probability,
      estimated_transition_count,
      failed,
      error_message
    )
}

summarise_sensitivity_errors <- function(data) {
  data %>%
    filter(!failed, origin_count > 0) %>%
    mutate(
      error_vs_true_probability = estimated_probability - true_probability,
      abs_error_vs_true_probability = abs(error_vs_true_probability),
      error_vs_realized_probability = estimated_probability - realized_probability,
      abs_error_vs_realized_probability = abs(error_vs_realized_probability)
    ) %>%
    group_by(sensitivity_type, variant, transition) %>%
    summarise(
      n_sim = n_distinct(sim_id),
      n_rows = n(),
      n_blocks = n_distinct(paste(sim_id, block_id)),
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

calculate_sensitivity_estimation_error <- function(data) {
  data %>%
    filter(
      !failed,
      !is.na(true_transition_count),
      !is.na(estimated_transition_count)
    ) %>%
    group_by(
      sensitivity_type,
      variant,
      sim_id,
      unit_id
    ) %>%
    summarise(
      total_true_count = sum(true_transition_count, na.rm = TRUE),
      total_estimated_count = sum(estimated_transition_count, na.rm = TRUE),
      absolute_joint_count_error = sum(
        abs(true_transition_count - estimated_transition_count),
        na.rm = TRUE
      ),
      estimation_error = 100 * 0.5 * absolute_joint_count_error / total_true_count,
      .groups = "drop"
    )
}

summarise_sensitivity_estimation_error <- function(estimation_error) {
  estimation_error %>%
    group_by(
      sensitivity_type,
      variant
    ) %>%
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

estimate_sensitivity_betas <- function(data) {
  split_keys <- paste(data$variant, data$sim_id, sep = "___")

  bind_rows(lapply(split(data, split_keys), function(current_data) {
    estimate_beta_model(current_data, "estimated_probability", "estimated_probability") %>%
      mutate(
        variant = unique(current_data$variant)[[1]],
        sensitivity_type = unique(current_data$sensitivity_type)[[1]],
        failed = FALSE,
        error_message = NA_character_,
        .before = sim_id
      )
  }))
}

summarise_sensitivity_betas <- function(data) {
  data %>%
    filter(!failed) %>%
    left_join(
      transition_betas,
      by = c("transition", "term")
    ) %>%
    mutate(
      beta_error = estimate - true_beta,
      abs_beta_error = abs(beta_error)
    ) %>%
    group_by(sensitivity_type, variant, transition, term) %>%
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

softmax_rows <- function(eta) {
  shifted <- eta - apply(eta, 1, max)
  exp_eta <- exp(shifted)
  exp_eta / rowSums(exp_eta)
}

simulate_three_party_sensitivity <- function(sim_id) {
  unit_id <- sprintf("party_%04d_unit_%03d", sim_id, seq_len(sensitivity_settings$party_grouping_n_units))
  x_binary <- stats::rbinom(sensitivity_settings$party_grouping_n_units, size = 1, prob = 0.45)
  x_continuous <- as.numeric(scale(stats::rnorm(sensitivity_settings$party_grouping_n_units)))
  electorate <- sample(
    seq.int(settings$electorate_min, settings$electorate_max),
    size = sensitivity_settings$party_grouping_n_units,
    replace = TRUE
  )

  origin_eta <- cbind(
    A = 0.20 + 0.50 * x_binary - 0.30 * x_continuous,
    B = -0.10 - 0.20 * x_binary + 0.40 * x_continuous,
    C = 0
  )
  origin_prob <- softmax_rows(origin_eta)
  origin_counts <- t(vapply(
    seq_along(electorate),
    function(i) as.integer(stats::rmultinom(1, size = electorate[[i]], prob = origin_prob[i, ])),
    integer(3)
  ))
  colnames(origin_counts) <- c("A", "B", "C")

  transition_prob <- array(
    NA_real_,
    dim = c(sensitivity_settings$party_grouping_n_units, 3, 3),
    dimnames = list(unit_id, c("A", "B", "C"), c("A", "B", "C"))
  )
  transition_prob[, "A", ] <- softmax_rows(cbind(
    A = 1.10 - 0.35 * x_binary + 0.55 * x_continuous,
    B = -0.15 + 0.20 * x_binary - 0.10 * x_continuous,
    C = 0
  ))
  transition_prob[, "B", ] <- softmax_rows(cbind(
    A = -0.80 + 0.60 * x_binary - 0.40 * x_continuous,
    B = 0.90 - 0.20 * x_binary + 0.20 * x_continuous,
    C = 0
  ))
  transition_prob[, "C", ] <- softmax_rows(cbind(
    A = -0.40 + 0.20 * x_binary + 0.10 * x_continuous,
    B = -0.20 - 0.10 * x_binary + 0.20 * x_continuous,
    C = 0
  ))

  transition_counts <- array(
    0L,
    dim = c(sensitivity_settings$party_grouping_n_units, 3, 3),
    dimnames = list(unit_id, c("A", "B", "C"), c("A", "B", "C"))
  )

  for (i in seq_along(unit_id)) {
    for (from_party in c("A", "B", "C")) {
      transition_counts[i, from_party, ] <- as.integer(stats::rmultinom(
        1,
        size = origin_counts[i, from_party],
        prob = transition_prob[i, from_party, ]
      ))
    }
  }

  destination_counts <- t(vapply(
    seq_along(unit_id),
    function(i) colSums(transition_counts[i, , ]),
    numeric(3)
  ))
  colnames(destination_counts) <- c("A", "B", "C")

  fit_full <- tryCatch(
    lphom::nslphom(
      votes_election1 = as.data.frame(origin_counts),
      votes_election2 = as.data.frame(destination_counts),
      new_and_exit_voters = "regular",
      iter.max = sensitivity_settings$iter_max,
      verbose = FALSE,
      solver = "lp_solve",
      distance.local = "abs"
    ),
    error = function(error) error
  )

  collapsed_origin <- data.frame(
    A = origin_counts[, "A"],
    Other = origin_counts[, "B"] + origin_counts[, "C"]
  )
  collapsed_destination <- data.frame(
    A = destination_counts[, "A"],
    Other = destination_counts[, "B"] + destination_counts[, "C"]
  )
  rownames(collapsed_origin) <- unit_id
  rownames(collapsed_destination) <- unit_id

  fit_collapsed <- tryCatch(
    lphom::nslphom(
      votes_election1 = collapsed_origin,
      votes_election2 = collapsed_destination,
      new_and_exit_voters = "regular",
      iter.max = sensitivity_settings$iter_max,
      verbose = FALSE,
      solver = "lp_solve",
      distance.local = "abs"
    ),
    error = function(error) error
  )

  if (inherits(fit_full, "error") || inherits(fit_collapsed, "error")) {
    return(tibble(
      sensitivity_type = "party_grouping",
      variant = c("three_party_full", "collapsed_A_vs_Other"),
      sim_id = sim_id,
      failed = TRUE,
      error_message = c(
        if (inherits(fit_full, "error")) conditionMessage(fit_full) else NA_character_,
        if (inherits(fit_collapsed, "error")) conditionMessage(fit_collapsed) else NA_character_
      )
    ))
  }

  full_truth <- bind_rows(lapply(c("A", "B", "C"), function(from_party) {
    bind_rows(lapply(c("A", "B", "C"), function(to_party) {
      tibble(
        sensitivity_type = "party_grouping",
        variant = "three_party_full",
        sim_id = sim_id,
        unit_id = unit_id,
        block_id = "global",
        from = from_party,
        to = to_party,
        transition = paste(from_party, "to", to_party, sep = "_"),
        origin_count = origin_counts[, from_party],
        destination_count = destination_counts[, to_party],
        true_probability = transition_prob[, from_party, to_party],
        realized_probability = transition_counts[, from_party, to_party] / origin_counts[, from_party],
        true_transition_count = transition_counts[, from_party, to_party],
        estimated_probability = extract_unit_values(fit_full[["VTM.prop.units"]], from_party, to_party),
        estimated_transition_count = extract_unit_values(fit_full[["VTM.votes.units"]], from_party, to_party),
        failed = FALSE,
        error_message = NA_character_
      )
    }))
  }))

  other_origin <- origin_counts[, "B"] + origin_counts[, "C"]
  other_to_a_expected <- origin_counts[, "B"] * transition_prob[, "B", "A"] +
    origin_counts[, "C"] * transition_prob[, "C", "A"]
  other_to_a_realized <- transition_counts[, "B", "A"] + transition_counts[, "C", "A"]

  collapsed_truth <- bind_rows(
    tibble(
      sensitivity_type = "party_grouping",
      variant = "collapsed_A_vs_Other",
      sim_id = sim_id,
      unit_id = unit_id,
      block_id = "global",
      from = "A",
      to = "A",
      transition = "A_to_A",
      origin_count = origin_counts[, "A"],
      destination_count = collapsed_destination$A,
      true_probability = transition_prob[, "A", "A"],
      realized_probability = transition_counts[, "A", "A"] / origin_counts[, "A"],
      true_transition_count = transition_counts[, "A", "A"],
      estimated_probability = extract_unit_values(fit_collapsed[["VTM.prop.units"]], "A", "A"),
      estimated_transition_count = extract_unit_values(fit_collapsed[["VTM.votes.units"]], "A", "A")
    ),
    tibble(
      sensitivity_type = "party_grouping",
      variant = "collapsed_A_vs_Other",
      sim_id = sim_id,
      unit_id = unit_id,
      block_id = "global",
      from = "Other",
      to = "A",
      transition = "Other_to_A",
      origin_count = other_origin,
      destination_count = collapsed_destination$A,
      true_probability = other_to_a_expected / other_origin,
      realized_probability = other_to_a_realized / other_origin,
      true_transition_count = other_to_a_realized,
      estimated_probability = extract_unit_values(fit_collapsed[["VTM.prop.units"]], "Other", "A"),
      estimated_transition_count = extract_unit_values(fit_collapsed[["VTM.votes.units"]], "Other", "A")
    )
  ) %>%
    mutate(
      failed = FALSE,
      error_message = NA_character_
    )

  bind_rows(full_truth, collapsed_truth)
}

sensitivity_sim_ids <- sort(unique(local_errors$sim_id))
sensitivity_sim_ids <- sensitivity_sim_ids[seq_len(min(length(sensitivity_sim_ids), sensitivity_settings$n_sim))]
sensitivity_source <- local_errors %>%
  filter(sim_id %in% sensitivity_sim_ids)

two_party_unit_data <- make_unit_data_from_local_errors(sensitivity_source)

two_party_sensitivity <- bind_rows(lapply(split(two_party_unit_data, two_party_unit_data$sim_id), function(unit_data) {
  message("Sensitivitaetscheck Zwei-Parteien-Simulation ", unique(unit_data$sim_id)[[1]], ".")

  unit_data <- unit_data %>%
    arrange(unit_id) %>%
    mutate(
      global_block = "global",
      x_binary_block = paste0("x_binary_", x_binary),
      unit_order_block = paste0(
        "order_block_",
        sprintf("%02d", ceiling(row_number() / sensitivity_settings$unit_order_block_size))
      )
    )

  aggregated_data <- make_aggregated_unit_data(
    unit_data = unit_data,
    group_size = sensitivity_settings$aggregation_group_size
  )

  bind_rows(
    make_existing_global_reference(sensitivity_source %>% filter(sim_id == unique(unit_data$sim_id)[[1]])),
    fit_two_party_variant(
      unit_data = unit_data,
      variant = "x_binary_blocks",
      sensitivity_type = "block_definition",
      block_col = "x_binary_block"
    ),
    fit_two_party_variant(
      unit_data = unit_data,
      variant = "unit_order_blocks",
      sensitivity_type = "block_definition",
      block_col = "unit_order_block"
    ),
    fit_two_party_variant(
      unit_data = aggregated_data,
      variant = "larger_units_pairs",
      sensitivity_type = "aggregation_level",
      block_col = "block_id"
    )
  )
}))

three_party_sensitivity <- bind_rows(lapply(seq_len(sensitivity_settings$party_grouping_n_sim), function(sim_id) {
  message("Sensitivitaetscheck Parteigruppierung ", sim_id, " von ", sensitivity_settings$party_grouping_n_sim, ".")
  simulate_three_party_sensitivity(sim_id)
}))

sensitivity_local_errors <- bind_rows(
  two_party_sensitivity,
  three_party_sensitivity
) %>%
  mutate(
    covariate_year = settings$covariate_year,
    error_vs_true_probability = estimated_probability - true_probability,
    error_vs_realized_probability = estimated_probability - realized_probability,
    abs_error_vs_true_probability = abs(error_vs_true_probability),
    abs_error_vs_realized_probability = abs(error_vs_realized_probability)
  )

sensitivity_summary <- summarise_sensitivity_errors(sensitivity_local_errors)
sensitivity_estimation_error <- calculate_sensitivity_estimation_error(sensitivity_local_errors)
sensitivity_estimation_error_summary <- summarise_sensitivity_estimation_error(sensitivity_estimation_error)
sensitivity_beta_estimates <- estimate_sensitivity_betas(
  two_party_sensitivity %>%
    filter(sensitivity_type %in% c("block_definition", "aggregation_level"))
)
sensitivity_beta_summary <- summarise_sensitivity_betas(sensitivity_beta_estimates)
sensitivity_settings_table <- tibble(
  parameter = names(sensitivity_settings),
  value = vapply(sensitivity_settings, as.character, character(1))
)

saveRDS(sensitivity_local_errors, file.path(validation_output_dir, "vorlaeufig_nslphom_sensitivity_local_errors.rds"))
saveRDS(sensitivity_summary, file.path(validation_output_dir, "vorlaeufig_nslphom_sensitivity_summary.rds"))
saveRDS(sensitivity_estimation_error, file.path(validation_output_dir, "vorlaeufig_nslphom_sensitivity_ei_unit.rds"))
saveRDS(sensitivity_estimation_error_summary, file.path(validation_output_dir, "vorlaeufig_nslphom_sensitivity_ei_summary.rds"))
saveRDS(sensitivity_beta_estimates, file.path(validation_output_dir, "vorlaeufig_nslphom_sensitivity_beta_estimates.rds"))
saveRDS(sensitivity_beta_summary, file.path(validation_output_dir, "vorlaeufig_nslphom_sensitivity_beta_summary.rds"))
saveRDS(sensitivity_settings_table, file.path(validation_output_dir, "vorlaeufig_nslphom_sensitivity_settings.rds"))

write.csv(sensitivity_local_errors, file.path(validation_output_dir, "vorlaeufig_nslphom_sensitivity_local_errors.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(sensitivity_summary, file.path(validation_output_dir, "vorlaeufig_nslphom_sensitivity_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(sensitivity_estimation_error, file.path(validation_output_dir, "vorlaeufig_nslphom_sensitivity_ei_unit.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(sensitivity_estimation_error_summary, file.path(validation_output_dir, "vorlaeufig_nslphom_sensitivity_ei_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(sensitivity_beta_estimates, file.path(validation_output_dir, "vorlaeufig_nslphom_sensitivity_beta_estimates.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(sensitivity_beta_summary, file.path(validation_output_dir, "vorlaeufig_nslphom_sensitivity_beta_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(sensitivity_settings_table, file.path(validation_output_dir, "vorlaeufig_nslphom_sensitivity_settings.csv"), row.names = FALSE, fileEncoding = "UTF-8")
# --- END Sensitivitaetschecks: andere Blockdefinitionen, Parteigruppierung und groessere Aggregationseinheiten ---
