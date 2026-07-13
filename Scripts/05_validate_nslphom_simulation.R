library(dplyr)
library(tidyr)

dir.create("Data/cleaned", recursive = TRUE, showWarnings = FALSE)

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
  progress_every = getOption("waehlendenwanderung.validation_progress_every", 50L)
)

# Zwei-Parteien-Simulation:
# Zuerst wird das Wahlergebnis der ersten Wahl je Gebiet aus einer
# Multinomialverteilung gezogen. Danach erzeugen bekannte logit-Modelle die
# lokalen Uebergangswahrscheinlichkeiten A->A und B->A. Die Gegenwahrscheinlich-
# keiten A->B und B->B ergeben sich daraus. Damit kennen wir die wahren lokalen
# Matrizen und koennen pruefen, ob nslphom sie aus den aggregierten Randdaten
# beider Wahlen wiederfindet.
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
beta_summary <- summarise_beta_recovery(
  beta_estimates %>%
    filter(!failed)
)

settings_table <- tibble(
  parameter = names(settings),
  value = vapply(settings, as.character, character(1))
)

saveRDS(local_errors, "Data/cleaned/vorlaeufig_nslphom_validation_local_errors.rds")
saveRDS(validation_summary, "Data/cleaned/vorlaeufig_nslphom_validation_summary.rds")
saveRDS(beta_estimates, "Data/cleaned/vorlaeufig_nslphom_validation_beta_estimates.rds")
saveRDS(beta_summary, "Data/cleaned/vorlaeufig_nslphom_validation_beta_summary.rds")
saveRDS(run_status, "Data/cleaned/vorlaeufig_nslphom_validation_run_status.rds")
saveRDS(settings_table, "Data/cleaned/vorlaeufig_nslphom_validation_settings.rds")
saveRDS(transition_betas, "Data/cleaned/vorlaeufig_nslphom_validation_true_betas.rds")

write.csv(local_errors, "Data/cleaned/vorlaeufig_nslphom_validation_local_errors.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(validation_summary, "Data/cleaned/vorlaeufig_nslphom_validation_summary.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(beta_estimates, "Data/cleaned/vorlaeufig_nslphom_validation_beta_estimates.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(beta_summary, "Data/cleaned/vorlaeufig_nslphom_validation_beta_summary.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(run_status, "Data/cleaned/vorlaeufig_nslphom_validation_run_status.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(settings_table, "Data/cleaned/vorlaeufig_nslphom_validation_settings.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(transition_betas, "Data/cleaned/vorlaeufig_nslphom_validation_true_betas.csv", row.names = FALSE, fileEncoding = "UTF-8")
