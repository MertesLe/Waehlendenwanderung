# nslphom und AfD-Regression wiederholt auf Bootstrap-Stichproben schaetzen.

library(dplyr)
library(tidyr)
library(ggplot2)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")
source("Functions/nslphom_functions.R", encoding = "UTF-8")
source("Functions/regression_functions.R", encoding = "UTF-8")
source("Functions/bootstrap_functions.R", encoding = "UTF-8")

ensure_data_dirs()

# Anzahl, Ziehungsumfang und nslphom-Einstellungen zentral ueber R-Optionen steuern.
threshold <- getOption("waehlendenwanderung.party_threshold", 0.12)
n_bootstrap <- getOption("waehlendenwanderung.bootstrap_n", 500L)
sample_size_option <- getOption("waehlendenwanderung.bootstrap_sample_size", NULL)
seed <- getOption("waehlendenwanderung.bootstrap_seed", 20260721L)
iter_max <- getOption("waehlendenwanderung.bootstrap_nslphom_iter_max", 3L)
tol <- getOption("waehlendenwanderung.bootstrap_nslphom_tol", 1e-5)
solver <- getOption("waehlendenwanderung.bootstrap_nslphom_solver", getOption("waehlendenwanderung.nslphom_solver", "osqp"))
solver <- match.arg(solver, c("osqp", "symphony", "lp_solve"))
run_bootstrap <- isTRUE(getOption("waehlendenwanderung.bootstrap_run", TRUE))
resume_existing <- isTRUE(getOption("waehlendenwanderung.bootstrap_resume", TRUE))
model_type <- "nslphom_dual"
analysis_region <- "ostdeutschland_ohne_berlin"
cache_version <- "ost_dual_osqp_linke_gruene_selected_structure_covariates_v3"

output_dir <- data_dir_model_bootstrap_ost
iteration_dir <- file.path(output_dir, "iterations")
chart_dir <- file.path("Charts", "bootstrap", "ostdeutschland")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(iteration_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)

# R-Objekte ohne Zusatzpaket hashen, damit alte Bootstrap-Iterationen nur bei
# identischer Analysepopulation, identischen Inputs und identischen Kovariaten wiederverwendet werden.
hash_r_object <- function(x) {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp)
  unname(tools::md5sum(tmp))
}

if (solver == "osqp") {
  check_osqp_available()
}

# Wahlinputs und Strukturwerte einmal laden und vor allen Wiederholungen validieren.
inputs <- read_prepared_nslphom_inputs()

# Bootstrap und Regression auf die ostdeutschen Flaechenlaender begrenzen.
# Berlin bleibt wegen der nur gesamtstaedtisch vorliegenden INKAR-Werte ausgeschlossen.
ost_ids <- inputs$input2021$agg_schluessel[
  is_ostdeutschland_ohne_berlin(inputs$input2021$agg_schluessel)
]
inputs$input2021 <- inputs$input2021 %>% filter(.data$agg_schluessel %in% ost_ids)
inputs$input2025 <- inputs$input2025 %>% filter(.data$agg_schluessel %in% ost_ids)
inputs$input_long <- inputs$input_long %>% filter(.data$agg_schluessel %in% ost_ids)
inputs$input_checks <- inputs$input_checks %>% filter(.data$agg_schluessel %in% ost_ids)

validation <- validate_prepared_nslphom_inputs(inputs, threshold = threshold)
struktur <- readRDS(file.path(data_dir_cleaned, "inkar_kovariaten_2023.rds"))
struktur_covariates <- get_structure_covariates(struktur)

# Fuer den Bootstrap wird die Population auf Einheiten begrenzt, die auch in der
# anschliessenden Regression vollstaendige Strukturwerte besitzen.
struktur <- struktur %>%
  filter(is_ostdeutschland_ohne_berlin(.data$agg_schluessel)) %>%
  filter(if_all(all_of(struktur_covariates), ~ !is.na(.x))) %>%
  arrange(.data$agg_schluessel)

analysis_ids <- inputs$input2021$agg_schluessel[
  inputs$input2021$agg_schluessel %in% struktur$agg_schluessel
]

if (length(analysis_ids) == 0L) {
  stop("Keine ostdeutsche Bootstrap-Analyseeinheit hat vollstaendige Strukturkovariaten.")
}

inputs$input2021 <- inputs$input2021 %>% filter(.data$agg_schluessel %in% analysis_ids)
inputs$input2025 <- inputs$input2025 %>% filter(.data$agg_schluessel %in% analysis_ids)
inputs$input_long <- inputs$input_long %>% filter(.data$agg_schluessel %in% analysis_ids)
inputs$input_checks <- inputs$input_checks %>% filter(.data$agg_schluessel %in% analysis_ids)
struktur <- struktur %>% filter(.data$agg_schluessel %in% analysis_ids)

stopifnot(identical(inputs$input2021$agg_schluessel, inputs$input2025$agg_schluessel))
stopifnot(setequal(inputs$input2021$agg_schluessel, struktur$agg_schluessel))

sample_size <- if (is.null(sample_size_option)) {
  nrow(inputs$input2021)
} else {
  as.integer(sample_size_option)
}

# Iterationscache nur wiederverwenden, wenn Inputs und Modelleinstellungen gleich sind.
input_file_signature <- paste(
  unname(tools::md5sum(c(
    file.path(data_dir_cleaned, "nslphom_input_2021.rds"),
    file.path(data_dir_cleaned, "nslphom_input_2025.rds"),
    file.path(data_dir_cleaned, "inkar_kovariaten_2023.rds")
  ))),
  collapse = "|"
)

analysis_signature <- hash_r_object(list(
  cache_version = cache_version,
  input_file_signature = input_file_signature,
  input2021 = inputs$input2021,
  input2025 = inputs$input2025,
  struktur = struktur %>% select(agg_schluessel, all_of(struktur_covariates)),
  model_type = model_type,
  analysis_region = analysis_region,
  threshold = threshold,
  sample_size = sample_size,
  seed = seed,
  iter_max = iter_max,
  tol = tol,
  solver = solver,
  osqp_local_solver = if (solver == "osqp") getOption("waehlendenwanderung.osqp_local_solver", "lp_solve") else NA_character_,
  osqp_max_iter = if (solver == "osqp") as.integer(getOption("waehlendenwanderung.osqp_max_iter", 100000L)) else NA_integer_,
  osqp_eps_abs = if (solver == "osqp") getOption("waehlendenwanderung.osqp_eps_abs", 1e-3) else NA_real_,
  osqp_eps_rel = if (solver == "osqp") getOption("waehlendenwanderung.osqp_eps_rel", 1e-3) else NA_real_,
  osqp_polishing = if (solver == "osqp") isTRUE(getOption("waehlendenwanderung.osqp_polishing", TRUE)) else NA,
  covariates = struktur_covariates,
  lphom_package_version = as.character(utils::packageVersion("lphom")),
  osqp_package_version = if (solver == "osqp") as.character(utils::packageVersion("osqp")) else NA_character_
))

analysis_signature_components <- tibble::tibble(
  cache_version = cache_version,
  input_file_signature = input_file_signature,
  threshold,
  sample_size,
  seed,
  iter_max,
  tol,
  solver,
  osqp_local_solver = if (solver == "osqp") getOption("waehlendenwanderung.osqp_local_solver", "lp_solve") else NA_character_,
  model = model_type,
  analysis_region = analysis_region,
  covariates = paste(struktur_covariates, collapse = ","),
  analysis_signature = analysis_signature
)

# Bootstrap-Idee: Pro Wiederholung werden ostdeutsche agg.schluessel ohne Berlin
# mit Zuruecklegen gezogen. nslphom_dual wird ohne Blockaufteilung auf dieser
# Bootstrap-Stichprobe geschaetzt. Danach werden die kuenstlichen Bootstrap-IDs
# vor der Regression wieder auf die originalen agg.schluessel gemappt.
# Vollstaendige Einstellungen speichern, damit ein unterbrochener Lauf reproduzierbar fortsetzbar ist.
settings <- tibble::tibble(
  threshold = threshold,
  n_bootstrap = n_bootstrap,
  sample_size = sample_size,
  seed = seed,
  iter_max = iter_max,
  tol = tol,
  covariates = paste(struktur_covariates, collapse = ", "),
  groups = paste(validation$group_names, collapse = ", "),
  keep_parties = paste(validation$kept_parties, collapse = ", "),
  new_and_exit_voters = "simultaneous",
  solver = solver,
  model = model_type,
  blocked = FALSE,
  analysis_region = analysis_region,
  berlin_included = FALSE,
  n_population = nrow(inputs$input2021),
  n_population_before_complete_covariate_filter = length(ost_ids),
  n_excluded_missing_covariates = length(ost_ids) - nrow(inputs$input2021),
  analysis_signature = analysis_signature,
  resampling = "Ostdeutschland ohne Berlin mit Zuruecklegen, ohne nslphom-Bloecke"
)

saveRDS(settings, file.path(output_dir, "bootstrap_settings.rds"))
saveRDS(analysis_signature_components, file.path(output_dir, "bootstrap_cache_signature.rds"))

if (!run_bootstrap) {
  message(
    "Bootstrap-Settings wurden gespeichert. ",
    "Der Bootstrap-Lauf wurde wegen option waehlendenwanderung.bootstrap_run = FALSE uebersprungen."
  )
} else {
  iteration_results <- vector("list", n_bootstrap)

  for (iteration in seq_len(n_bootstrap)) {
    iteration_file <- file.path(
      iteration_dir,
      sprintf("bootstrap_iteration_%04d.rds", iteration)
    )

    if (resume_existing && file.exists(iteration_file)) {
      cached_result <- readRDS(iteration_file)

      cached_terms <- if (!is.null(cached_result$beta_draws)) {
        unique(cached_result$beta_draws$term)
      } else {
        character()
      }
      expected_terms <- paste0(struktur_covariates, "_z")
      cache_uses_current_covariates <- setequal(
        setdiff(cached_terms, "(Intercept)"),
        expected_terms
      )
      cache_uses_current_model_target <- identical(
        unique(cached_result$beta_draws$model_target),
        "origin_to_AfD_probability"
      )
      cache_uses_current_settings <- identical(
        cached_result$analysis_signature,
        analysis_signature
      )

      if (
        is.null(cached_result$error) &&
          cache_uses_current_covariates &&
          cache_uses_current_model_target &&
          cache_uses_current_settings
      ) {
        message("Lese erfolgreiche vorhandene Bootstrap-Iteration ", iteration, ".")
        iteration_results[[iteration]] <- cached_result
        next
      }

      if (is.null(cached_result$error)) {
        message(
          "Vorhandene Bootstrap-Iteration ",
          iteration,
          " verwendet andere Inputs oder Modelleinstellungen und wird neu gestartet."
        )
      } else {
        message(
          "Vorhandene Bootstrap-Iteration ",
          iteration,
          " war fehlgeschlagen und wird neu gestartet. Alter Fehler: ",
          cached_result$error
        )
      }
    }

    message("Starte Ost-Bootstrap-Iteration ", iteration, " von ", n_bootstrap, ".")

    current_result <- tryCatch(
      run_bootstrap_iteration(
        iteration = iteration,
        input2021 = inputs$input2021,
        input2025 = inputs$input2025,
        struktur = struktur,
        covariates = struktur_covariates,
        sample_size = sample_size,
        seed = seed,
        iter_max = iter_max,
        tol = tol,
        solver = solver,
        threshold = threshold
      ),
      error = function(error) {
        list(
          error = conditionMessage(error),
          bootstrap_id = iteration
        )
      }
    )

    current_result$analysis_signature <- analysis_signature

    saveRDS(current_result, iteration_file)
    iteration_results[[iteration]] <- current_result
  }

  failures <- dplyr::bind_rows(lapply(iteration_results, function(result) {
    if (!is.null(result$error)) {
      tibble::tibble(
        bootstrap_id = result$bootstrap_id,
        error = result$error
      )
    }
  }))

  successful_results <- Filter(function(result) is.null(result$error), iteration_results)

  if (length(successful_results) == 0) {
    stop("Keine Bootstrap-Iteration war erfolgreich. Details liegen in ", iteration_dir, ".")
  }

  beta_draws <- dplyr::bind_rows(lapply(successful_results, `[[`, "beta_draws"))
  beta_intervals <- summarise_bootstrap_betas(beta_draws)
  bootstrap_checks <- dplyr::bind_rows(lapply(successful_results, `[[`, "checks"))
  bootstrap_mapping_checks <- dplyr::bind_rows(lapply(successful_results, `[[`, "mapping_checks"))
  bootstrap_sample_summary <- dplyr::bind_rows(lapply(successful_results, `[[`, "sample_summary"))

  write_bootstrap_outputs(
    beta_draws = beta_draws,
    beta_intervals = beta_intervals,
    checks = bootstrap_checks,
    mapping_checks = bootstrap_mapping_checks,
    sample_summary = bootstrap_sample_summary,
    settings = settings,
    output_dir = output_dir
  )

  saveRDS(failures, file.path(output_dir, "bootstrap_failures.rds"))

  if (nrow(beta_draws) > 0) {
    beta_plot <- plot_bootstrap_beta_distributions(beta_draws)
    ggplot2::ggsave(
      filename = file.path(chart_dir, "bootstrap_beta_verteilungen_ostdeutschland.pdf"),
      plot = beta_plot,
      width = 14,
      height = 9,
      device = grDevices::cairo_pdf
    )
  }

  message("Ost-Bootstrap abgeschlossen. Ergebnisse gespeichert unter: ", output_dir)
}
