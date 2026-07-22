library(dplyr)
library(tidyr)
library(ggplot2)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")
source("Functions/nslphom_functions.R", encoding = "UTF-8")
source("Functions/regression_functions.R", encoding = "UTF-8")
source("Functions/bootstrap_functions.R", encoding = "UTF-8")

ensure_data_dirs()

threshold <- getOption("waehlendenwanderung.party_threshold", 0.12)
n_bootstrap <- getOption("waehlendenwanderung.bootstrap_n", 500L)
sample_size <- getOption("waehlendenwanderung.bootstrap_sample_size", 2000L)
seed <- getOption("waehlendenwanderung.bootstrap_seed", 20260721L)
iter_max <- getOption("waehlendenwanderung.bootstrap_nslphom_iter_max", 10L)
tol <- getOption("waehlendenwanderung.bootstrap_nslphom_tol", 1e-5)
run_bootstrap <- isTRUE(getOption("waehlendenwanderung.bootstrap_run", TRUE))
resume_existing <- isTRUE(getOption("waehlendenwanderung.bootstrap_resume", TRUE))

output_dir <- data_dir_model_bootstrap
iteration_dir <- file.path(output_dir, "iterations")
chart_dir <- "Charts"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(iteration_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)

inputs <- read_prepared_nslphom_inputs()
validation <- validate_prepared_nslphom_inputs(inputs, threshold = threshold)
struktur <- readRDS(file.path(data_dir_cleaned, "vorlaeufig_inkar_kovariaten_2023.rds"))

# Bootstrap-Idee: Pro Wiederholung werden bundesweit agg.schluessel mit
# Zuruecklegen gezogen. nslphom wird ohne Blockaufteilung auf genau dieser
# Bootstrap-Stichprobe geschaetzt. Danach werden die kuenstlichen Bootstrap-IDs
# vor der Regression wieder auf die originalen agg.schluessel gemappt.
settings <- tibble::tibble(
  threshold = threshold,
  n_bootstrap = n_bootstrap,
  sample_size = sample_size,
  seed = seed,
  iter_max = iter_max,
  tol = tol,
  groups = paste(validation$group_names, collapse = ", "),
  keep_parties = paste(validation$kept_parties, collapse = ", "),
  new_and_exit_voters = "simultaneous",
  solver = "lp_solve",
  blocked = FALSE,
  resampling = "bundesweit mit Zuruecklegen, ohne nslphom-Bloecke"
)

saveRDS(settings, file.path(output_dir, "vorlaeufig_bootstrap_settings.rds"))

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
      sprintf("vorlaeufig_bootstrap_iteration_%04d.rds", iteration)
    )

    if (resume_existing && file.exists(iteration_file)) {
      cached_result <- readRDS(iteration_file)

      if (is.null(cached_result$error)) {
        message("Lese erfolgreiche vorhandene Bootstrap-Iteration ", iteration, ".")
        iteration_results[[iteration]] <- cached_result
        next
      }

      message(
        "Vorhandene Bootstrap-Iteration ",
        iteration,
        " war fehlgeschlagen und wird neu gestartet. Alter Fehler: ",
        cached_result$error
      )
    }

    message("Starte unblocked Bootstrap-Iteration ", iteration, " von ", n_bootstrap, ".")

    current_result <- tryCatch(
      run_bootstrap_iteration(
        iteration = iteration,
        input2021 = inputs$input2021,
        input2025 = inputs$input2025,
        struktur = struktur,
        sample_size = sample_size,
        seed = seed,
        iter_max = iter_max,
        tol = tol,
        threshold = threshold,
        covariates = default_struktur_covariates
      ),
      error = function(error) {
        list(
          error = conditionMessage(error),
          bootstrap_id = iteration
        )
      }
    )

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

  saveRDS(failures, file.path(output_dir, "vorlaeufig_bootstrap_failures.rds"))

  if (nrow(beta_draws) > 0) {
    beta_plot <- plot_bootstrap_beta_distributions(beta_draws)
    ggplot2::ggsave(
      filename = file.path(chart_dir, "vorlaeufig_bootstrap_beta_verteilungen.pdf"),
      plot = beta_plot,
      width = 14,
      height = 9,
      device = grDevices::cairo_pdf
    )
  }

  message("Unblocked Bootstrap abgeschlossen. Ergebnisse gespeichert unter: ", output_dir)
}
