# Einfaches nslphom ungeblockt auf allen oder ausgewaehlten Aggregationseinheiten schaetzen.

library(dplyr)
library(tidyr)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")
source("Functions/nslphom_functions.R", encoding = "UTF-8")

ensure_data_dirs()

threshold <- getOption("waehlendenwanderung.party_threshold", 0.12)
output_dir <- file.path("Data", "modeloutput", "nslphom_unblocked")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

run_fit <- isTRUE(getOption("waehlendenwanderung.unblocked_run_fit", TRUE))
iter_max <- getOption("waehlendenwanderung.unblocked_nslphom_iter_max", 10L)
tol <- getOption("waehlendenwanderung.unblocked_nslphom_tol", 1e-5)
solver <- getOption("waehlendenwanderung.unblocked_nslphom_solver", getOption("waehlendenwanderung.nslphom_solver", "osqp"))
solver <- match.arg(solver, c("osqp", "symphony", "lp_solve"))

message("Lese zentral vorbereitete nslphom-Inputs mit ", threshold * 100, "%-Parteischwelle.")
inputs <- read_prepared_nslphom_inputs()
validation <- validate_prepared_nslphom_inputs(inputs, threshold = threshold)

settings <- make_unblocked_settings(
  inputs = inputs,
  validation = validation,
  threshold = threshold,
  iter_max = iter_max,
  tol = tol,
  solver = solver,
  blocked = FALSE
)

# Inputkopien und Einstellungen fuer den Lauf auf dem leistungsstaerkeren PC speichern.
saveRDS(inputs$input2021, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_2021.rds"))
saveRDS(inputs$input2025, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_2025.rds"))
saveRDS(inputs$input_long, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_long.rds"))
saveRDS(inputs$party_thresholds, file.path(output_dir, "vorlaeufig_nslphom_unblocked_party_thresholds.rds"))
saveRDS(inputs$input_checks, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_checks.rds"))
saveRDS(settings, file.path(output_dir, "vorlaeufig_nslphom_unblocked_settings.rds"))

if (!run_fit) {
  message(
    "Input-Kopien und Settings wurden gespeichert. ",
    "Der nslphom-Fit wurde wegen option waehlendenwanderung.unblocked_run_fit = FALSE uebersprungen."
  )
} else {
  ids <- inputs$input2021$agg_schluessel
  origin_counts <- make_count_matrix(inputs$input2021)
  destination_counts <- make_count_matrix(inputs$input2025)
  method <- paste0("nslphom_unblocked_", solver)

  message(
    "Starte nationalen nslphom-Lauf ohne Bloecke mit ",
    nrow(origin_counts),
    " Aggregationseinheiten und ",
    ncol(origin_counts),
    " Gruppen mit Solver ",
    solver,
    "."
  )
  message("Dieser Schritt ist speicherintensiv und fuer den leistungsstaerkeren PC gedacht.")

  fit <- fit_nslphom_model(
    origin_counts,
    destination_counts,
    iter_max = iter_max,
    tol = tol,
    solver = solver,
    verbose = TRUE,
    method = method
  )

  message("Bereite lokale und globale Uebergangsmatrizen auf.")

  transition_long <- local_matrices_to_long(fit, ids, method = method) %>%
    arrange(.data$agg_schluessel, .data$from, .data$to)
  transition_wide <- make_transition_wide(transition_long)

  global_transition <- matrix_to_long(
    prop_matrix = fit[["VTM"]],
    votes_matrix = fit[["VTM.votes"]],
    matrix_scope = "global",
    method = method
  )
  global_transition_complete <- matrix_to_long(
    prop_matrix = fit[["VTM.complete"]],
    votes_matrix = fit[["VTM.complete.votes"]],
    matrix_scope = "global_complete",
    method = method
  )

  checks <- make_nslphom_checks(
    fit,
    transition_long,
    block_id = NA_character_,
    method = method,
    threshold = threshold,
    blocked = FALSE
  )

  fit_bundle <- list(
    fit = fit,
    settings = settings,
    checks = checks,
    package = "lphom",
    package_version = as.character(utils::packageVersion("lphom"))
  )

  # Ergebnisse des unblocked Laufs speichern.
  saveRDS(fit_bundle, file.path(output_dir, "vorlaeufig_nslphom_unblocked_fit.rds"))
  saveRDS(transition_long, file.path(output_dir, "vorlaeufig_nslphom_unblocked_local_matrices_long.rds"))
  saveRDS(transition_wide, file.path(output_dir, "vorlaeufig_nslphom_unblocked_local_matrices_wide.rds"))
  saveRDS(global_transition, file.path(output_dir, "vorlaeufig_nslphom_unblocked_global_matrix.rds"))
  saveRDS(global_transition_complete, file.path(output_dir, "vorlaeufig_nslphom_unblocked_global_matrix_complete.rds"))
  saveRDS(checks, file.path(output_dir, "vorlaeufig_nslphom_unblocked_checks.rds"))

  message("Fertig. Ergebnisse gespeichert unter: ", output_dir)
}
