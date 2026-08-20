# Deutschlandweit nslphom_dual schaetzen und danach die EHet-Diagnose erstellen.

library(dplyr)
library(tidyr)

source("paths.R", encoding = "UTF-8")
source("Functions/nslphom_functions.R", encoding = "UTF-8")

ensure_data_dirs()

threshold <- getOption("waehlendenwanderung.party_threshold", 0.12)
iter_max <- getOption("waehlendenwanderung.nslphom_iter_max", 10L)
tol <- getOption("waehlendenwanderung.nslphom_tol", 1e-5)
solver <- match.arg(
  getOption("waehlendenwanderung.nslphom_solver", "osqp"),
  c("osqp", "symphony", "lp_solve")
)
run_fit <- isTRUE(getOption("waehlendenwanderung.deutschland_nslphom_run_fit", TRUE))
run_ehet <- isTRUE(getOption("waehlendenwanderung.deutschland_ehet_run", TRUE))

# Dieselben vorbereiteten Inputs wie im Ost-Hauptlauf verwenden, aber nicht regional filtern.
inputs <- read_prepared_nslphom_inputs()
validation <- validate_prepared_nslphom_inputs(inputs, threshold = threshold)
settings <- make_unblocked_settings(
  inputs = inputs,
  validation = validation,
  threshold = threshold,
  iter_max = iter_max,
  tol = tol,
  solver = solver,
  blocked = FALSE,
  model = "nslphom_dual"
) %>%
  mutate(
    analysis_region = "deutschland",
    berlin_included = TRUE
  )

endoutput_path <- file.path(
  data_dir_model_nslphom_deutschland,
  "vorlaeufig_nslphom_deutschland_endoutput.rds"
)

if (!run_fit) {
  message(
    "Der Deutschlandfit wurde wegen option ",
    "waehlendenwanderung.deutschland_nslphom_run_fit = FALSE uebersprungen."
  )
} else {
  ids <- inputs$input2021$agg_schluessel
  origin_counts <- make_count_matrix(inputs$input2021)
  destination_counts <- make_count_matrix(inputs$input2025)
  method <- paste0("nslphom_dual_deutschland_unblocked_", solver)

  message(
    "Starte Deutschlandfit mit ", nrow(origin_counts),
    " Aggregationseinheiten und Solver ", solver, "."
  )

  fit <- fit_nslphom_dual_model(
    origin_counts,
    destination_counts,
    iter_max = iter_max,
    tol = tol,
    solver = solver,
    verbose = TRUE,
    method = method
  )

  # Lokale Matrizen, globale Matrix und EHet fuer die Deutschlanddiagnose aufbereiten.
  transition_long <- local_matrices_to_long(fit, ids, method = method) %>%
    arrange(.data$agg_schluessel, .data$from, .data$to)
  transition_wide <- make_transition_wide(transition_long)
  global_transition <- matrix_to_long(
    prop_matrix = fit[["VTM12.w"]],
    votes_matrix = fit[["VTM.votes.w"]],
    matrix_scope = "global",
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

  nslphom_fit <- list(
    fit = fit,
    settings = settings,
    checks = checks,
    global_matrix = global_transition,
    model = "nslphom_dual",
    analysis_region = "deutschland",
    main_matrix_type = "HETe_weighted"
  )
  endoutput <- list(
    local_matrices_long = transition_long,
    global_matrix = global_transition,
    EHet = fit$EHet,
    EHet_ids = ids,
    settings = settings,
    checks = checks
  )

  # Nationale Outputs getrennt vom Ost-Hauptfit speichern.
  saveRDS(settings, file.path(data_dir_model_nslphom_deutschland, "vorlaeufig_nslphom_settings.rds"))
  saveRDS(nslphom_fit, file.path(data_dir_model_nslphom_deutschland, "vorlaeufig_nslphom_fit.rds"))
  saveRDS(transition_long, file.path(data_dir_model_nslphom_deutschland, "vorlaeufig_transition_matrices_long.rds"))
  saveRDS(transition_wide, file.path(data_dir_model_nslphom_deutschland, "vorlaeufig_transition_matrices_wide.rds"))
  saveRDS(global_transition, file.path(data_dir_model_nslphom_deutschland, "vorlaeufig_nslphom_global_matrix.rds"))
  saveRDS(checks, file.path(data_dir_model_nslphom_deutschland, "vorlaeufig_transition_checks.rds"))
  saveRDS(endoutput, endoutput_path)

  message("Deutschlandfit gespeichert unter: ", data_dir_model_nslphom_deutschland)
}

# Die bestehende EHet-Auswertung auf den gerade erzeugten Deutschlandfit anwenden.
if (run_ehet && file.exists(endoutput_path)) {
  old_options <- options(
    waehlendenwanderung.ehet_nslphom_output_path = endoutput_path,
    waehlendenwanderung.ehet_run_label = "deutschland"
  )
  source("Scripts/09_visualize_homogenitaetsannahme.R", encoding = "UTF-8")
  options(old_options)
} else if (run_ehet) {
  message("EHet-Visualisierung uebersprungen, weil noch kein Deutschland-Endoutput vorliegt.")
}
