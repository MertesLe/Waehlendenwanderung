# Deutschlandweit nslphom_dual schaetzen und danach die EHet-Diagnose erstellen.

library(dplyr)
library(tidyr)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")
source("Functions/nslphom_functions.R", encoding = "UTF-8")

ensure_data_dirs()

threshold <- getOption("waehlendenwanderung.party_threshold", 0.12)
iter_max <- getOption("waehlendenwanderung.nslphom_iter_max", 2L)
tol <- getOption("waehlendenwanderung.nslphom_tol", 1e-5)
osqp_max_iter_deutschland <- 1000000L
solver <- match.arg(
  getOption("waehlendenwanderung.nslphom_solver", "osqp"),
  c("osqp", "symphony", "lp_solve")
)
run_fit <- isTRUE(getOption("waehlendenwanderung.deutschland_nslphom_run_fit", TRUE))
run_ehet <- isTRUE(getOption("waehlendenwanderung.deutschland_ehet_run", TRUE))

# Dieselben vorbereiteten Inputs wie im Ost-Hauptlauf verwenden.
inputs <- read_prepared_nslphom_inputs()

# Kruft (07137057) 2025: 2801 Waehlende bei nur 2667 Wahlberechtigten.
# Die Bundeswahlleiterin weist darauf hin, dass Wahlbezirksdaten auf Meldungen
# der Laender beruhen und nicht alle Inkonsistenzen beseitigt werden konnten (Hinweismaterial 
# der Daten 2025). Die Einheit wird im Deutschlandfit rausgelassen.
kruft_id <- "07137057"
kruft_check <- inputs$input_checks %>%
  filter(.data$Jahr == 2025, .data$agg_schluessel == kruft_id)
if (nrow(kruft_check) != 1 ||
    !isTRUE(kruft_check$flag_waehlende_groesser_wahlberechtigte[[1]])) {
  stop("Der Kruft-Sonderfall ist in den vorbereiteten Inputs nicht wie erwartet vorhanden.")
}

inputs$input2021 <- inputs$input2021 %>% filter(.data$agg_schluessel != kruft_id)
inputs$input2025 <- inputs$input2025 %>% filter(.data$agg_schluessel != kruft_id)
inputs$input_long <- inputs$input_long %>% filter(.data$agg_schluessel != kruft_id)
inputs$input_checks <- inputs$input_checks %>% filter(.data$agg_schluessel != kruft_id)

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
    berlin_included = TRUE,
    excluded_agg_schluessel = kruft_id,
    osqp_max_iter = if (solver == "osqp") osqp_max_iter_deutschland else NA_integer_,
  )

endoutput_path <- file.path(
  data_dir_model_nslphom_deutschland,
  "nslphom_deutschland_endoutput.rds"
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

  alte_osqp_optionen <- options(
    waehlendenwanderung.osqp_max_iter = osqp_max_iter_deutschland
  )
  
  fit <- tryCatch(
    fit_nslphom_dual_model(
      origin_counts,
      destination_counts,
      iter_max = iter_max,
      tol = tol,
      solver = solver,
      verbose = TRUE,
      method = method
    ),
    finally = options(alte_osqp_optionen)
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
  saveRDS(settings, file.path(data_dir_model_nslphom_deutschland, "nslphom_settings.rds"))
  saveRDS(nslphom_fit, file.path(data_dir_model_nslphom_deutschland, "nslphom_fit.rds"))
  saveRDS(transition_long, file.path(data_dir_model_nslphom_deutschland, "transition_matrices_long.rds"))
  saveRDS(transition_wide, file.path(data_dir_model_nslphom_deutschland, "transition_matrices_wide.rds"))
  saveRDS(global_transition, file.path(data_dir_model_nslphom_deutschland, "nslphom_global_matrix.rds"))
  saveRDS(checks, file.path(data_dir_model_nslphom_deutschland, "transition_checks.rds"))
  saveRDS(endoutput, endoutput_path)

  message("Deutschlandfit gespeichert unter: ", data_dir_model_nslphom_deutschland)
}

# Die bestehende EHet-Auswertung auf den gerade erzeugten Deutschlandfit anwenden.
if (run_ehet && file.exists(endoutput_path)) {
  old_options <- options(
    waehlendenwanderung.ehet_nslphom_output_path = endoutput_path,
    waehlendenwanderung.ehet_run_label = "deutschland"
  )
  source("Scripts/07b_visualize_homogenitaetsannahme.R", encoding = "UTF-8")
  options(old_options)
} else if (run_ehet) {
  message("EHet-Visualisierung uebersprungen, weil noch kein Deutschland-Endoutput vorliegt.")
}

