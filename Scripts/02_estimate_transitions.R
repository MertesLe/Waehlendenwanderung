# nslphom_dual fuer Ostdeutschland ohne Berlin schaetzen und Matrizen speichern.

library(dplyr)
library(tidyr)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")
source("Functions/nslphom_functions.R", encoding = "UTF-8")

ensure_data_dirs()

threshold <- getOption("waehlendenwanderung.party_threshold", 0.12)
iter_max <- getOption("waehlendenwanderung.nslphom_iter_max", 3L)
tol <- getOption("waehlendenwanderung.nslphom_tol", 1e-5)
solver <- match.arg(
  getOption("waehlendenwanderung.nslphom_solver", "osqp"),
  c("osqp", "symphony", "lp_solve")
)
run_fit <- isTRUE(getOption("waehlendenwanderung.nslphom_run_fit", TRUE))
run_ehet <- isTRUE(getOption("waehlendenwanderung.ost_ehet_run", TRUE))

# Vorbereitete Wahlinputs einlesen.
files <- c(
  input2021 = file.path(data_dir_cleaned, "nslphom_input_2021.rds"),
  input2025 = file.path(data_dir_cleaned, "nslphom_input_2025.rds"),
  input_long = file.path(data_dir_cleaned, "nslphom_input_long.rds"),
  party_thresholds = file.path(data_dir_cleaned, "partei_schwellenwerte.rds"),
  input_checks = file.path(data_dir_validation, "nslphom_input_checks.rds")
)

missing_files <- files[!file.exists(files)]
if (length(missing_files) > 0) {
  stop(
    "Zentrale nslphom-Inputdateien fehlen. Fuehre zuerst ",
    "Scripts/01_prepare_nslphom_input.R aus. Fehlend: ",
    paste(missing_files, collapse = ", ")
  )
}

inputs <- list(
  input2021 = readRDS(files[["input2021"]]) %>% arrange(.data$agg_schluessel),
  input2025 = readRDS(files[["input2025"]]) %>% arrange(.data$agg_schluessel),
  input_long = readRDS(files[["input_long"]]),
  party_thresholds = readRDS(files[["party_thresholds"]]),
  input_checks = readRDS(files[["input_checks"]])
)

# Hauptanalyse auf die ostdeutschen Flaechenlaender begrenzen. Berlin bleibt
# wegen der nur gesamtstaedtisch vorliegenden Strukturwerte ausgeschlossen.
ost_ids <- inputs$input2021$agg_schluessel[
  is_ostdeutschland_ohne_berlin(inputs$input2021$agg_schluessel)
]

inputs$input2021 <- inputs$input2021 %>% filter(.data$agg_schluessel %in% ost_ids)
inputs$input2025 <- inputs$input2025 %>% filter(.data$agg_schluessel %in% ost_ids)
inputs$input_long <- inputs$input_long %>% filter(.data$agg_schluessel %in% ost_ids)
inputs$input_checks <- inputs$input_checks %>% filter(.data$agg_schluessel %in% ost_ids)

if (nrow(inputs$input2021) == 0 || any(substr(ost_ids, 1, 2) == "11")) {
  stop("Die Ostdeutschland-Filterung ohne Berlin ist nicht plausibel.")
}

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
    analysis_region = "ostdeutschland_ohne_berlin",
    included_state_prefixes = "12, 13, 14, 15, 16",
    berlin_included = FALSE
  )

message("Ost-Hauptanalyse umfasst ", nrow(inputs$input2021), " Aggregationseinheiten ohne Berlin.")

endoutput_path <- file.path(
  data_dir_model_nslphom_ost,
  "nslphom_ost_endoutput.rds"
)

if (!run_fit) {
  message(
    "Der Ost-nslphom_dual-Fit wurde wegen option ",
    "waehlendenwanderung.nslphom_run_fit = FALSE uebersprungen. ",
    "Es wurden keine Modelloutputs gespeichert."
  )
} else {
  ids <- inputs$input2021$agg_schluessel
  origin_counts <- make_count_matrix(inputs$input2021)
  destination_counts <- make_count_matrix(inputs$input2025)
  method <- paste0("nslphom_dual_ost_unblocked_", solver)

  message(
    "Starte Ost-nslphom_dual-Lauf ohne Bloecke mit ",
    nrow(origin_counts), " Aggregationseinheiten, ",
    ncol(origin_counts), " Gruppen und Solver ", solver, "."
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

  # Lokale und globale Uebergangsmatrizen aufbereiten.
  transition_long <- local_matrices_to_long(fit, ids, method = method) %>%
    arrange(.data$agg_schluessel, .data$from, .data$to)
  transition_wide <- make_transition_wide(transition_long)
  global_transition <- matrix_to_long(
    prop_matrix = fit[["VTM12.w"]],
    votes_matrix = fit[["VTM.votes.w"]],
    matrix_scope = "global",
    method = method
  )

  # Rekonstruktion und Zeilensummen der Schaetzung pruefen.
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
    analysis_region = "ostdeutschland_ohne_berlin",
    main_matrix_type = "HETe_weighted",
    package = "lphom",
    package_version = as.character(utils::packageVersion("lphom")),
    osqp_package_version = if (requireNamespace("osqp", quietly = TRUE)) {
      as.character(utils::packageVersion("osqp"))
    } else {
      NA_character_
    }
  )
  endoutput <- list(
    local_matrices_long = transition_long,
    global_matrix = global_transition,
    EHet = fit$EHet,
    EHet_ids = ids,
    settings = settings,
    checks = checks
  )

  # Ostoutputs getrennt von nationalen Diagnosefits speichern.
  saveRDS(settings, file.path(data_dir_model_nslphom_ost, "nslphom_settings.rds"))
  saveRDS(nslphom_fit, file.path(data_dir_model_nslphom_ost, "nslphom_fit.rds"))
  saveRDS(transition_long, file.path(data_dir_model_nslphom_ost, "transition_matrices_long.rds"))
  saveRDS(transition_wide, file.path(data_dir_model_nslphom_ost, "transition_matrices_wide.rds"))
  saveRDS(global_transition, file.path(data_dir_model_nslphom_ost, "nslphom_global_matrix.rds"))
  saveRDS(checks, file.path(data_dir_model_nslphom_ost, "transition_checks.rds"))
  saveRDS(endoutput, endoutput_path)

  message("Ost-Hauptlauf abgeschlossen. Outputs liegen unter: ", data_dir_model_nslphom_ost)
}

# Dieselbe EHet-Auswertung wie fuer den Deutschlandfit auf den Ost-Hauptfit anwenden.
if (run_ehet && file.exists(endoutput_path)) {
  old_options <- options(
    waehlendenwanderung.ehet_nslphom_output_path = endoutput_path,
    waehlendenwanderung.ehet_run_label = "ostdeutschland_ohne_berlin"
  )
  source("Scripts/09_visualize_homogenitaetsannahme.R", encoding = "UTF-8")
  options(old_options)
} else if (run_ehet) {
  message("EHet-Visualisierung uebersprungen, weil noch kein Ost-Endoutput vorliegt.")
}
