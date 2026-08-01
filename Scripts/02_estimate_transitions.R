library(dplyr)
library(tidyr)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")
source("Functions/nslphom_functions.R", encoding = "UTF-8")

ensure_data_dirs()

threshold <- getOption("waehlendenwanderung.party_threshold", 0.12)
iter_max <- getOption("waehlendenwanderung.nslphom_iter_max", 10L)
tol <- getOption("waehlendenwanderung.nslphom_tol", 1e-5)
solver <- getOption("waehlendenwanderung.nslphom_solver", "osqp")
solver <- match.arg(solver, c("osqp", "symphony", "lp_solve"))
run_fit <- isTRUE(getOption("waehlendenwanderung.nslphom_run_fit", TRUE))

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

saveRDS(settings, file.path(data_dir_model_nslphom, "vorlaeufig_nslphom_settings.rds"))

if (!run_fit) {
  message(
    "nslphom-Settings wurden gespeichert. ",
    "Der Fit wurde wegen option waehlendenwanderung.nslphom_run_fit = FALSE uebersprungen."
  )
} else {
  ids <- inputs$input2021$agg_schluessel
  origin_counts <- make_count_matrix(inputs$input2021)
  destination_counts <- make_count_matrix(inputs$input2025)

  message(
    "Starte nationalen nslphom-Lauf ohne Bloecke mit ",
    nrow(origin_counts),
    " Aggregationseinheiten, ",
    ncol(origin_counts),
    " Gruppen und Solver ",
    solver,
    "."
  )

  fit <- fit_nslphom_model(
    origin_counts,
    destination_counts,
    iter_max = iter_max,
    tol = tol,
    solver = solver,
    verbose = TRUE,
    method = paste0("nslphom_unblocked_", solver)
  )

  message("Bereite lokale und globale Uebergangsmatrizen auf.")

  write_main_unblocked_nslphom_outputs(
    fit = fit,
    ids = ids,
    settings = settings,
    method = paste0("nslphom_unblocked_", solver),
    threshold = threshold
  )

  message("nslphom-Hauptlauf ohne Bloecke abgeschlossen. Outputs liegen unter: ", data_dir_model_nslphom)
}
