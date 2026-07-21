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

main <- function() {
  message("Lese zentral vorbereitete nslphom-Inputs mit ", threshold * 100, "%-Parteischwelle.")
  inputs <- read_prepared_nslphom_inputs()
  validation <- validate_prepared_nslphom_inputs(inputs, threshold = threshold)

  settings <- make_unblocked_settings(
    inputs = inputs,
    validation = validation,
    threshold = threshold,
    iter_max = iter_max,
    tol = tol,
    blocked = FALSE
  )

  write_unblocked_input_copies(inputs, settings, output_dir, write_csv = TRUE)

  if (!run_fit) {
    message(
      "Input-Kopien und Settings wurden gespeichert. ",
      "Der nslphom-Fit wurde wegen option waehlendenwanderung.unblocked_run_fit = FALSE uebersprungen."
    )
    return(invisible(settings))
  }

  ids <- inputs$input2021$agg_schluessel
  origin_counts <- make_count_matrix(inputs$input2021)
  destination_counts <- make_count_matrix(inputs$input2025)

  message(
    "Starte nationalen nslphom-Lauf ohne Bloecke mit ",
    nrow(origin_counts),
    " Aggregationseinheiten und ",
    ncol(origin_counts),
    " Gruppen."
  )
  message("Dieser Schritt ist speicherintensiv und fuer den leistungsstaerkeren PC gedacht.")

  fit <- fit_nslphom_model(
    origin_counts,
    destination_counts,
    iter_max = iter_max,
    tol = tol,
    verbose = TRUE,
    method = "lphom::nslphom_unblocked"
  )

  message("Bereite lokale und globale Uebergangsmatrizen auf.")
  fit_bundle <- write_unblocked_nslphom_outputs(
    fit = fit,
    ids = ids,
    output_dir = output_dir,
    settings = settings,
    method = "lphom::nslphom_unblocked",
    threshold = threshold,
    write_csv = TRUE
  )

  message("Fertig. Ergebnisse gespeichert unter: ", output_dir)
  invisible(fit_bundle)
}

main()
