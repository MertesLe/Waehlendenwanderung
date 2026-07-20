library(dplyr)
library(tidyr)

source("paths.R", encoding = "UTF-8")

ensure_data_dirs()

threshold <- 0.12
output_dir <- file.path("Data", "modeloutput", "nslphom_unblocked")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

run_fit <- isTRUE(getOption("waehlendenwanderung.unblocked_run_fit", TRUE))
iter_max <- getOption("waehlendenwanderung.unblocked_nslphom_iter_max", 10L)
tol <- getOption("waehlendenwanderung.unblocked_nslphom_tol", 1e-5)

read_prepared_inputs <- function() {
  files <- c(
    input2021 = file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_2021.rds"),
    input2025 = file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_2025.rds"),
    input_long = file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_long.rds"),
    party_thresholds = file.path(data_dir_cleaned, "vorlaeufig_partei_schwellenwerte.rds"),
    input_checks = file.path(data_dir_validation, "vorlaeufig_nslphom_input_checks.rds")
  )

  missing_files <- files[!file.exists(files)]

  if (length(missing_files) > 0) {
    stop(
      "Zentrale nslphom-Inputdateien fehlen. Fuehre zuerst ",
      "Scripts/01_prepare_nslphom_input.R aus. Fehlend: ",
      paste(missing_files, collapse = ", ")
    )
  }

  list(
    input2021 = readRDS(files[["input2021"]]) %>% arrange(agg_schluessel),
    input2025 = readRDS(files[["input2025"]]) %>% arrange(agg_schluessel),
    input_long = readRDS(files[["input_long"]]),
    party_thresholds = readRDS(files[["party_thresholds"]]),
    input_checks = readRDS(files[["input_checks"]])
  )
}

make_count_matrix <- function(data, id_col = "agg_schluessel") {
  mat <- data %>%
    select(-all_of(id_col)) %>%
    as.matrix()

  storage.mode(mat) <- "numeric"
  rownames(mat) <- data[[id_col]]

  mat
}

fit_nslphom_unblocked <- function(origin_counts, destination_counts) {
  if (!requireNamespace("lphom", quietly = TRUE)) {
    stop(
      "Das Paket 'lphom' ist nicht installiert. ",
      "Installiere es mit install.packages('lphom'), damit lphom::nslphom() laufen kann."
    )
  }

  lphom::nslphom(
    votes_election1 = as.data.frame(origin_counts),
    votes_election2 = as.data.frame(destination_counts),
    new_and_exit_voters = "raw",
    apriori = NULL,
    uniform = TRUE,
    iter.max = iter_max,
    min.first = FALSE,
    structural_zeros = NULL,
    integers = FALSE,
    distance.local = "abs",
    verbose = TRUE,
    solver = "lp_solve",
    burnin = 0,
    tol = tol
  )
}

local_matrices_to_long <- function(fit, ids) {
  prop_units <- fit[["VTM.prop.units"]]
  votes_units <- fit[["VTM.votes.units"]]

  stopifnot(length(dim(prop_units)) == 3)
  stopifnot(identical(dim(prop_units), dim(votes_units)))
  stopifnot(dim(prop_units)[[3]] == length(ids))

  bind_rows(lapply(seq_along(ids), function(i) {
    prop_matrix <- prop_units[, , i]
    votes_matrix <- votes_units[, , i]

    origin_count <- rowSums(votes_matrix, na.rm = TRUE)
    destination_count <- colSums(votes_matrix, na.rm = TRUE)
    matrix_cells <- as.data.frame(as.table(prop_matrix), stringsAsFactors = FALSE) %>%
      mutate(
        from = as.character(Var1),
        to = as.character(Var2),
        row_index = match(from, rownames(votes_matrix)),
        col_index = match(to, colnames(votes_matrix))
      )

    matrix_cells %>%
      transmute(
        agg_schluessel = ids[[i]],
        from,
        to,
        transition_probability = as.numeric(Freq),
        origin_count = as.numeric(origin_count[from]),
        destination_count = as.numeric(destination_count[to]),
        estimated_transition_count = as.numeric(votes_matrix[cbind(row_index, col_index)]),
        method = "lphom::nslphom_unblocked"
      )
  }))
}

matrix_to_long <- function(prop_matrix, votes_matrix, matrix_scope) {
  origin_count <- rowSums(votes_matrix, na.rm = TRUE)
  destination_count <- colSums(votes_matrix, na.rm = TRUE)
  matrix_cells <- as.data.frame(as.table(prop_matrix), stringsAsFactors = FALSE) %>%
    mutate(
      from = as.character(Var1),
      to = as.character(Var2),
      row_index = match(from, rownames(votes_matrix)),
      col_index = match(to, colnames(votes_matrix))
    )

  matrix_cells %>%
    transmute(
      matrix_scope = matrix_scope,
      from,
      to,
      transition_probability = as.numeric(Freq),
      origin_count = as.numeric(origin_count[from]),
      destination_count = as.numeric(destination_count[to]),
      estimated_transition_count = as.numeric(votes_matrix[cbind(row_index, col_index)]),
      method = "lphom::nslphom_unblocked"
    )
}

make_nslphom_checks <- function(fit, transition_long) {
  prop_units <- fit[["VTM.prop.units"]]
  votes_units <- fit[["VTM.votes.units"]]
  origin_used <- as.matrix(fit[["origin"]])
  destination_used <- as.matrix(fit[["destination"]])

  origin_from_units <- t(vapply(
    seq_len(dim(votes_units)[[3]]),
    function(i) rowSums(votes_units[, , i], na.rm = TRUE),
    numeric(dim(votes_units)[[1]])
  ))
  colnames(origin_from_units) <- dimnames(votes_units)[[1]]

  destination_from_units <- t(vapply(
    seq_len(dim(votes_units)[[3]]),
    function(i) colSums(votes_units[, , i], na.rm = TRUE),
    numeric(dim(votes_units)[[2]])
  ))
  colnames(destination_from_units) <- dimnames(votes_units)[[2]]

  origin_used <- origin_used[, colnames(origin_from_units), drop = FALSE]
  destination_used <- destination_used[, colnames(destination_from_units), drop = FALSE]

  tibble::tibble(
    n_units = dim(prop_units)[[3]],
    n_origin_groups = dim(prop_units)[[1]],
    n_destination_groups = dim(prop_units)[[2]],
    max_abs_row_sum_error = transition_long %>%
      group_by(agg_schluessel, from) %>%
      summarise(
        origin_count = max(origin_count, na.rm = TRUE),
        row_sum = sum(transition_probability),
        .groups = "drop"
      ) %>%
      filter(origin_count > 0) %>%
      summarise(value = max(abs(row_sum - 1), na.rm = TRUE)) %>%
      pull(value),
    max_abs_origin_reconstruction_error = max(abs(origin_from_units - origin_used), na.rm = TRUE),
    max_abs_destination_reconstruction_error = max(abs(destination_from_units - destination_used), na.rm = TRUE),
    iter = fit[["iter"]],
    iter_min = fit[["iter.min"]],
    HETe = fit[["HETe"]],
    HETe_init = fit[["solution_init"]][["HETe_init"]],
    method = "lphom::nslphom_unblocked",
    package = "lphom",
    package_version = as.character(utils::packageVersion("lphom")),
    new_and_exit_voters = "raw",
    threshold = threshold,
    blocked = FALSE
  )
}

write_input_copies <- function(inputs, settings) {
  saveRDS(inputs$input2021, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_2021.rds"))
  saveRDS(inputs$input2025, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_2025.rds"))
  saveRDS(inputs$input_long, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_long.rds"))
  saveRDS(inputs$party_thresholds, file.path(output_dir, "vorlaeufig_nslphom_unblocked_party_thresholds.rds"))
  saveRDS(inputs$input_checks, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_checks.rds"))
  saveRDS(settings, file.path(output_dir, "vorlaeufig_nslphom_unblocked_settings.rds"))

  write.csv(inputs$input2021, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_2021.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(inputs$input2025, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_2025.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(inputs$input_long, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_long.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(inputs$party_thresholds, file.path(output_dir, "vorlaeufig_nslphom_unblocked_party_thresholds.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(inputs$input_checks, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_checks.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(settings, file.path(output_dir, "vorlaeufig_nslphom_unblocked_settings.csv"), row.names = FALSE, fileEncoding = "UTF-8")
}

main <- function() {
  message("Lese zentral vorbereitete nslphom-Inputs mit 12%-Parteischwelle.")
  inputs <- read_prepared_inputs()

  stopifnot(identical(inputs$input2021$agg_schluessel, inputs$input2025$agg_schluessel))
  stopifnot(identical(names(inputs$input2021), names(inputs$input2025)))
  stopifnot(all(abs(inputs$input_checks$differenz_input_zu_referenz) < 1e-8))
  stopifnot(all(abs(inputs$input_checks$differenz_stimmen_zu_waehlenden) < 1e-8))

  group_names <- setdiff(names(inputs$input2021), "agg_schluessel")
  threshold_check <- inputs$party_thresholds %>%
    mutate(expected_keep_party_year = national_share_valid >= threshold) %>%
    filter(keep_party_year != expected_keep_party_year)

  if (nrow(threshold_check) > 0) {
    stop(
      "Die zentralen nslphom-Inputs passen nicht zur 12%-Schwelle. ",
      "Fuehre zuerst Scripts/01_prepare_nslphom_input.R neu aus."
    )
  }

  kept_parties <- inputs$party_thresholds %>%
    filter(keep_party) %>%
    pull(party) %>%
    unique() %>%
    sort()
  expected_kept_parties <- inputs$party_thresholds %>%
    group_by(party) %>%
    summarise(
      expected_keep_party = any(national_share_valid >= threshold),
      .groups = "drop"
    ) %>%
    filter(expected_keep_party) %>%
    pull(party) %>%
    sort()

  stopifnot(identical(kept_parties, expected_kept_parties))
  stopifnot(setequal(c(kept_parties, "Andere", "Nichtwaehler"), group_names))
  stopifnot(all(make_count_matrix(inputs$input2021) >= 0, na.rm = TRUE))
  stopifnot(all(make_count_matrix(inputs$input2025) >= 0, na.rm = TRUE))

  settings <- tibble::tibble(
    threshold = threshold,
    keep_parties = paste(kept_parties, collapse = ", "),
    groups = paste(group_names, collapse = ", "),
    blocked = FALSE,
    n_units = nrow(inputs$input2021),
    n_groups = length(group_names),
    iter_max = iter_max,
    tol = tol,
    new_and_exit_voters = "raw",
    solver = "lp_solve"
  )

  write_input_copies(inputs, settings)

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

  fit <- fit_nslphom_unblocked(origin_counts, destination_counts)

  message("Bereite lokale und globale Uebergangsmatrizen auf.")
  transition_long <- local_matrices_to_long(fit, ids) %>%
    arrange(
      agg_schluessel,
      from,
      to
    )

  transition_wide <- transition_long %>%
    mutate(
      transition = paste0("p_", from, "_to_", to)
    ) %>%
    select(
      agg_schluessel,
      transition,
      transition_probability
    ) %>%
    pivot_wider(
      names_from = transition,
      values_from = transition_probability
    ) %>%
    arrange(agg_schluessel)

  global_transition <- matrix_to_long(
    prop_matrix = fit[["VTM"]],
    votes_matrix = fit[["VTM.votes"]],
    matrix_scope = "global"
  )

  global_transition_complete <- matrix_to_long(
    prop_matrix = fit[["VTM.complete"]],
    votes_matrix = fit[["VTM.complete.votes"]],
    matrix_scope = "global_complete"
  )

  checks <- make_nslphom_checks(fit, transition_long)

  fit_bundle <- list(
    fit = fit,
    settings = settings,
    checks = checks,
    package = "lphom",
    package_version = as.character(utils::packageVersion("lphom"))
  )

  saveRDS(fit_bundle, file.path(output_dir, "vorlaeufig_nslphom_unblocked_fit.rds"))
  saveRDS(transition_long, file.path(output_dir, "vorlaeufig_nslphom_unblocked_local_matrices_long.rds"))
  saveRDS(transition_wide, file.path(output_dir, "vorlaeufig_nslphom_unblocked_local_matrices_wide.rds"))
  saveRDS(global_transition, file.path(output_dir, "vorlaeufig_nslphom_unblocked_global_matrix.rds"))
  saveRDS(global_transition_complete, file.path(output_dir, "vorlaeufig_nslphom_unblocked_global_matrix_complete.rds"))
  saveRDS(checks, file.path(output_dir, "vorlaeufig_nslphom_unblocked_checks.rds"))

  write.csv(transition_long, file.path(output_dir, "vorlaeufig_nslphom_unblocked_local_matrices_long.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(transition_wide, file.path(output_dir, "vorlaeufig_nslphom_unblocked_local_matrices_wide.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(global_transition, file.path(output_dir, "vorlaeufig_nslphom_unblocked_global_matrix.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(global_transition_complete, file.path(output_dir, "vorlaeufig_nslphom_unblocked_global_matrix_complete.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(checks, file.path(output_dir, "vorlaeufig_nslphom_unblocked_checks.csv"), row.names = FALSE, fileEncoding = "UTF-8")

  message("Fertig. Ergebnisse gespeichert unter: ", output_dir)
  invisible(fit_bundle)
}

main()
