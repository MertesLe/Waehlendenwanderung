# nslphom_dual auf einer waehlbaren Teilmenge testen und nur den kompakten Endoutput speichern.

library(dplyr)
library(tidyr)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")
source("Functions/nslphom_functions.R", encoding = "UTF-8")

ensure_data_dirs()
check_lphom_available()

if (!exists("nslphom_dual", where = asNamespace("lphom"), inherits = FALSE)) {
  stop("lphom::nslphom_dual() ist in der installierten lphom-Version nicht verfuegbar.")
}

n_test_units <- getOption("waehlendenwanderung.test_dual_nslphom_units", 20L)
selection_mode <- getOption("waehlendenwanderung.test_dual_nslphom_selection", "first")
selection_mode <- match.arg(selection_mode, c("first", "last", "random"))
seed <- getOption("waehlendenwanderung.test_dual_nslphom_seed", 42L)
threshold <- getOption("waehlendenwanderung.party_threshold", 0.12)
iter_max <- getOption("waehlendenwanderung.test_dual_nslphom_iter_max", 10L)
tol <- getOption("waehlendenwanderung.test_dual_nslphom_tol", 1e-5)
solver <- getOption("waehlendenwanderung.test_dual_nslphom_solver", "lp_solve")
solver <- match.arg(solver, c("lp_solve", "symphony"))
method_label <- "lphom::nslphom_dual"

output_dir <- getOption(
  "waehlendenwanderung.test_dual_nslphom_output_dir",
  file.path(data_dir_model_nslphom, paste0("test", n_test_units, "_", selection_mode, "_dual"))
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Lokale absolute Dual-Matrizen in das einheitliche Longformat ueberfuehren.
make_local_dual_matrices_long <- function(votes_units, ids, from_names, to_names, matrix_type) {
  stopifnot(length(dim(votes_units)) == 3)
  stopifnot(dim(votes_units)[[3]] == length(ids))
  stopifnot(dim(votes_units)[[1]] == length(from_names))
  stopifnot(dim(votes_units)[[2]] == length(to_names))

  dimnames(votes_units) <- list(from_names, to_names, ids)

  bind_rows(lapply(seq_along(ids), function(i) {
    votes_matrix <- votes_units[, , i]
    origin_count <- rowSums(votes_matrix, na.rm = TRUE)
    destination_count <- colSums(votes_matrix, na.rm = TRUE)

    prop_matrix <- sweep(votes_matrix, 1, origin_count, "/")
    prop_matrix[!is.finite(prop_matrix)] <- 0

    matrix_cells <- as.data.frame(as.table(prop_matrix), stringsAsFactors = FALSE) %>%
      mutate(
        from = as.character(.data$Var1),
        to = as.character(.data$Var2),
        row_index = match(.data$from, rownames(votes_matrix)),
        col_index = match(.data$to, colnames(votes_matrix))
      )

    matrix_cells %>%
      transmute(
        agg_schluessel = ids[[i]],
        from = .data$from,
        to = .data$to,
        transition_probability = as.numeric(.data$Freq),
        origin_count = as.numeric(origin_count[.data$from]),
        destination_count = as.numeric(destination_count[.data$to]),
        estimated_transition_count = as.numeric(votes_matrix[cbind(.data$row_index, .data$col_index)]),
        matrix_type = matrix_type,
        method = method_label
      )
  }))
}

# Eine globale Dual-Matrix mit Wahrscheinlichkeiten und absoluten Zahlen lang formatieren.
make_global_dual_matrix_long <- function(prop_matrix, votes_matrix, matrix_scope, matrix_type) {
  prop_matrix <- as.matrix(prop_matrix)
  votes_matrix <- as.matrix(votes_matrix)

  matrix_to_long(
    prop_matrix = prop_matrix,
    votes_matrix = votes_matrix,
    matrix_scope = matrix_scope,
    method = paste(method_label, matrix_type, sep = "_")
  ) %>%
    mutate(matrix_type = matrix_type, .before = method)
}

# EHet der globalen Dual-Matrix je Einheit berechnen und relativ zusammenfassen.
make_dual_ehet <- function(origin_counts, destination_counts, prop_matrix, ids, ehet_type) {
  prop_matrix <- as.matrix(prop_matrix)
  expected_destination <- origin_counts %*% prop_matrix
  ehet_matrix <- destination_counts - expected_destination

  rownames(ehet_matrix) <- ids
  colnames(ehet_matrix) <- colnames(destination_counts)

  ehet_long <- as.data.frame(as.table(ehet_matrix), stringsAsFactors = FALSE) %>%
    transmute(
      agg_schluessel = as.character(Var1),
      to = as.character(Var2),
      ehet_deviation_count = as.numeric(Freq),
      abs_ehet_deviation_count = abs(ehet_deviation_count),
      ehet_type = ehet_type,
      method = method_label
    )

  wahlberechtigte <- tibble(
    agg_schluessel = ids,
    wahlberechtigte = as.numeric(rowSums(origin_counts, na.rm = TRUE))
  )

  ehet_unit_metrics <- ehet_long %>%
    group_by(agg_schluessel) %>%
    summarise(
      ehet_abs_sum = sum(abs_ehet_deviation_count, na.rm = TRUE),
      ehet_abs_half = 0.5 * ehet_abs_sum,
      max_abs_cell_deviation = max(abs_ehet_deviation_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(wahlberechtigte, by = "agg_schluessel") %>%
    mutate(
      ehet_index = ehet_abs_half / wahlberechtigte,
      ehet_index_percent = 100 * ehet_index,
      ehet_type = ehet_type,
      method = method_label
    ) %>%
    arrange(desc(ehet_index))

  list(
    matrix = ehet_matrix,
    long = ehet_long,
    unit_metrics = ehet_unit_metrics
  )
}

inputs <- read_prepared_nslphom_inputs()
validation <- validate_prepared_nslphom_inputs(inputs, threshold = threshold)
stopifnot(nrow(inputs$input2021) >= n_test_units)

all_ids <- inputs$input2021$agg_schluessel

# Fuer Belastungstests kann derselbe Code erste, letzte oder zufaellige
# Aggregationseinheiten schaetzen. Wichtig ist danach die identische Reihenfolge
# in 2021 und 2025.
selected_ids <- switch(
  selection_mode,
  first = head(all_ids, n_test_units),
  last = tail(all_ids, n_test_units),
  random = {
    set.seed(seed)
    sample(all_ids, size = n_test_units, replace = FALSE)
  }
)

input2021_test <- inputs$input2021 %>%
  filter(agg_schluessel %in% selected_ids) %>%
  arrange(match(agg_schluessel, selected_ids))

input2025_test <- inputs$input2025 %>%
  filter(agg_schluessel %in% selected_ids) %>%
  arrange(match(agg_schluessel, selected_ids))

stopifnot(identical(input2021_test$agg_schluessel, input2025_test$agg_schluessel))

ids <- input2021_test$agg_schluessel
origin_counts <- make_count_matrix(input2021_test)
destination_counts <- make_count_matrix(input2025_test)
groups <- validation$group_names

rm(inputs, input2021_test, input2025_test)
gc()

message(
  "Starte test nslphom_dual mit ",
  nrow(origin_counts),
  " Aggregationseinheiten und ",
  ncol(origin_counts),
  " Gruppen mit Solver ",
  solver,
  "."
)

fit <- lphom::nslphom_dual(
  votes_election1 = as.data.frame(origin_counts),
  votes_election2 = as.data.frame(destination_counts),
  iter.max = iter_max,
  min.first = FALSE,
  integers = FALSE,
  solver = solver,
  tol = tol
)

message("Bereite nslphom_dual-Endoutput auf.")

from_names <- colnames(origin_counts)
to_names <- colnames(destination_counts)

# Hauptvariante: HET-gewichtete Kombination der beiden Richtungen.
local_matrices_long <- make_local_dual_matrices_long(
  votes_units = fit$VTM.votes.units.w,
  ids = ids,
  from_names = from_names,
  to_names = to_names,
  matrix_type = "weighted"
) %>%
  arrange(agg_schluessel, from, to)

local_matrices_wide <- make_transition_wide(local_matrices_long)

global_matrix <- make_global_dual_matrix_long(
  prop_matrix = fit$VTM12.w,
  votes_matrix = fit$VTM.votes.w,
  matrix_scope = "global",
  matrix_type = "weighted"
)

ehet_weighted <- make_dual_ehet(
  origin_counts = origin_counts,
  destination_counts = destination_counts,
  prop_matrix = fit$VTM12.w,
  ids = ids,
  ehet_type = "dual_weighted"
)

# Zusatzvariante: einfache arithmetische Kombination der beiden Richtungen.
local_matrices_long_average <- make_local_dual_matrices_long(
  votes_units = fit$VTM.votes.units.a,
  ids = ids,
  from_names = from_names,
  to_names = to_names,
  matrix_type = "average"
) %>%
  arrange(agg_schluessel, from, to)

global_matrix_average <- make_global_dual_matrix_long(
  prop_matrix = fit$VTM12.a,
  votes_matrix = fit$VTM.votes.a,
  matrix_scope = "global",
  matrix_type = "average"
)

ehet_average <- make_dual_ehet(
  origin_counts = origin_counts,
  destination_counts = destination_counts,
  prop_matrix = fit$VTM12.a,
  ids = ids,
  ehet_type = "dual_average"
)

settings <- tibble(
  threshold = threshold,
  n_units = length(ids),
  groups = paste(groups, collapse = ", "),
  n_groups = length(groups),
  iter_max = iter_max,
  tol = tol,
  new_and_exit_voters = "simultaneous",
  solver = solver,
  selection_mode = selection_mode,
  seed = if (selection_mode == "random") seed else NA_integer_,
  main_matrix_type = "weighted",
  note = "nslphom_dual schaetzt intern 2021->2025 und 2025->2021 und kombiniert die lokalen Matrizen."
)

endoutput <- list(
  local_matrices_long = local_matrices_long,
  local_matrices_wide = local_matrices_wide,
  global_matrix = global_matrix,
  EHet = ehet_weighted$matrix,
  EHet_long = ehet_weighted$long,
  EHet_unit_metrics = ehet_weighted$unit_metrics,
  local_matrices_long_average = local_matrices_long_average,
  global_matrix_average = global_matrix_average,
  EHet_average = ehet_average$matrix,
  EHet_average_long = ehet_average$long,
  EHet_average_unit_metrics = ehet_average$unit_metrics,
  EHet_2021_to_2025 = fit$nslphom.object.12$EHet,
  EHet_2025_to_2021 = fit$nslphom.object.21$EHet,
  EHet_ids = ids,
  settings = settings
)

output_file <- file.path(
  output_dir,
  paste0("vorlaeufig_test", n_test_units, "_", selection_mode, "_nslphom_dual_endoutput.rds")
)

saveRDS(endoutput, output_file)

rm(fit)
gc()

message("Fertig. Endoutput gespeichert unter: ", output_file)
