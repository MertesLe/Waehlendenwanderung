library(dplyr)
library(tidyr)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")
source("Functions/nslphom_functions.R", encoding = "UTF-8")

ensure_data_dirs()

n_test_units <- 2000L
threshold <- getOption("waehlendenwanderung.party_threshold", 0.12)
iter_max <- getOption("waehlendenwanderung.test2000_nslphom_iter_max", 10L)
tol <- getOption("waehlendenwanderung.test2000_nslphom_tol", 1e-5)
output_dir <- file.path(data_dir_model_nslphom, "test2000")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

check_lphom_available()

inputs <- read_prepared_nslphom_inputs()
validation <- validate_prepared_nslphom_inputs(inputs, threshold = threshold)
stopifnot(nrow(inputs$input2021) >= n_test_units)

# Fuer den reinen Belastungstest werden reproduzierbar die ersten 2000
# sortierten agg_schluessel genommen; fachliche Auswahl spielt hier keine Rolle.
input2021_test <- inputs$input2021 %>%
  slice_head(n = n_test_units)
input2025_test <- inputs$input2025 %>%
  slice_head(n = n_test_units)

ids <- input2021_test$agg_schluessel
origin_counts <- make_count_matrix(input2021_test)
destination_counts <- make_count_matrix(input2025_test)
groups <- validation$group_names

rm(inputs, input2021_test, input2025_test)
gc()

message(
  "Starte test2000 nslphom unblocked mit ",
  nrow(origin_counts),
  " Aggregationseinheiten und ",
  ncol(origin_counts),
  " Gruppen."
)

fit <- fit_nslphom_model(
  origin_counts,
  destination_counts,
  iter_max = iter_max,
  tol = tol,
  verbose = TRUE,
  method = "lphom::nslphom_unblocked_test2000"
)

message("Bereite Endoutput mit lokalen und globalen Uebergangswahrscheinlichkeiten auf.")

local_matrices_long <- local_matrices_to_long(
  fit,
  ids,
  method = "lphom::nslphom_unblocked_test2000"
) %>%
  arrange(
    agg_schluessel,
    from,
    to
  )

local_matrices_wide <- make_transition_wide(local_matrices_long)

global_matrix <- matrix_to_long(
  prop_matrix = fit[["VTM"]],
  votes_matrix = fit[["VTM.votes"]],
  matrix_scope = "global",
  method = "lphom::nslphom_unblocked_test2000"
)

global_matrix_complete <- matrix_to_long(
  prop_matrix = fit[["VTM.complete"]],
  votes_matrix = fit[["VTM.complete.votes"]],
  matrix_scope = "global_complete",
  method = "lphom::nslphom_unblocked_test2000"
)

settings <- tibble::tibble(
  threshold = threshold,
  n_units = length(ids),
  groups = paste(groups, collapse = ", "),
  n_groups = length(groups),
  iter_max = iter_max,
  tol = tol,
  new_and_exit_voters = "simultaneous",
  solver = "lp_solve",
  selection = "erste 2000 sortierte agg_schluessel"
)

endoutput <- list(
  local_matrices_long = local_matrices_long,
  local_matrices_wide = local_matrices_wide,
  global_matrix = global_matrix,
  global_matrix_complete = global_matrix_complete,
  settings = settings
)

saveRDS(
  endoutput,
  file.path(output_dir, "vorlaeufig_test2000_nslphom_unblocked_endoutput.rds")
)

message("Fertig. Endoutput gespeichert unter: ", output_dir)
