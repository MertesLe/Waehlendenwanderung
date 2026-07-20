library(dplyr)
library(tidyr)

source("paths.R", encoding = "UTF-8")

ensure_data_dirs()

n_test_units <- 2000L
threshold <- 0.12
iter_max <- getOption("waehlendenwanderung.test2000_nslphom_iter_max", 10L)
tol <- getOption("waehlendenwanderung.test2000_nslphom_tol", 1e-5)
output_dir <- file.path(data_dir_model_nslphom, "test2000")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

make_count_matrix <- function(data, id_col = "agg_schluessel") {
  mat <- data %>%
    select(-all_of(id_col)) %>%
    as.matrix()

  storage.mode(mat) <- "numeric"
  rownames(mat) <- data[[id_col]]

  mat
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
        method = "lphom::nslphom_unblocked_test2000"
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
      method = "lphom::nslphom_unblocked_test2000"
    )
}

if (!requireNamespace("lphom", quietly = TRUE)) {
  stop(
    "Das Paket 'lphom' ist nicht installiert. ",
    "Installiere es mit install.packages('lphom'), damit lphom::nslphom() laufen kann."
  )
}

input2021 <- readRDS(file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_2021.rds")) %>%
  arrange(agg_schluessel)
input2025 <- readRDS(file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_2025.rds")) %>%
  arrange(agg_schluessel)

stopifnot(identical(input2021$agg_schluessel, input2025$agg_schluessel))
stopifnot(identical(names(input2021), names(input2025)))
stopifnot(nrow(input2021) >= n_test_units)

# Fuer den reinen Belastungstest werden reproduzierbar die ersten 2000
# sortierten agg_schluessel genommen; fachliche Auswahl spielt hier keine Rolle.
input2021_test <- input2021 %>%
  slice_head(n = n_test_units)
input2025_test <- input2025 %>%
  slice_head(n = n_test_units)

ids <- input2021_test$agg_schluessel
origin_counts <- make_count_matrix(input2021_test)
destination_counts <- make_count_matrix(input2025_test)
groups <- colnames(origin_counts)

rm(input2021, input2025, input2021_test, input2025_test)
gc()

message(
  "Starte test2000 nslphom unblocked mit ",
  nrow(origin_counts),
  " Aggregationseinheiten und ",
  ncol(origin_counts),
  " Gruppen."
)

fit <- lphom::nslphom(
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

message("Bereite Endoutput mit lokalen und globalen Uebergangswahrscheinlichkeiten auf.")

local_matrices_long <- local_matrices_to_long(fit, ids) %>%
  arrange(
    agg_schluessel,
    from,
    to
  )

local_matrices_wide <- local_matrices_long %>%
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

global_matrix <- matrix_to_long(
  prop_matrix = fit[["VTM"]],
  votes_matrix = fit[["VTM.votes"]],
  matrix_scope = "global"
)

global_matrix_complete <- matrix_to_long(
  prop_matrix = fit[["VTM.complete"]],
  votes_matrix = fit[["VTM.complete.votes"]],
  matrix_scope = "global_complete"
)

settings <- tibble(
  threshold = threshold,
  n_units = length(ids),
  groups = paste(groups, collapse = ", "),
  n_groups = length(groups),
  iter_max = iter_max,
  tol = tol,
  new_and_exit_voters = "raw",
  solver = "lp_solve",
  selection = "erste 2000 sortierte agg_schluessel"
)

# Gespeichert wird nur der kompakte Endoutput, kein voller nslphom-Fit
# und keine Kopien der Inputdaten.
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
