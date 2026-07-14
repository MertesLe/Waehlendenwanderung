library(dplyr)
library(tidyr)
library(stringr)

source("paths.R", encoding = "UTF-8")

ensure_data_dirs()

output_dir <- file.path("Data", "modeloutput", "nslphom_unblocked_10pct")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
run_fit <- isTRUE(getOption("waehlendenwanderung.unblocked_run_fit", TRUE))

if (!requireNamespace("lphom", quietly = TRUE)) {
  stop(
    "Das Paket 'lphom' ist nicht installiert. ",
    "Installiere es mit install.packages('lphom'), damit lphom::nslphom() laufen kann."
  )
}

get_agg_col <- function(data) {
  grep("^agg\\.", names(data), value = TRUE)[1]
}

first_existing <- function(data, patterns) {
  for (pattern in patterns) {
    hit <- grep(pattern, names(data), value = TRUE)

    if (length(hit) > 0) {
      return(hit[[1]])
    }
  }

  NA_character_
}

standardize_party <- function(x) {
  x <- x %>%
    str_replace("^E_", "") %>%
    str_replace("\\.\\.\\.Erststimmen$", "") %>%
    str_replace_all("\\.", "_")

  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""

  party <- x %>%
    str_replace_all("[^A-Za-z0-9_]", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "")

  recode(
    party,
    AFD = "AfD",
    DIE_LINKE = "Die_Linke",
    GRUENE = "GRUNE",
    .default = party
  )
}

get_first_vote_columns <- function(data, jahr, invalid_col, valid_col) {
  if (jahr == 2021) {
    cols <- grep("^E_", names(data), value = TRUE)
  } else {
    cols <- grep("\\.\\.\\.Erststimmen$", names(data), value = TRUE)
  }

  setdiff(cols, c(invalid_col, valid_col))
}

prepare_vote_year <- function(data, jahr, threshold = 0.10, keep_parties = NULL) {
  agg_col <- get_agg_col(data)
  a_col <- first_existing(data, c("^Wahlberechtigte"))
  b_col <- first_existing(data, c("^W.hlende"))
  invalid_col <- if (jahr == 2021) {
    first_existing(data, c("^E_Ung.ltige$"))
  } else {
    first_existing(data, c("^Ung.ltige\\.\\.\\.Erststimmen$"))
  }
  valid_col <- if (jahr == 2021) {
    first_existing(data, c("^E_G.ltige$"))
  } else {
    first_existing(data, c("^G.ltige\\.\\.\\.Erststimmen$"))
  }

  stopifnot(!is.na(agg_col), !is.na(a_col), !is.na(b_col))
  stopifnot(!is.na(invalid_col), !is.na(valid_col))

  party_cols <- get_first_vote_columns(data, jahr, invalid_col, valid_col)
  stopifnot(length(party_cols) > 0)

  party_lookup <- tibble(
    source_col = party_cols,
    party = standardize_party(party_cols)
  )

  national <- data %>%
    summarise(
      across(all_of(party_cols), ~ sum(.x, na.rm = TRUE)),
      valid_total = sum(.data[[valid_col]], na.rm = TRUE)
    ) %>%
    pivot_longer(
      cols = all_of(party_cols),
      names_to = "source_col",
      values_to = "votes"
    ) %>%
    left_join(party_lookup, by = "source_col") %>%
    mutate(
      national_share_valid = votes / valid_total,
      keep_party_year = national_share_valid >= threshold
    )

  if (is.null(keep_parties)) {
    keep_parties <- national %>%
      filter(keep_party_year) %>%
      pull(party)
  }

  keep_parties <- sort(unique(keep_parties))
  output_groups <- c(keep_parties, "Andere", "Nichtwaehler")
  national <- national %>%
    mutate(
      keep_party = party %in% keep_parties
    )

  long <- data %>%
    transmute(
      agg_schluessel = .data[[agg_col]],
      wahlberechtigte = .data[[a_col]],
      waehlende = .data[[b_col]],
      ungueltig_erst = .data[[invalid_col]],
      gueltig_erst = .data[[valid_col]],
      across(all_of(party_cols))
    ) %>%
    pivot_longer(
      cols = all_of(party_cols),
      names_to = "source_col",
      values_to = "votes"
    ) %>%
    left_join(party_lookup, by = "source_col") %>%
    mutate(
      gruppe = if_else(party %in% keep_parties, party, "Andere")
    ) %>%
    group_by(
      agg_schluessel,
      gruppe
    ) %>%
    summarise(
      votes = sum(votes, na.rm = TRUE),
      .groups = "drop"
    )

  residual <- data %>%
    transmute(
      agg_schluessel = .data[[agg_col]],
      Andere = .data[[invalid_col]],
      Nichtwaehler = pmax(.data[[a_col]] - .data[[b_col]], 0)
    ) %>%
    pivot_longer(
      cols = c("Andere", "Nichtwaehler"),
      names_to = "gruppe",
      values_to = "votes"
    ) %>%
    group_by(
      agg_schluessel,
      gruppe
    ) %>%
    summarise(
      votes = sum(votes, na.rm = TRUE),
      .groups = "drop"
    )

  counts <- bind_rows(long, residual) %>%
    group_by(
      agg_schluessel,
      gruppe
    ) %>%
    summarise(
      votes = sum(votes, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    complete(
      agg_schluessel,
      gruppe = output_groups,
      fill = list(votes = 0)
    )

  wide <- counts %>%
    mutate(
      gruppe = factor(gruppe, levels = output_groups)
    ) %>%
    pivot_wider(
      names_from = gruppe,
      values_from = votes,
      values_fill = 0
    ) %>%
    arrange(agg_schluessel) %>%
    select(agg_schluessel, all_of(output_groups))

  check <- data %>%
    transmute(
      agg_schluessel = .data[[agg_col]],
      wahlberechtigte = .data[[a_col]],
      waehlende = .data[[b_col]],
      gueltig_erst = .data[[valid_col]],
      ungueltig_erst = .data[[invalid_col]]
    ) %>%
    group_by(agg_schluessel) %>%
    summarise(
      wahlberechtigte = sum(wahlberechtigte, na.rm = TRUE),
      waehlende = sum(waehlende, na.rm = TRUE),
      gueltig_erst = sum(gueltig_erst, na.rm = TRUE),
      ungueltig_erst = sum(ungueltig_erst, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      counts %>%
        group_by(agg_schluessel) %>%
        summarise(input_sum = sum(votes, na.rm = TRUE), .groups = "drop"),
      by = "agg_schluessel"
    ) %>%
    mutate(
      input_reference = pmax(wahlberechtigte, waehlende),
      differenz_input_zu_wahlberechtigten = input_sum - wahlberechtigte,
      differenz_input_zu_referenz = input_sum - input_reference,
      differenz_stimmen_zu_waehlenden = gueltig_erst + ungueltig_erst - waehlende,
      flag_waehlende_groesser_wahlberechtigte = waehlende > wahlberechtigte
    )

  list(
    wide = wide,
    long = counts %>% mutate(Jahr = jahr, .before = 1),
    national = national %>% mutate(Jahr = jahr, .before = 1),
    check = check %>% mutate(Jahr = jahr, .before = 1)
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
  lphom::nslphom(
    votes_election1 = as.data.frame(origin_counts),
    votes_election2 = as.data.frame(destination_counts),
    new_and_exit_voters = "raw",
    apriori = NULL,
    uniform = TRUE,
    iter.max = getOption("waehlendenwanderung.unblocked_nslphom_iter_max", 10L),
    min.first = FALSE,
    structural_zeros = NULL,
    integers = FALSE,
    distance.local = "abs",
    verbose = TRUE,
    solver = "lp_solve",
    burnin = 0,
    tol = getOption("waehlendenwanderung.unblocked_nslphom_tol", 1e-5)
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
        method = "lphom::nslphom_unblocked_10pct"
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
      method = "lphom::nslphom_unblocked_10pct"
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

  tibble(
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
    method = "lphom::nslphom_unblocked_10pct",
    package = "lphom",
    package_version = as.character(utils::packageVersion("lphom")),
    new_and_exit_voters = "raw",
    threshold = 0.10,
    blocked = FALSE
  )
}

message("Lese gemappte Wahldaten.")
wahldaten2021 <- readRDS(file.path(data_dir_cleaned, "wahldaten2021_gemappt.rds"))
wahldaten2025 <- readRDS(file.path(data_dir_cleaned, "wahldaten2025_gemappt.rds"))

# Die 10%-Schwelle wird wie im bisherigen Workflow bundesweit auf den Anteil an
# gueltigen Erststimmen angewendet. Eine Partei bleibt erhalten, wenn sie 2021
# oder 2025 mindestens 10 Prozent erreicht; alle anderen Parteien werden zu
# "Andere" zusammengefasst.
message("Bereite nslphom-Input mit 10%-Parteischwelle vor.")
initial2021 <- prepare_vote_year(wahldaten2021, 2021, threshold = 0.10)
initial2025 <- prepare_vote_year(wahldaten2025, 2025, threshold = 0.10)

keep_parties <- bind_rows(initial2021$national, initial2025$national) %>%
  filter(keep_party_year) %>%
  pull(party) %>%
  unique() %>%
  sort()

prepared2021 <- prepare_vote_year(
  wahldaten2021,
  2021,
  threshold = 0.10,
  keep_parties = keep_parties
)
prepared2025 <- prepare_vote_year(
  wahldaten2025,
  2025,
  threshold = 0.10,
  keep_parties = keep_parties
)

common_ids <- intersect(prepared2021$wide$agg_schluessel, prepared2025$wide$agg_schluessel)
stopifnot(length(common_ids) == nrow(prepared2021$wide))
stopifnot(length(common_ids) == nrow(prepared2025$wide))

prepared2021$wide <- prepared2021$wide %>%
  filter(agg_schluessel %in% common_ids) %>%
  arrange(agg_schluessel)
prepared2025$wide <- prepared2025$wide %>%
  filter(agg_schluessel %in% common_ids) %>%
  arrange(agg_schluessel)

stopifnot(identical(prepared2021$wide$agg_schluessel, prepared2025$wide$agg_schluessel))
stopifnot(identical(names(prepared2021$wide), names(prepared2025$wide)))
stopifnot(all(abs(prepared2021$check$differenz_input_zu_referenz) < 1e-8))
stopifnot(all(abs(prepared2025$check$differenz_input_zu_referenz) < 1e-8))
stopifnot(all(abs(prepared2021$check$differenz_stimmen_zu_waehlenden) < 1e-8))
stopifnot(all(abs(prepared2025$check$differenz_stimmen_zu_waehlenden) < 1e-8))

input_checks <- bind_rows(prepared2021$check, prepared2025$check)
party_thresholds <- bind_rows(prepared2021$national, prepared2025$national)
input_long <- bind_rows(prepared2021$long, prepared2025$long)

saveRDS(prepared2021$wide, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_input_2021.rds"))
saveRDS(prepared2025$wide, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_input_2025.rds"))
saveRDS(input_long, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_input_long.rds"))
saveRDS(party_thresholds, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_party_thresholds.rds"))
saveRDS(input_checks, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_input_checks.rds"))

write.csv(prepared2021$wide, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_input_2021.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(prepared2025$wide, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_input_2025.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(input_long, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_input_long.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(party_thresholds, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_party_thresholds.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(input_checks, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_input_checks.csv"), row.names = FALSE, fileEncoding = "UTF-8")

if (!run_fit) {
  message(
    "Input wurde erzeugt und gespeichert. ",
    "Der nslphom-Fit wurde wegen option waehlendenwanderung.unblocked_run_fit = FALSE uebersprungen."
  )
  quit(save = "no", status = 0)
}

ids <- prepared2021$wide$agg_schluessel
origin_counts <- make_count_matrix(prepared2021$wide)
destination_counts <- make_count_matrix(prepared2025$wide)

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
  settings = list(
    threshold = 0.10,
    keep_parties = keep_parties,
    blocked = FALSE,
    n_units = nrow(origin_counts),
    n_groups = ncol(origin_counts),
    iter_max = getOption("waehlendenwanderung.unblocked_nslphom_iter_max", 10L),
    tol = getOption("waehlendenwanderung.unblocked_nslphom_tol", 1e-5),
    new_and_exit_voters = "raw",
    solver = "lp_solve"
  ),
  checks = checks,
  package = "lphom",
  package_version = as.character(utils::packageVersion("lphom"))
)

saveRDS(fit_bundle, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_fit.rds"))
saveRDS(transition_long, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_local_matrices_long.rds"))
saveRDS(transition_wide, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_local_matrices_wide.rds"))
saveRDS(global_transition, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_global_matrix.rds"))
saveRDS(global_transition_complete, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_global_matrix_complete.rds"))
saveRDS(checks, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_checks.rds"))

write.csv(transition_long, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_local_matrices_long.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(transition_wide, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_local_matrices_wide.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(global_transition, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_global_matrix.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(global_transition_complete, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_global_matrix_complete.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(checks, file.path(output_dir, "vorlaeufig_nslphom_unblocked_10pct_checks.csv"), row.names = FALSE, fileEncoding = "UTF-8")

message("Fertig. Ergebnisse gespeichert unter: ", output_dir)
