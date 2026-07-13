library(dplyr)
library(tidyr)

dir.create("Data/cleaned", recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("lphom", quietly = TRUE)) {
  stop(
    "Das Paket 'lphom' ist nicht installiert. ",
    "Installiere es mit install.packages('lphom'), damit lphom::nslphom() laufen kann."
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

split_agg_keys <- function(agg_schluessel) {
  keys <- unlist(strsplit(agg_schluessel, ",\\s*"), use.names = FALSE)
  keys[keys != ""]
}

derive_block_id <- function(agg_schluessel, prefix_length = 3L) {
  prefix_groups <- lapply(
    agg_schluessel,
    function(id) {
      keys <- split_agg_keys(id)
      sort(unique(substr(keys, 1, prefix_length)))
    }
  )

  all_prefixes <- sort(unique(unlist(prefix_groups, use.names = FALSE)))
  parent <- stats::setNames(all_prefixes, all_prefixes)

  find_root <- function(x) {
    root <- x

    while (!identical(parent[[root]], root)) {
      root <- parent[[root]]
    }

    while (!identical(parent[[x]], x)) {
      next_x <- parent[[x]]
      parent[[x]] <<- root
      x <- next_x
    }

    root
  }

  union_prefixes <- function(a, b) {
    root_a <- find_root(a)
    root_b <- find_root(b)

    if (!identical(root_a, root_b)) {
      parent[[root_b]] <<- root_a
    }
  }

  for (group in prefix_groups) {
    if (length(group) > 1) {
      for (prefix in group[-1]) {
        union_prefixes(group[[1]], prefix)
      }
    }
  }

  if (identical(as.integer(prefix_length), 3L)) {
    city_state_neighbor_groups <- list(
      c("111", "112", "120"),
      c("040", "031", "032", "033", "034"),
      c("020", "010", "031", "032", "033", "034")
    )

    # Stadtstaaten haben nur sehr wenige Aggregationseinheiten. Damit ihre
    # lokale nslphom-Schaetzung nicht auf einer extrem kleinen eigenen globalen
    # Matrix basiert, werden sie mit naheliegenden Nachbarlaendern geschaetzt:
    # Berlin mit Brandenburg, Bremen mit Niedersachsen, Hamburg mit
    # Schleswig-Holstein und Niedersachsen. Weil Bremen und Hamburg beide mit
    # Niedersachsen verbunden sind, entsteht daraus ein gemeinsamer Nord-Block.
    for (group in city_state_neighbor_groups) {
      existing_group <- intersect(group, all_prefixes)

      if (length(existing_group) > 1) {
        for (prefix in existing_group[-1]) {
          union_prefixes(existing_group[[1]], prefix)
        }
      }
    }
  }

  roots <- vapply(all_prefixes, find_root, character(1))
  components <- split(all_prefixes, roots)
  prefix_lookup <- stats::setNames(character(length(all_prefixes)), all_prefixes)

  for (component in components) {
    prefix_lookup[component] <- paste(sort(component), collapse = "+")
  }

  vapply(
    prefix_groups,
    function(group) prefix_lookup[[group[[1]]]],
    character(1)
  )
}

fit_nslphom <- function(origin_counts, destination_counts) {
  lphom::nslphom(
    votes_election1 = as.data.frame(origin_counts),
    votes_election2 = as.data.frame(destination_counts),
    new_and_exit_voters = "raw",
    apriori = NULL,
    uniform = TRUE,
    iter.max = getOption("waehlendenwanderung.nslphom_iter_max", 10L),
    min.first = FALSE,
    structural_zeros = NULL,
    integers = FALSE,
    distance.local = "abs",
    verbose = FALSE,
    solver = "lp_solve",
    burnin = 0,
    tol = getOption("waehlendenwanderung.nslphom_tol", 1e-5)
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
        method = "lphom::nslphom"
      )
  }))
}

make_nslphom_checks <- function(fit, transition_long, block_id) {
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
    nslphom_block = block_id,
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
    n_units = dim(prop_units)[[3]],
    n_origin_groups = dim(prop_units)[[1]],
    n_destination_groups = dim(prop_units)[[2]],
    iter = fit[["iter"]],
    iter_min = fit[["iter.min"]],
    HETe = fit[["HETe"]],
    HETe_init = fit[["solution_init"]][["HETe_init"]],
    method = "lphom::nslphom",
    package = "lphom",
    package_version = as.character(utils::packageVersion("lphom")),
    new_and_exit_voters = "raw"
  )
}

fit_block <- function(block_id, block_index, input2021, input2025) {
  block_rows <- which(block_index$nslphom_block == block_id)
  block_ids <- block_index$agg_schluessel[block_rows]

  message(
    "Schaetze nslphom-Block ",
    block_id,
    " mit ",
    length(block_rows),
    " Aggregationseinheiten."
  )

  origin_counts <- make_count_matrix(input2021[block_rows, , drop = FALSE])
  destination_counts <- make_count_matrix(input2025[block_rows, , drop = FALSE])
  fit <- fit_nslphom(origin_counts, destination_counts)

  transition_long <- local_matrices_to_long(fit, block_ids) %>%
    mutate(
      nslphom_block = block_id,
      .after = agg_schluessel
    )

  list(
    block_id = block_id,
    ids = block_ids,
    fit = fit,
    transition_long = transition_long,
    checks = make_nslphom_checks(fit, transition_long, block_id)
  )
}

input2021 <- readRDS("Data/cleaned/vorlaeufig_nslphom_input_2021.rds") %>%
  arrange(agg_schluessel)
input2025 <- readRDS("Data/cleaned/vorlaeufig_nslphom_input_2025.rds") %>%
  arrange(agg_schluessel)

stopifnot(identical(input2021$agg_schluessel, input2025$agg_schluessel))
stopifnot(identical(names(input2021), names(input2025)))

ids <- input2021$agg_schluessel
block_index <- tibble(
  agg_schluessel = ids,
  nslphom_block = derive_block_id(
    ids,
    prefix_length = getOption("waehlendenwanderung.nslphom_block_prefix_length", 3L)
  )
)

block_results <- lapply(
  sort(unique(block_index$nslphom_block)),
  fit_block,
  block_index = block_index,
  input2021 = input2021,
  input2025 = input2025
)

names(block_results) <- vapply(block_results, `[[`, character(1), "block_id")

nslphom_fit <- list(
  fits = lapply(block_results, `[[`, "fit"),
  block_index = block_index,
  checks = bind_rows(lapply(block_results, `[[`, "checks")),
  settings = list(
    block_prefix_length = getOption("waehlendenwanderung.nslphom_block_prefix_length", 3L),
    city_state_neighbor_blocks = TRUE,
    iter_max = getOption("waehlendenwanderung.nslphom_iter_max", 10L),
    tol = getOption("waehlendenwanderung.nslphom_tol", 1e-5),
    new_and_exit_voters = "raw",
    solver = "lp_solve"
  ),
  package = "lphom",
  package_version = as.character(utils::packageVersion("lphom"))
)

transition_long <- bind_rows(lapply(block_results, `[[`, "transition_long")) %>%
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

checks <- nslphom_fit$checks %>%
  arrange(nslphom_block)

saveRDS(nslphom_fit, "Data/cleaned/vorlaeufig_nslphom_fit.rds")
saveRDS(transition_long, "Data/cleaned/vorlaeufig_transition_matrices_long.rds")
saveRDS(transition_wide, "Data/cleaned/vorlaeufig_transition_matrices_wide.rds")
saveRDS(checks, "Data/cleaned/vorlaeufig_transition_checks.rds")

write.csv(transition_long, "Data/cleaned/vorlaeufig_transition_matrices_long.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(transition_wide, "Data/cleaned/vorlaeufig_transition_matrices_wide.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(checks, "Data/cleaned/vorlaeufig_transition_checks.csv", row.names = FALSE, fileEncoding = "UTF-8")
