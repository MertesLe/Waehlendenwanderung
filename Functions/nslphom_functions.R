check_lphom_available <- function() {
  if (!requireNamespace("lphom", quietly = TRUE)) {
    stop(
      "Das Paket 'lphom' ist nicht installiert. ",
      "Installiere es mit install.packages('lphom'), damit lphom::nslphom() laufen kann."
    )
  }

  invisible(TRUE)
}

read_prepared_nslphom_inputs <- function() {
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
    input2021 = readRDS(files[["input2021"]]) %>% dplyr::arrange(.data$agg_schluessel),
    input2025 = readRDS(files[["input2025"]]) %>% dplyr::arrange(.data$agg_schluessel),
    input_long = readRDS(files[["input_long"]]),
    party_thresholds = readRDS(files[["party_thresholds"]]),
    input_checks = readRDS(files[["input_checks"]])
  )
}

validate_prepared_nslphom_inputs <- function(inputs, threshold = 0.12) {
  stopifnot(identical(inputs$input2021$agg_schluessel, inputs$input2025$agg_schluessel))
  stopifnot(identical(names(inputs$input2021), names(inputs$input2025)))

  group_names <- setdiff(names(inputs$input2021), "agg_schluessel")
  stopifnot(!any(c("CDU", "CSU") %in% group_names))
  stopifnot("Union" %in% group_names)
  stopifnot(all(rowSums(inputs$input2021[group_names]) == rowSums(inputs$input2025[group_names])))
  stopifnot(all(abs(inputs$input_checks$differenz_input_zu_referenz) < 1e-8))
  stopifnot(all(abs(inputs$input_checks$differenz_stimmen_zu_waehlenden) < 1e-8))
  stopifnot(all(make_count_matrix(inputs$input2021) >= 0, na.rm = TRUE))
  stopifnot(all(make_count_matrix(inputs$input2025) >= 0, na.rm = TRUE))

  threshold_check <- inputs$party_thresholds %>%
    dplyr::mutate(expected_keep_party_year = .data$national_share_valid >= threshold) %>%
    dplyr::filter(.data$keep_party_year != .data$expected_keep_party_year)

  if (nrow(threshold_check) > 0) {
    stop(
      "Die zentralen nslphom-Inputs passen nicht zur ",
      threshold * 100,
      "%-Schwelle. Fuehre zuerst Scripts/01_prepare_nslphom_input.R neu aus."
    )
  }

  kept_parties <- inputs$party_thresholds %>%
    dplyr::filter(.data$keep_party) %>%
    dplyr::pull(party) %>%
    unique() %>%
    sort()

  expected_kept_parties <- inputs$party_thresholds %>%
    dplyr::group_by(.data$party) %>%
    dplyr::summarise(
      expected_keep_party = any(.data$national_share_valid >= threshold),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$expected_keep_party) %>%
    dplyr::pull(party) %>%
    sort()

  stopifnot(identical(kept_parties, expected_kept_parties))
  stopifnot(setequal(c(kept_parties, "Andere", "Nichtwaehler"), group_names))

  list(
    group_names = group_names,
    kept_parties = kept_parties
  )
}

make_count_matrix <- function(data, id_col = "agg_schluessel") {
  mat <- data %>%
    dplyr::select(-dplyr::all_of(id_col)) %>%
    as.matrix()

  storage.mode(mat) <- "numeric"
  rownames(mat) <- data[[id_col]]

  mat
}

derive_nslphom_block_id <- function(agg_schluessel, prefix_length = 3L) {
  prefix_groups <- lapply(
    agg_schluessel,
    function(id) {
      keys <- split_keys(id)
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

    # Stadtstaaten haben nur sehr wenige Aggregationseinheiten. Deshalb werden
    # Berlin+Brandenburg, Bremen+Niedersachsen und Hamburg+Schleswig-Holstein/
    # Niedersachsen gemeinsam geschaetzt; durch Niedersachsen entsteht ein
    # gemeinsamer Nord-Block fuer Bremen und Hamburg.
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

make_nslphom_block_index <- function(input2021, prefix_length = 3L) {
  tibble::tibble(
    agg_schluessel = input2021$agg_schluessel,
    nslphom_block = derive_nslphom_block_id(input2021$agg_schluessel, prefix_length = prefix_length)
  )
}

fit_nslphom_model <- function(
    origin_counts,
    destination_counts,
    iter_max = getOption("waehlendenwanderung.nslphom_iter_max", 10L),
    tol = getOption("waehlendenwanderung.nslphom_tol", 1e-5),
    verbose = FALSE,
    method = "lphom::nslphom") {
  check_lphom_available()

  fit <- lphom::nslphom(
    votes_election1 = as.data.frame(origin_counts),
    votes_election2 = as.data.frame(destination_counts),
    new_and_exit_voters = "simultaneous",
    apriori = NULL,
    uniform = TRUE,
    iter.max = iter_max,
    min.first = FALSE,
    structural_zeros = NULL,
    integers = FALSE,
    distance.local = "abs",
    verbose = verbose,
    solver = "lp_solve",
    burnin = 0,
    tol = tol
  )

  attr(fit, "method_label") <- method
  fit
}

local_matrices_to_long <- function(fit, ids, method = "lphom::nslphom") {
  prop_units <- fit[["VTM.prop.units"]]
  votes_units <- fit[["VTM.votes.units"]]

  stopifnot(length(dim(prop_units)) == 3)
  stopifnot(identical(dim(prop_units), dim(votes_units)))
  stopifnot(dim(prop_units)[[3]] == length(ids))

  dplyr::bind_rows(lapply(seq_along(ids), function(i) {
    prop_matrix <- prop_units[, , i]
    votes_matrix <- votes_units[, , i]

    origin_count <- rowSums(votes_matrix, na.rm = TRUE)
    destination_count <- colSums(votes_matrix, na.rm = TRUE)
    matrix_cells <- as.data.frame(as.table(prop_matrix), stringsAsFactors = FALSE) %>%
      dplyr::mutate(
        from = as.character(.data$Var1),
        to = as.character(.data$Var2),
        row_index = match(.data$from, rownames(votes_matrix)),
        col_index = match(.data$to, colnames(votes_matrix))
      )

    matrix_cells %>%
      dplyr::transmute(
        agg_schluessel = ids[[i]],
        from = .data$from,
        to = .data$to,
        transition_probability = as.numeric(.data$Freq),
        origin_count = as.numeric(origin_count[.data$from]),
        destination_count = as.numeric(destination_count[.data$to]),
        estimated_transition_count = as.numeric(votes_matrix[cbind(.data$row_index, .data$col_index)]),
        method = method
      )
  }))
}

matrix_to_long <- function(prop_matrix, votes_matrix, matrix_scope, method = "lphom::nslphom") {
  origin_count <- rowSums(votes_matrix, na.rm = TRUE)
  destination_count <- colSums(votes_matrix, na.rm = TRUE)
  matrix_cells <- as.data.frame(as.table(prop_matrix), stringsAsFactors = FALSE) %>%
    dplyr::mutate(
      from = as.character(.data$Var1),
      to = as.character(.data$Var2),
      row_index = match(.data$from, rownames(votes_matrix)),
      col_index = match(.data$to, colnames(votes_matrix))
    )

  matrix_cells %>%
    dplyr::transmute(
      matrix_scope = matrix_scope,
      from = .data$from,
      to = .data$to,
      transition_probability = as.numeric(.data$Freq),
      origin_count = as.numeric(origin_count[.data$from]),
      destination_count = as.numeric(destination_count[.data$to]),
      estimated_transition_count = as.numeric(votes_matrix[cbind(.data$row_index, .data$col_index)]),
      method = method
    )
}

make_nslphom_checks <- function(
    fit,
    transition_long,
    block_id = NA_character_,
    method = "lphom::nslphom",
    threshold = NA_real_,
    blocked = TRUE) {
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
    nslphom_block = block_id,
    n_units = dim(prop_units)[[3]],
    n_origin_groups = dim(prop_units)[[1]],
    n_destination_groups = dim(prop_units)[[2]],
    max_abs_row_sum_error = transition_long %>%
      dplyr::group_by(.data$agg_schluessel, .data$from) %>%
      dplyr::summarise(
        origin_count = max(.data$origin_count, na.rm = TRUE),
        row_sum = sum(.data$transition_probability),
        .groups = "drop"
      ) %>%
      dplyr::filter(.data$origin_count > 0) %>%
      dplyr::summarise(value = max(abs(.data$row_sum - 1), na.rm = TRUE)) %>%
      dplyr::pull(value),
    max_abs_origin_reconstruction_error = max(abs(origin_from_units - origin_used), na.rm = TRUE),
    max_abs_destination_reconstruction_error = max(abs(destination_from_units - destination_used), na.rm = TRUE),
    iter = fit[["iter"]],
    iter_min = fit[["iter.min"]],
    HETe = fit[["HETe"]],
    HETe_init = fit[["solution_init"]][["HETe_init"]],
    method = method,
    package = "lphom",
    package_version = as.character(utils::packageVersion("lphom")),
    new_and_exit_voters = "simultaneous",
    threshold = threshold,
    blocked = blocked
  )
}

fit_nslphom_block <- function(
    block_id,
    block_index,
    input2021,
    input2025,
    iter_max = getOption("waehlendenwanderung.nslphom_iter_max", 10L),
    tol = getOption("waehlendenwanderung.nslphom_tol", 1e-5),
    verbose = FALSE,
    threshold = 0.12) {
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
  fit <- fit_nslphom_model(
    origin_counts,
    destination_counts,
    iter_max = iter_max,
    tol = tol,
    verbose = verbose,
    method = "lphom::nslphom"
  )

  transition_long <- local_matrices_to_long(fit, block_ids, method = "lphom::nslphom") %>%
    dplyr::mutate(nslphom_block = block_id, .after = agg_schluessel)

  list(
    block_id = block_id,
    ids = block_ids,
    fit = fit,
    transition_long = transition_long,
    checks = make_nslphom_checks(
      fit,
      transition_long,
      block_id = block_id,
      method = "lphom::nslphom",
      threshold = threshold,
      blocked = TRUE
    )
  )
}

fit_nslphom_blocks <- function(
    block_index,
    input2021,
    input2025,
    iter_max = getOption("waehlendenwanderung.nslphom_iter_max", 10L),
    tol = getOption("waehlendenwanderung.nslphom_tol", 1e-5),
    verbose = FALSE,
    threshold = 0.12) {
  block_results <- lapply(
    sort(unique(block_index$nslphom_block)),
    fit_nslphom_block,
    block_index = block_index,
    input2021 = input2021,
    input2025 = input2025,
    iter_max = iter_max,
    tol = tol,
    verbose = verbose,
    threshold = threshold
  )

  names(block_results) <- vapply(block_results, `[[`, character(1), "block_id")
  block_results
}

make_transition_wide <- function(transition_long) {
  transition_long %>%
    dplyr::mutate(transition = paste0("p_", .data$from, "_to_", .data$to)) %>%
    dplyr::select(agg_schluessel, transition, transition_probability) %>%
    tidyr::pivot_wider(
      names_from = transition,
      values_from = transition_probability
    ) %>%
    dplyr::arrange(.data$agg_schluessel)
}

make_nslphom_fit_bundle <- function(
    block_results,
    block_index,
    prefix_length = 3L,
    iter_max = getOption("waehlendenwanderung.nslphom_iter_max", 10L),
    tol = getOption("waehlendenwanderung.nslphom_tol", 1e-5)) {
  list(
    fits = lapply(block_results, `[[`, "fit"),
    block_index = block_index,
    checks = dplyr::bind_rows(lapply(block_results, `[[`, "checks")),
    settings = list(
      block_prefix_length = prefix_length,
      city_state_neighbor_blocks = TRUE,
      iter_max = iter_max,
      tol = tol,
      new_and_exit_voters = "simultaneous",
      solver = "lp_solve"
    ),
    package = "lphom",
    package_version = as.character(utils::packageVersion("lphom"))
  )
}

write_blocked_nslphom_outputs <- function(
    nslphom_fit,
    transition_long,
    transition_wide,
    checks,
    write_csv = TRUE) {
  saveRDS(nslphom_fit, file.path(data_dir_model_nslphom, "vorlaeufig_nslphom_fit.rds"))
  saveRDS(transition_long, file.path(data_dir_model_nslphom, "vorlaeufig_transition_matrices_long.rds"))
  saveRDS(transition_wide, file.path(data_dir_model_nslphom, "vorlaeufig_transition_matrices_wide.rds"))
  saveRDS(checks, file.path(data_dir_model_nslphom, "vorlaeufig_transition_checks.rds"))

  if (isTRUE(write_csv)) {
    utils::write.csv(transition_long, file.path(data_dir_model_nslphom, "vorlaeufig_transition_matrices_long.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(transition_wide, file.path(data_dir_model_nslphom, "vorlaeufig_transition_matrices_wide.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(checks, file.path(data_dir_model_nslphom, "vorlaeufig_transition_checks.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }

  invisible(nslphom_fit)
}

make_unblocked_settings <- function(
    inputs,
    validation,
    threshold = 0.12,
    iter_max = 10L,
    tol = 1e-5,
    blocked = FALSE) {
  tibble::tibble(
    threshold = threshold,
    keep_parties = paste(validation$kept_parties, collapse = ", "),
    groups = paste(validation$group_names, collapse = ", "),
    blocked = blocked,
    n_units = nrow(inputs$input2021),
    n_groups = length(validation$group_names),
    iter_max = iter_max,
    tol = tol,
    new_and_exit_voters = "simultaneous",
    solver = "lp_solve"
  )
}

write_unblocked_input_copies <- function(inputs, settings, output_dir, write_csv = TRUE) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  saveRDS(inputs$input2021, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_2021.rds"))
  saveRDS(inputs$input2025, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_2025.rds"))
  saveRDS(inputs$input_long, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_long.rds"))
  saveRDS(inputs$party_thresholds, file.path(output_dir, "vorlaeufig_nslphom_unblocked_party_thresholds.rds"))
  saveRDS(inputs$input_checks, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_checks.rds"))
  saveRDS(settings, file.path(output_dir, "vorlaeufig_nslphom_unblocked_settings.rds"))

  if (isTRUE(write_csv)) {
    utils::write.csv(inputs$input2021, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_2021.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(inputs$input2025, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_2025.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(inputs$input_long, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_long.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(inputs$party_thresholds, file.path(output_dir, "vorlaeufig_nslphom_unblocked_party_thresholds.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(inputs$input_checks, file.path(output_dir, "vorlaeufig_nslphom_unblocked_input_checks.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(settings, file.path(output_dir, "vorlaeufig_nslphom_unblocked_settings.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }

  invisible(settings)
}

write_unblocked_nslphom_outputs <- function(
    fit,
    ids,
    output_dir,
    settings,
    method = "lphom::nslphom_unblocked",
    threshold = 0.12,
    write_csv = TRUE) {
  transition_long <- local_matrices_to_long(fit, ids, method = method) %>%
    dplyr::arrange(.data$agg_schluessel, .data$from, .data$to)

  transition_wide <- make_transition_wide(transition_long)

  global_transition <- matrix_to_long(
    prop_matrix = fit[["VTM"]],
    votes_matrix = fit[["VTM.votes"]],
    matrix_scope = "global",
    method = method
  )

  global_transition_complete <- matrix_to_long(
    prop_matrix = fit[["VTM.complete"]],
    votes_matrix = fit[["VTM.complete.votes"]],
    matrix_scope = "global_complete",
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

  if (isTRUE(write_csv)) {
    utils::write.csv(transition_long, file.path(output_dir, "vorlaeufig_nslphom_unblocked_local_matrices_long.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(transition_wide, file.path(output_dir, "vorlaeufig_nslphom_unblocked_local_matrices_wide.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(global_transition, file.path(output_dir, "vorlaeufig_nslphom_unblocked_global_matrix.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(global_transition_complete, file.path(output_dir, "vorlaeufig_nslphom_unblocked_global_matrix_complete.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(checks, file.path(output_dir, "vorlaeufig_nslphom_unblocked_checks.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }

  invisible(fit_bundle)
}
