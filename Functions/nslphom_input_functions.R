standardize_party <- function(x) {
  x <- x %>%
    stringr::str_replace("^Z_", "") %>%
    stringr::str_replace("\\.\\.\\.Zweitstimmen$", "") %>%
    stringr::str_replace_all("\\.", "_")

  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""

  party <- x %>%
    stringr::str_replace_all("[^A-Za-z0-9_]", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace_all("^_|_$", "")

  dplyr::recode(
    party,
    CDU = "Union",
    CSU = "Union",
    AFD = "AfD",
    DIE_LINKE = "Die_Linke",
    GRUENE = "GRUNE",
    .default = party
  )
}

get_second_vote_columns <- function(data, jahr, invalid_col, valid_col) {
  if (jahr == 2021) {
    cols <- grep("^Z_", names(data), value = TRUE)
  } else {
    cols <- grep("\\.\\.\\.Zweitstimmen$", names(data), value = TRUE)
  }

  setdiff(cols, c(invalid_col, valid_col))
}

prepare_vote_year <- function(data, jahr, threshold = 0.12, keep_parties = NULL) {
  agg_col <- get_agg_col(data)
  a_col <- first_existing(data, c("^Wahlberechtigte"))
  b_col <- first_existing(data, c("^W.hlende"))
  invalid_col <- if (jahr == 2021) {
    first_existing(data, c("^Z_Ung.ltige$"))
  } else {
    first_existing(data, c("^Ung.ltige\\.\\.\\.Zweitstimmen$"))
  }
  valid_col <- if (jahr == 2021) {
    first_existing(data, c("^Z_G.ltige$"))
  } else {
    first_existing(data, c("^G.ltige\\.\\.\\.Zweitstimmen$"))
  }

  stopifnot(!is.na(agg_col), !is.na(a_col), !is.na(b_col))
  stopifnot(!is.na(invalid_col), !is.na(valid_col))

  party_cols <- get_second_vote_columns(data, jahr, invalid_col, valid_col)
  stopifnot(length(party_cols) > 0)

  party_lookup <- tibble::tibble(
    source_col = party_cols,
    party = standardize_party(party_cols)
  )

  national <- data %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(party_cols), ~ sum(.x, na.rm = TRUE)),
      valid_total = sum(.data[[valid_col]], na.rm = TRUE)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(party_cols),
      names_to = "source_col",
      values_to = "votes"
    ) %>%
    dplyr::left_join(party_lookup, by = "source_col") %>%
    dplyr::group_by(.data$party) %>%
    dplyr::summarise(
      source_col = paste(.data$source_col, collapse = ", "),
      votes = sum(.data$votes, na.rm = TRUE),
      valid_total = max(.data$valid_total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      threshold = threshold,
      national_share_valid = .data$votes / .data$valid_total,
      keep_party_year = .data$national_share_valid >= .data$threshold
    )

  if (is.null(keep_parties)) {
    keep_parties <- national %>%
      dplyr::filter(.data$keep_party_year) %>%
      dplyr::pull(party)
  }

  keep_parties <- sort(unique(keep_parties))
  output_groups <- c(keep_parties, "Andere", "Nichtwaehler")
  national <- national %>%
    dplyr::mutate(
      keep_party = .data$party %in% keep_parties
    )

  long <- data %>%
    dplyr::transmute(
      agg_schluessel = .data[[agg_col]],
      wahlberechtigte = .data[[a_col]],
      waehlende = .data[[b_col]],
      ungueltig_stimmen = .data[[invalid_col]],
      gueltig_stimmen = .data[[valid_col]],
      dplyr::across(dplyr::all_of(party_cols))
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(party_cols),
      names_to = "source_col",
      values_to = "votes"
    ) %>%
    dplyr::left_join(party_lookup, by = "source_col") %>%
    dplyr::mutate(
      gruppe = dplyr::if_else(.data$party %in% keep_parties, .data$party, "Andere")
    ) %>%
    dplyr::group_by(.data$agg_schluessel, .data$gruppe) %>%
    dplyr::summarise(
      votes = sum(.data$votes, na.rm = TRUE),
      .groups = "drop"
    )

  residual <- data %>%
    dplyr::transmute(
      agg_schluessel = .data[[agg_col]],
      Andere = .data[[invalid_col]],
      Nichtwaehler = pmax(.data[[a_col]] - .data[[b_col]], 0)
    ) %>%
    tidyr::pivot_longer(
      cols = c("Andere", "Nichtwaehler"),
      names_to = "gruppe",
      values_to = "votes"
    ) %>%
    dplyr::group_by(.data$agg_schluessel, .data$gruppe) %>%
    dplyr::summarise(
      votes = sum(.data$votes, na.rm = TRUE),
      .groups = "drop"
    )

  counts <- dplyr::bind_rows(long, residual) %>%
    dplyr::group_by(.data$agg_schluessel, .data$gruppe) %>%
    dplyr::summarise(
      votes = sum(.data$votes, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::complete(
      agg_schluessel,
      gruppe = output_groups,
      fill = list(votes = 0)
    )

  wide <- counts %>%
    dplyr::mutate(
      gruppe = factor(.data$gruppe, levels = output_groups)
    ) %>%
    tidyr::pivot_wider(
      names_from = gruppe,
      values_from = votes,
      values_fill = 0
    ) %>%
    dplyr::arrange(.data$agg_schluessel) %>%
    dplyr::select(agg_schluessel, dplyr::all_of(output_groups))

  check <- data %>%
    dplyr::transmute(
      agg_schluessel = .data[[agg_col]],
      wahlberechtigte = .data[[a_col]],
      waehlende = .data[[b_col]],
      gueltig_stimmen = .data[[valid_col]],
      ungueltig_stimmen = .data[[invalid_col]]
    ) %>%
    dplyr::group_by(.data$agg_schluessel) %>%
    dplyr::summarise(
      wahlberechtigte = sum(.data$wahlberechtigte, na.rm = TRUE),
      waehlende = sum(.data$waehlende, na.rm = TRUE),
      gueltig_stimmen = sum(.data$gueltig_stimmen, na.rm = TRUE),
      ungueltig_stimmen = sum(.data$ungueltig_stimmen, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      counts %>%
        dplyr::group_by(.data$agg_schluessel) %>%
        dplyr::summarise(input_sum = sum(.data$votes, na.rm = TRUE), .groups = "drop"),
      by = "agg_schluessel"
    ) %>%
    dplyr::mutate(
      input_reference = pmax(.data$wahlberechtigte, .data$waehlende),
      differenz_input_zu_wahlberechtigten = .data$input_sum - .data$wahlberechtigte,
      differenz_input_zu_referenz = .data$input_sum - .data$input_reference,
      differenz_stimmen_zu_waehlenden = .data$gueltig_stimmen + .data$ungueltig_stimmen - .data$waehlende,
      flag_waehlende_groesser_wahlberechtigte = .data$waehlende > .data$wahlberechtigte
    )

  list(
    wide = wide,
    long = counts %>% dplyr::mutate(Jahr = jahr, .before = 1),
    national = national %>% dplyr::mutate(Jahr = jahr, .before = 1),
    check = check %>% dplyr::mutate(Jahr = jahr, .before = 1)
  )
}

scale_2025_to_2021 <- function(prepared2021, prepared2025) {
  groups <- setdiff(names(prepared2021$wide), "agg_schluessel")
  stopifnot(identical(groups, setdiff(names(prepared2025$wide), "agg_schluessel")))

  scale_factors <- prepared2021$wide %>%
    dplyr::transmute(
      agg_schluessel = .data$agg_schluessel,
      input_sum_2021 = rowSums(dplyr::across(dplyr::all_of(groups)))
    ) %>%
    dplyr::left_join(
      prepared2025$wide %>%
        dplyr::transmute(
          agg_schluessel = .data$agg_schluessel,
          input_sum_2025_original = rowSums(dplyr::across(dplyr::all_of(groups)))
        ),
      by = "agg_schluessel"
    ) %>%
    dplyr::mutate(
      skalierungsfaktor_2025 = dplyr::case_when(
        .data$input_sum_2025_original == 0 & .data$input_sum_2021 == 0 ~ 1,
        .data$input_sum_2025_original > 0 ~ .data$input_sum_2021 / .data$input_sum_2025_original,
        TRUE ~ NA_real_
      )
    )

  if (any(is.na(scale_factors$skalierungsfaktor_2025))) {
    stop("Mindestens eine Einheit hat 2021 Masse, aber 2025 keine skalierbare Masse.")
  }

  prepared2021$check <- prepared2021$check %>%
    dplyr::mutate(
      input_sum_original = .data$input_sum,
      input_reference_original = .data$input_reference,
      input_sum_2021 = .data$input_sum,
      input_sum_2025_original = NA_real_,
      skalierungsfaktor_2025 = 1,
      input_skaliert_auf_2021 = FALSE
    )

  prepared2025$wide <- prepared2025$wide %>%
    dplyr::left_join(
      scale_factors %>% dplyr::select(agg_schluessel, skalierungsfaktor_2025),
      by = "agg_schluessel"
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(groups), ~ .x * .data$skalierungsfaktor_2025)
    ) %>%
    dplyr::select(agg_schluessel, dplyr::all_of(groups))

  adjustment_col <- if ("Nichtwaehler" %in% groups) "Nichtwaehler" else groups[[length(groups)]]
  other_groups <- setdiff(groups, adjustment_col)
  prepared2025$wide[[adjustment_col]] <- scale_factors$input_sum_2021 -
    rowSums(prepared2025$wide[other_groups])

  if (any(prepared2025$wide[[adjustment_col]] < -1e-8)) {
    stop("Die exakte 2025-Skalierung erzeugt negative Werte in ", adjustment_col, ".")
  }

  scaled_sums <- prepared2025$wide %>%
    dplyr::transmute(
      agg_schluessel = .data$agg_schluessel,
      input_sum_scaled = rowSums(dplyr::across(dplyr::all_of(groups)))
    )

  prepared2025$long <- prepared2025$wide %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(groups),
      names_to = "gruppe",
      values_to = "votes"
    ) %>%
    dplyr::mutate(Jahr = 2025, .before = 1)

  prepared2025$check <- prepared2025$check %>%
    dplyr::left_join(scale_factors, by = "agg_schluessel") %>%
    dplyr::left_join(scaled_sums, by = "agg_schluessel") %>%
    dplyr::mutate(
      input_sum_original = .data$input_sum,
      input_reference_original = .data$input_reference,
      input_sum = .data$input_sum_scaled,
      input_reference = .data$input_sum_2021,
      differenz_input_zu_wahlberechtigten = .data$input_sum - .data$wahlberechtigte,
      differenz_input_zu_referenz = .data$input_sum - .data$input_reference,
      input_skaliert_auf_2021 = TRUE
    ) %>%
    dplyr::select(-input_sum_scaled)

  list(
    prepared2021 = prepared2021,
    prepared2025 = prepared2025,
    scale_factors = scale_factors
  )
}

prepare_nslphom_inputs <- function(wahldaten2021, wahldaten2025, threshold = 0.12) {
  initial2021 <- prepare_vote_year(wahldaten2021, 2021, threshold = threshold)
  initial2025 <- prepare_vote_year(wahldaten2025, 2025, threshold = threshold)

  keep_parties <- dplyr::bind_rows(initial2021$national, initial2025$national) %>%
    dplyr::filter(.data$keep_party_year) %>%
    dplyr::pull(party) %>%
    unique() %>%
    sort()

  prepared2021 <- prepare_vote_year(wahldaten2021, 2021, threshold = threshold, keep_parties = keep_parties)
  prepared2025 <- prepare_vote_year(wahldaten2025, 2025, threshold = threshold, keep_parties = keep_parties)

  common_ids <- intersect(prepared2021$wide$agg_schluessel, prepared2025$wide$agg_schluessel)
  stopifnot(length(common_ids) == nrow(prepared2021$wide))
  stopifnot(length(common_ids) == nrow(prepared2025$wide))

  prepared2021$wide <- prepared2021$wide %>%
    dplyr::filter(.data$agg_schluessel %in% common_ids) %>%
    dplyr::arrange(.data$agg_schluessel)
  prepared2025$wide <- prepared2025$wide %>%
    dplyr::filter(.data$agg_schluessel %in% common_ids) %>%
    dplyr::arrange(.data$agg_schluessel)

  stopifnot(identical(prepared2021$wide$agg_schluessel, prepared2025$wide$agg_schluessel))

  scaled_inputs <- scale_2025_to_2021(prepared2021, prepared2025)
  prepared2021 <- scaled_inputs$prepared2021
  prepared2025 <- scaled_inputs$prepared2025
  scale_factors <- scaled_inputs$scale_factors

  input_groups <- setdiff(names(prepared2021$wide), "agg_schluessel")
  stopifnot(!any(c("CDU", "CSU") %in% input_groups))
  stopifnot("Union" %in% input_groups)
  stopifnot(identical(input_groups, setdiff(names(prepared2025$wide), "agg_schluessel")))
  stopifnot(all(rowSums(prepared2021$wide[input_groups]) == rowSums(prepared2025$wide[input_groups])))
  stopifnot(all(abs(prepared2021$check$differenz_input_zu_referenz) < 1e-8))
  stopifnot(all(abs(prepared2025$check$differenz_input_zu_referenz) < 1e-8))
  stopifnot(all(abs(prepared2021$check$differenz_stimmen_zu_waehlenden) < 1e-8))
  stopifnot(all(abs(prepared2025$check$differenz_stimmen_zu_waehlenden) < 1e-8))

  diagnostics <- make_nslphom_input_diagnostics(prepared2021$wide, prepared2025$wide)

  list(
    prepared2021 = prepared2021,
    prepared2025 = prepared2025,
    keep_parties = keep_parties,
    scale_factors = scale_factors,
    diagnostics = diagnostics
  )
}

make_nslphom_input_diagnostics <- function(input2021, input2025) {
  wahlberechtigte_agg <- dplyr::bind_rows(
    input2021 %>%
      dplyr::mutate(
        Jahr = 2021,
        wahlberechtigte_input = rowSums(dplyr::across(-agg_schluessel))
      ) %>%
      dplyr::select(Jahr, agg_schluessel, wahlberechtigte_input),
    input2025 %>%
      dplyr::mutate(
        Jahr = 2025,
        wahlberechtigte_input = rowSums(dplyr::across(-agg_schluessel))
      ) %>%
      dplyr::select(Jahr, agg_schluessel, wahlberechtigte_input)
  )

  wahlberechtigte_summary <- wahlberechtigte_agg %>%
    dplyr::group_by(.data$Jahr) %>%
    dplyr::summarise(
      n_agg = dplyr::n(),
      min = min(.data$wahlberechtigte_input, na.rm = TRUE),
      q10 = stats::quantile(.data$wahlberechtigte_input, 0.10, na.rm = TRUE),
      q25 = stats::quantile(.data$wahlberechtigte_input, 0.25, na.rm = TRUE),
      median = stats::median(.data$wahlberechtigte_input, na.rm = TRUE),
      mean = mean(.data$wahlberechtigte_input, na.rm = TRUE),
      q75 = stats::quantile(.data$wahlberechtigte_input, 0.75, na.rm = TRUE),
      q90 = stats::quantile(.data$wahlberechtigte_input, 0.90, na.rm = TRUE),
      q95 = stats::quantile(.data$wahlberechtigte_input, 0.95, na.rm = TRUE),
      max = max(.data$wahlberechtigte_input, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    wahlberechtigte_agg = wahlberechtigte_agg,
    wahlberechtigte_summary = wahlberechtigte_summary
  )
}

plot_nslphom_input_diagnostics <- function(diagnostics) {
  View(diagnostics$wahlberechtigte_summary)
  View(diagnostics$wahlberechtigte_agg)

  print(
    ggplot2::ggplot(diagnostics$wahlberechtigte_agg, ggplot2::aes(x = .data$wahlberechtigte_input)) +
      ggplot2::geom_histogram(bins = 80, fill = "grey50", color = "white") +
      ggplot2::facet_wrap(~Jahr, scales = "free_y") +
      ggplot2::labs(
        title = "Wahlberechtigte pro agg.schluessel",
        x = "Wahlberechtigte / Input-Gesamtmasse",
        y = "Anzahl agg.schluessel"
      ) +
      ggplot2::theme_minimal()
  )

  cutoff <- stats::quantile(diagnostics$wahlberechtigte_agg$wahlberechtigte_input, 0.95, na.rm = TRUE)

  print(
    ggplot2::ggplot(
      diagnostics$wahlberechtigte_agg %>%
        dplyr::filter(.data$wahlberechtigte_input <= cutoff),
      ggplot2::aes(x = .data$wahlberechtigte_input)
    ) +
      ggplot2::geom_histogram(bins = 80, fill = "grey50", color = "white") +
      ggplot2::facet_wrap(~Jahr, scales = "free_y") +
      ggplot2::labs(
        title = "Wahlberechtigte pro agg.schluessel, untere 95 %",
        subtitle = paste("Rechter Rand abgeschnitten bei", round(cutoff), "Wahlberechtigten"),
        x = "Wahlberechtigte / Input-Gesamtmasse",
        y = "Anzahl agg.schluessel"
      ) +
      ggplot2::theme_minimal()
  )

  invisible(diagnostics)
}

save_nslphom_inputs <- function(prepared_inputs) {
  prepared2021 <- prepared_inputs$prepared2021
  prepared2025 <- prepared_inputs$prepared2025

  saveRDS(prepared2021$wide, file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_2021.rds"))
  saveRDS(prepared2025$wide, file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_2025.rds"))
  saveRDS(dplyr::bind_rows(prepared2021$long, prepared2025$long), file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_long.rds"))
  saveRDS(dplyr::bind_rows(prepared2021$national, prepared2025$national), file.path(data_dir_cleaned, "vorlaeufig_partei_schwellenwerte.rds"))
  saveRDS(dplyr::bind_rows(prepared2021$check, prepared2025$check), file.path(data_dir_validation, "vorlaeufig_nslphom_input_checks.rds"))
  saveRDS(prepared_inputs$scale_factors, file.path(data_dir_validation, "vorlaeufig_nslphom_input_scaling_2025_to_2021.rds"))

  invisible(prepared_inputs)
}
