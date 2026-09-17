# Funktionen zur Aufbereitung der Zweitstimmen fuer den nslphom-Input.

# Unterschiedliche amtliche Parteinamen auf einheitliche Gruppennamen abbilden.
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
    DIE_LINKE = "LINKE_GRUENE",
    Die_Linke = "LINKE_GRUENE",
    GRUENE = "LINKE_GRUENE",
    GRUNE = "LINKE_GRUENE",
    .default = party
  )
}

# Wahldaten eines Jahres auf Aggregationseinheiten und standardisierte Parteien vorbereiten.
prepare_vote_year <- function(
    data,
    jahr,
    party_cols,
    invalid_col,
    valid_col,
    threshold = 0.12) {
  agg_col <- get_agg_col(data)
  a_col <- first_existing(data, c("^Wahlberechtigte"))
  b_col <- first_existing(data, c("^W.hlende"))

  stopifnot(!is.na(agg_col), !is.na(a_col), !is.na(b_col))
  stopifnot(!is.na(invalid_col), !is.na(valid_col))
  stopifnot(length(party_cols) > 0)

  # Urspruengliche Zweitstimmenspalten ihren vereinheitlichten Parteien zuordnen.
  party_lookup <- tibble::tibble(
    source_col = party_cols,
    party = standardize_party(party_cols)
  )

  # CDU und CSU sowie DIE LINKE und GRUENE jeweils zu ihrer gemeinsamen Gruppe addieren.
  party_counts <- data %>%
    dplyr::transmute(
      agg_schluessel = .data[[agg_col]],
      dplyr::across(dplyr::all_of(party_cols))
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(party_cols),
      names_to = "source_col",
      values_to = "votes"
    ) %>%
    dplyr::left_join(party_lookup, by = "source_col") %>%
    dplyr::group_by(.data$agg_schluessel, .data$party) %>%
    dplyr::summarise(
      votes = sum(.data$votes, na.rm = TRUE),
      .groups = "drop"
    )

  # Nationale Zweitstimmenanteile bestimmen, auf denen die Parteischwelle beruht.
  national <- party_counts %>%
    dplyr::group_by(.data$party) %>%
    dplyr::summarise(votes = sum(.data$votes, na.rm = TRUE), .groups = "drop") %>%
    dplyr::left_join(
      party_lookup %>%
        dplyr::group_by(.data$party) %>%
        dplyr::summarise(
          source_col = paste(.data$source_col, collapse = ", "),
          .groups = "drop"
        ),
      by = "party"
    ) %>%
    dplyr::mutate(
      valid_total = sum(data[[valid_col]], na.rm = TRUE),
      threshold = threshold,
      national_share_valid = .data$votes / .data$valid_total,
      keep_party_year = .data$national_share_valid >= .data$threshold
    ) %>%
    dplyr::select(
      party,
      source_col,
      votes,
      valid_total,
      threshold,
      national_share_valid,
      keep_party_year
    )

  # Ungueltige Stimmen als Andere und Nichtwaehlende als eigene Gruppen ergaenzen.
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
    )

  # Amtliche Gesamtwerte fuer spaetere Rekonstruktionschecks je Einheit sichern.
  check_base <- data %>%
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
    )

  list(
    jahr = jahr,
    party_counts = party_counts,
    residual = residual,
    national = national,
    check_base = check_base
  )
}

# Kleine Parteien zu Andere zusammenfassen und eine vollstaendige Gruppenmatrix erzeugen.
finalize_vote_year <- function(prepared_year, keep_parties) {
  keep_parties <- sort(unique(keep_parties))
  output_groups <- c(keep_parties, "Andere", "Nichtwaehler")

  # Nicht separat gefuehrte Parteien und ungueltige Stimmen gemeinsam zu Andere addieren.
  counts <- dplyr::bind_rows(
    prepared_year$party_counts %>%
      dplyr::transmute(
        agg_schluessel,
        gruppe = dplyr::if_else(.data$party %in% keep_parties, .data$party, "Andere"),
        votes
      ),
    prepared_year$residual
  ) %>%
    dplyr::group_by(.data$agg_schluessel, .data$gruppe) %>%
    dplyr::summarise(votes = sum(.data$votes, na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(
      agg_schluessel,
      gruppe = output_groups,
      fill = list(votes = 0)
    )

  # Fuer nslphom eine breite Matrix mit denselben Gruppen in jedem Jahr erzeugen.
  wide <- counts %>%
    dplyr::mutate(gruppe = factor(.data$gruppe, levels = output_groups)) %>%
    tidyr::pivot_wider(
      names_from = gruppe,
      values_from = votes,
      values_fill = 0
    ) %>%
    dplyr::arrange(.data$agg_schluessel) %>%
    dplyr::select(agg_schluessel, dplyr::all_of(output_groups))

  # Pruefen, ob Parteien, Andere und Nichtwaehlende die amtliche Gesamtmasse ergeben.
  check <- prepared_year$check_base %>%
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
    long = counts %>% dplyr::mutate(Jahr = prepared_year$jahr, .before = 1),
    national = prepared_year$national %>%
      dplyr::mutate(keep_party = .data$party %in% keep_parties) %>%
      dplyr::mutate(
        Jahr = prepared_year$jahr,
        .before = 1
      ),
    check = check %>% dplyr::mutate(Jahr = prepared_year$jahr, .before = 1)
  )
}

# Alle 2025-Gruppen je Einheit proportional auf die Gesamtmasse von 2021 skalieren.
scale_2025_to_2021 <- function(prepared2021, prepared2025) {
  groups <- setdiff(names(prepared2021$wide), "agg_schluessel")
  stopifnot(identical(groups, setdiff(names(prepared2025$wide), "agg_schluessel")))

  # Je Einheit den Faktor Gesamtmasse 2021 geteilt durch Gesamtmasse 2025 berechnen.
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

  # Jede Gruppe 2025 mit demselben einheitsspezifischen Faktor multiplizieren.
  prepared2025$wide <- prepared2025$wide %>%
    dplyr::left_join(
      scale_factors %>% dplyr::select(agg_schluessel, skalierungsfaktor_2025),
      by = "agg_schluessel"
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(groups), ~ .x * .data$skalierungsfaktor_2025)
    ) %>%
    dplyr::select(agg_schluessel, dplyr::all_of(groups))

  # Rundungsreste einer Gruppe zuweisen, damit die skalierten Summen exakt 2021 entsprechen.
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

  # Nach der Skalierung Longformat und Plausibilitaetswerte konsistent neu erzeugen.
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

# Plausibilitaetskennzahlen und Verteilungen fuer die fertigen Inputmatrizen berechnen.
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

# Histogramme der Einheitsgroessen aus den Inputdiagnosen erzeugen.
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
