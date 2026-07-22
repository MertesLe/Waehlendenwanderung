library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")

ensure_data_dirs()

ehet_input_path <- getOption(
  "waehlendenwanderung.ehet_nslphom_output_path",
  file.path(
    data_dir_model_nslphom,
    "test5000_first",
    "vorlaeufig_test2000_nslphom_unblocked_endoutput.rds"
  )
)

geometry_path <- getOption(
  "waehlendenwanderung.ehet_geometry_path",
  file.path(
    "Data",
    "raw",
    "gebiete_visualisierung",
    "vg250_01-01.utm32s.gpkg.ebenen",
    "vg250_ebenen_0101",
    "DE_VG250.gpkg"
  )
)

geometry_layer <- getOption("waehlendenwanderung.ehet_geometry_layer", "vg250_gem")
output_dir <- file.path(data_dir_validation, "homogenitaetsannahme")
chart_dir <- file.path("Charts", "homogenitaetsannahme")
run_label <- getOption("waehlendenwanderung.ehet_run_label", basename(dirname(ehet_input_path)))
run_label <- str_replace_all(run_label, "[^A-Za-z0-9_]+", "_")
run_prefix <- paste0("vorlaeufig_", run_label, "_ehet")
save_data_outputs <- isTRUE(getOption("waehlendenwanderung.ehet_save_data_outputs", TRUE))
save_diagnostic_plots <- isTRUE(getOption("waehlendenwanderung.ehet_save_diagnostic_plots", TRUE))
save_map_plot <- isTRUE(getOption("waehlendenwanderung.ehet_save_map_plot", TRUE))

data_file <- function(name) {
  file.path(output_dir, paste0(run_prefix, "_", name))
}

chart_file <- function(name) {
  file.path(chart_dir, paste0(run_prefix, "_", name))
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(ehet_input_path)) {
  stop("Der nslphom-Endoutput wurde nicht gefunden: ", ehet_input_path)
}

nslphom_output <- readRDS(ehet_input_path)

if (!all(c("local_matrices_long", "global_matrix") %in% names(nslphom_output))) {
  stop(
    "Der nslphom-Endoutput muss local_matrices_long und global_matrix enthalten. ",
    "Aktuell enthalten: ",
    paste(names(nslphom_output), collapse = ", ")
  )
}

local_transitions <- nslphom_output$local_matrices_long
global_matrix <- nslphom_output$global_matrix

required_transition_cols <- c(
  "agg_schluessel",
  "from",
  "to",
  "origin_count",
  "estimated_transition_count"
)

missing_transition_cols <- setdiff(required_transition_cols, names(local_transitions))
if (length(missing_transition_cols) > 0) {
  stop("local_matrices_long enthaelt nicht alle benoetigten Spalten: ", paste(missing_transition_cols, collapse = ", "))
}

if (!all(c("from", "to", "transition_probability") %in% names(global_matrix))) {
  stop("global_matrix muss from, to und transition_probability enthalten.")
}

# lphom speichert globale Uebergangswahrscheinlichkeiten teilweise in Prozent.
# Wenn die Matrix eine Prozentmatrix ist, muessen alle Zellen geteilt werden,
# auch kleine Werte wie 0.52 Prozent.
global_probabilities_are_percent <- max(global_matrix$transition_probability, na.rm = TRUE) > 1 + 1e-8

global_probabilities <- global_matrix %>%
  transmute(
    from,
    to,
    global_probability = if (global_probabilities_are_percent) {
      transition_probability / 100
    } else {
      transition_probability
    }
  )

global_row_check <- global_probabilities %>%
  group_by(from) %>%
  summarise(
    row_sum = sum(global_probability, na.rm = TRUE),
    .groups = "drop"
  )

if (max(abs(global_row_check$row_sum - 1), na.rm = TRUE) > 0.02) {
  stop("Die globalen Uebergangswahrscheinlichkeiten summieren sich zeilenweise nicht plausibel zu 1.")
}

wahlberechtigte_by_unit <- local_transitions %>%
  distinct(agg_schluessel, from, origin_count) %>%
  group_by(agg_schluessel) %>%
  summarise(
    wahlberechtigte = sum(origin_count, na.rm = TRUE),
    .groups = "drop"
  )

# Wenn das nslphom-Endobjekt fit$EHet enthaelt, wird diese Modellinformation
# direkt genutzt. fit$EHet ist eine Matrix aus Einheiten x Zielgruppen; sie
# misst je Einheit die Zielgruppen-Abweichung vom homogenen globalen Modell.
if ("EHet" %in% names(nslphom_output) && !is.null(nslphom_output$EHet)) {
  ehet_source <- "fit_EHet"
  ehet_matrix <- as.matrix(nslphom_output$EHet)
  ehet_ids <- if ("EHet_ids" %in% names(nslphom_output)) {
    as.character(nslphom_output$EHet_ids)
  } else {
    rownames(ehet_matrix)
  }

  if (length(ehet_ids) != nrow(ehet_matrix) || any(is.na(ehet_ids))) {
    stop("EHet ist vorhanden, aber seine Zeilen koennen keinen agg.schluesseln zugeordnet werden.")
  }

  rownames(ehet_matrix) <- ehet_ids
  if (is.null(colnames(ehet_matrix))) {
    colnames(ehet_matrix) <- sort(unique(local_transitions$to))
  }

  local_ids <- unique(local_transitions$agg_schluessel)
  if (!setequal(ehet_ids, local_ids)) {
    stop("Die EHet-Zeilen passen nicht zu den lokalen nslphom-Matrizen.")
  }

  ehet_cells <- as.data.frame(as.table(ehet_matrix), stringsAsFactors = FALSE) %>%
    transmute(
      agg_schluessel = as.character(Var1),
      to = as.character(Var2),
      ehet_deviation_count = as.numeric(Freq),
      abs_ehet_deviation_count = abs(ehet_deviation_count),
      ehet_source = ehet_source
    )

  ehet_unit_metrics <- ehet_cells %>%
    group_by(agg_schluessel) %>%
    summarise(
      ehet_abs_sum = sum(abs_ehet_deviation_count, na.rm = TRUE),
      ehet_abs_half = 0.5 * ehet_abs_sum,
      max_abs_cell_deviation = max(abs_ehet_deviation_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(wahlberechtigte_by_unit, by = "agg_schluessel") %>%
    mutate(
      ehet_index = ehet_abs_half / wahlberechtigte,
      ehet_index_percent = 100 * ehet_index,
      n_gemeindeschluessel = lengths(lapply(agg_schluessel, split_keys)),
      ehet_source = ehet_source
    ) %>%
    arrange(desc(ehet_index))

  # fit$EHet ist zielgruppenbezogen. Herkunftsgruppenbeitraege lassen sich
  # daraus nicht eindeutig trennen; diese Tabelle bleibt deshalb leer.
  ehet_by_from <- tibble::tibble(
    agg_schluessel = character(),
    from = character(),
    ehet_abs_half_from = numeric(),
    wahlberechtigte = numeric(),
    ehet_index_from = numeric(),
    ehet_source = character()
  )

  ehet_by_to <- ehet_cells %>%
    group_by(agg_schluessel, to) %>%
    summarise(
      ehet_abs_half_to = 0.5 * sum(abs_ehet_deviation_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(wahlberechtigte_by_unit, by = "agg_schluessel") %>%
    mutate(
      ehet_index_to = ehet_abs_half_to / wahlberechtigte,
      ehet_source = ehet_source
    )
} else {
  ehet_source <- "reconstructed_transition_cells"

  # Fallback fuer alte Endoutputs ohne fit$EHet: Homogenitaet bedeutet hier,
  # dass jede Einheit innerhalb einer Herkunftsgruppe dieselbe globale
  # Uebergangswahrscheinlichkeit haette.
  ehet_cells <- local_transitions %>%
    left_join(global_probabilities, by = c("from", "to")) %>%
    mutate(
      expected_count_homogeneous = origin_count * global_probability,
      ehet_deviation_count = estimated_transition_count - expected_count_homogeneous,
      abs_ehet_deviation_count = abs(ehet_deviation_count),
      ehet_source = ehet_source
    )

  if (any(is.na(ehet_cells$global_probability))) {
    stop("Mindestens eine lokale Uebergangszelle konnte keiner globalen Uebergangswahrscheinlichkeit zugeordnet werden.")
  }

  ehet_unit_metrics <- ehet_cells %>%
    group_by(agg_schluessel) %>%
    summarise(
      ehet_abs_sum = sum(abs_ehet_deviation_count, na.rm = TRUE),
      ehet_abs_half = 0.5 * ehet_abs_sum,
      max_abs_cell_deviation = max(abs_ehet_deviation_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(wahlberechtigte_by_unit, by = "agg_schluessel") %>%
    mutate(
      ehet_index = ehet_abs_half / wahlberechtigte,
      ehet_index_percent = 100 * ehet_index,
      n_gemeindeschluessel = lengths(lapply(agg_schluessel, split_keys)),
      ehet_source = ehet_source
    ) %>%
    arrange(desc(ehet_index))

  ehet_by_from <- ehet_cells %>%
    group_by(agg_schluessel, from) %>%
    summarise(
      ehet_abs_half_from = 0.5 * sum(abs_ehet_deviation_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(wahlberechtigte_by_unit, by = "agg_schluessel") %>%
    mutate(
      ehet_index_from = ehet_abs_half_from / wahlberechtigte,
      ehet_source = ehet_source
    )

  ehet_by_to <- ehet_cells %>%
    group_by(agg_schluessel, to) %>%
    summarise(
      ehet_abs_half_to = 0.5 * sum(abs_ehet_deviation_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(wahlberechtigte_by_unit, by = "agg_schluessel") %>%
    mutate(
      ehet_index_to = ehet_abs_half_to / wahlberechtigte,
      ehet_source = ehet_source
    )
}

if (any(is.na(ehet_unit_metrics$wahlberechtigte))) {
  stop("Mindestens eine EHet-Einheit konnte keiner Wahlberechtigtenzahl zugeordnet werden.")
}

ehet_summary <- ehet_unit_metrics %>%
  summarise(
    n_units = n(),
    min = min(ehet_index, na.rm = TRUE),
    q05 = quantile(ehet_index, 0.05, na.rm = TRUE),
    q25 = quantile(ehet_index, 0.25, na.rm = TRUE),
    median = median(ehet_index, na.rm = TRUE),
    mean = mean(ehet_index, na.rm = TRUE),
    q75 = quantile(ehet_index, 0.75, na.rm = TRUE),
    q90 = quantile(ehet_index, 0.90, na.rm = TRUE),
    q95 = quantile(ehet_index, 0.95, na.rm = TRUE),
    q99 = quantile(ehet_index, 0.99, na.rm = TRUE),
    max = max(ehet_index, na.rm = TRUE)
  ) %>%
  mutate(
    run_label = run_label,
    ehet_source = ehet_source,
    input_path = normalizePath(ehet_input_path, winslash = "/", mustWork = FALSE),
    .before = 1
  )

if (save_data_outputs) {
  saveRDS(ehet_cells, data_file("abweichungen.rds"))
  saveRDS(ehet_unit_metrics, data_file("einheiten.rds"))
  saveRDS(ehet_by_from, data_file("nach_herkunft.rds"))
  saveRDS(ehet_by_to, data_file("nach_ziel.rds"))
  saveRDS(ehet_summary, data_file("summary.rds"))
}

hist_plot <- ggplot(ehet_unit_metrics, aes(x = ehet_index)) +
  geom_histogram(bins = 60, fill = "grey55", color = "white") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Heterogenitaet der lokalen Uebergangsmatrizen",
    subtitle = "0.5 * Summe absoluter Zellabweichungen von der globalen Matrix, relativ zu Wahlberechtigten",
    x = "Heterogenitaetsindex",
    y = "Anzahl agg.schluessel"
  ) +
  theme_minimal()

size_plot <- ggplot(ehet_unit_metrics, aes(x = wahlberechtigte, y = ehet_index)) +
  geom_point(alpha = 0.45, size = 1.2) +
  scale_x_log10(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Heterogenitaet nach Groesse der Analyseeinheit",
    x = "Wahlberechtigte, log-Skala",
    y = "Heterogenitaetsindex"
  ) +
  theme_minimal()

top_plot <- ehet_unit_metrics %>%
  slice_max(ehet_index, n = 25, with_ties = FALSE) %>%
  mutate(agg_schluessel = reorder(agg_schluessel, ehet_index)) %>%
  ggplot(aes(x = ehet_index, y = agg_schluessel)) +
  geom_col(fill = "grey40") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Top-25 Einheiten nach Heterogenitaetsindex",
    x = "Heterogenitaetsindex",
    y = "agg.schluessel"
  ) +
  theme_minimal()

if (save_diagnostic_plots) {
  ggsave(chart_file("histogramm.png"), hist_plot, width = 9, height = 6, dpi = 300, bg = "white")
  ggsave(chart_file("groesse_scatter.png"), size_plot, width = 9, height = 6, dpi = 300, bg = "white")
  ggsave(chart_file("top25.png"), top_plot, width = 10, height = 8, dpi = 300, bg = "white")
}

if (save_data_outputs) {
  message("EHet-Kennzahlen gespeichert unter: ", output_dir)
} else {
  message("EHet-Kennzahlen berechnet, aber nicht gespeichert.")
}

if (save_diagnostic_plots) {
  message("EHet-Diagnoseplots gespeichert unter: ", chart_dir)
} else {
  message("EHet-Diagnoseplots wurden wegen waehlendenwanderung.ehet_save_diagnostic_plots = FALSE uebersprungen.")
}

if (!save_map_plot) {
  message("EHet-Karte wurde wegen waehlendenwanderung.ehet_save_map_plot = FALSE uebersprungen.")
} else if (!requireNamespace("sf", quietly = TRUE)) {
  message(
    "Das Paket 'sf' ist nicht installiert. ",
    "Die nicht-raeumliche EHet-Analyse wurde erstellt; die Karte wird uebersprungen."
  )
} else {
  if (!file.exists(geometry_path)) {
    stop("Die Gemeindeflaechen-Datei wurde nicht gefunden: ", geometry_path)
  }

  gemeinde_geometrien <- sf::st_read(
    geometry_path,
    layer = geometry_layer,
    quiet = TRUE
  )

  ags_col <- if ("AGS" %in% names(gemeinde_geometrien)) {
    "AGS"
  } else {
    grep("Gemeindesch.*AGS|Gemeindesch", names(gemeinde_geometrien), value = TRUE)[1]
  }

  name_col <- if ("GEN" %in% names(gemeinde_geometrien)) {
    "GEN"
  } else {
    grep("GeografischerName|GEN", names(gemeinde_geometrien), value = TRUE)[1]
  }

  if (is.na(ags_col)) {
    stop("In der Geometriedatei wurde keine AGS-Spalte gefunden.")
  }

  gemeinde_geometrien <- gemeinde_geometrien %>%
    mutate(
      gemeindeschluessel = as.character(.data[[ags_col]]),
      gemeindename = if (!is.na(name_col)) as.character(.data[[name_col]]) else NA_character_
    )

  geometrie_keys <- unique(gemeinde_geometrien$gemeindeschluessel)

  ehet_ags_long <- ehet_unit_metrics %>%
    select(agg_schluessel, ehet_index, ehet_index_percent, ehet_abs_half, wahlberechtigte) %>%
    mutate(gemeindeschluessel = lapply(agg_schluessel, split_keys)) %>%
    unnest(gemeindeschluessel) %>%
    mutate(
      hat_direkte_geometrie = gemeindeschluessel %in% geometrie_keys,
      ersatz_geometrie = case_when(
        hat_direkte_geometrie ~ gemeindeschluessel,
        str_starts(gemeindeschluessel, "02") & "02000000" %in% geometrie_keys ~ "02000000",
        str_starts(gemeindeschluessel, "11") & "11000000" %in% geometrie_keys ~ "11000000",
        TRUE ~ NA_character_
      )
    )

  geometry_status <- ehet_ags_long %>%
    group_by(agg_schluessel) %>%
    summarise(
      n_gemeindeschluessel = n(),
      n_direkte_geometrien = sum(hat_direkte_geometrie),
      n_fehlende_gemeindegeometrien = sum(!hat_direkte_geometrie),
      n_ersatz_geometrien = sum(!hat_direkte_geometrie & !is.na(ersatz_geometrie)),
      fehlende_gemeindeschluessel = paste(gemeindeschluessel[!hat_direkte_geometrie], collapse = ", "),
      .groups = "drop"
    ) %>%
    mutate(
      geometrie_status = case_when(
        n_fehlende_gemeindegeometrien == 0 ~ "vollstaendig",
        n_direkte_geometrien > 0 ~ "teilweise_2025_geometrie",
        n_ersatz_geometrien > 0 ~ "fehlende_untergliederung_grau",
        TRUE ~ "keine_geometrie"
      )
    )

  if (save_data_outputs) {
    saveRDS(geometry_status, data_file("geometrie_status.rds"))
  }

  # Standardfall: alle direkt vorhandenen Gemeindegeometrien eines agg.schluessels
  # werden vereinigt. Wenn einzelne historische Bestandteile 2025 nicht mehr als
  # eigene Gemeindegeometrie existieren, wird die Einheit als teilweise markiert.
  direct_lookup <- ehet_ags_long %>%
    filter(hat_direkte_geometrie) %>%
    select(agg_schluessel, gemeindeschluessel) %>%
    distinct()

  direct_map <- gemeinde_geometrien %>%
    inner_join(direct_lookup, by = "gemeindeschluessel") %>%
    left_join(ehet_unit_metrics, by = "agg_schluessel") %>%
    left_join(geometry_status, by = "agg_schluessel") %>%
    group_by(agg_schluessel) %>%
    summarise(
      ehet_index = first(ehet_index),
      ehet_index_percent = first(ehet_index_percent),
      ehet_abs_half = first(ehet_abs_half),
      wahlberechtigte = first(wahlberechtigte),
      n_gemeindeschluessel = first(n_gemeindeschluessel.x),
      n_fehlende_gemeindegeometrien = first(n_fehlende_gemeindegeometrien),
      geometrie_status = first(geometrie_status),
      .groups = "drop"
    )

  missing_substitute_lookup <- ehet_ags_long %>%
    group_by(agg_schluessel) %>%
    filter(!any(hat_direkte_geometrie)) %>%
    ungroup() %>%
    filter(!is.na(ersatz_geometrie)) %>%
    distinct(ersatz_geometrie, agg_schluessel) %>%
    left_join(ehet_unit_metrics, by = "agg_schluessel") %>%
    group_by(ersatz_geometrie) %>%
    summarise(
      n_agg_schluessel = n_distinct(agg_schluessel),
      agg_schluessel = paste(sort(unique(agg_schluessel)), collapse = ", "),
      ehet_abs_half = sum(ehet_abs_half, na.rm = TRUE),
      wahlberechtigte = sum(wahlberechtigte, na.rm = TRUE),
      ehet_index = NA_real_,
      ehet_index_percent = NA_real_,
      geometrie_status = "fehlende_untergliederung_grau",
      .groups = "drop"
    )

  # Berlin/Hamburg-Untergliederungen fehlen in VG250 als eigenstaendige
  # Gemeindegeometrien. Vorerst werden die Gesamtstadt-Flaechen grau gezeigt,
  # damit sichtbar bleibt, dass hier keine kleinraeumige Flaeche verfuegbar ist.
  substitute_map <- gemeinde_geometrien %>%
    inner_join(missing_substitute_lookup, by = c("gemeindeschluessel" = "ersatz_geometrie")) %>%
    group_by(gemeindeschluessel) %>%
    summarise(
      agg_schluessel = first(agg_schluessel),
      ehet_index = first(ehet_index),
      ehet_index_percent = first(ehet_index_percent),
      ehet_abs_half = first(ehet_abs_half),
      wahlberechtigte = first(wahlberechtigte),
      n_gemeindeschluessel = NA_integer_,
      n_fehlende_gemeindegeometrien = NA_integer_,
      geometrie_status = first(geometrie_status),
      .groups = "drop"
    )

  ehet_map_data <- bind_rows(
    direct_map,
    substitute_map
  )

  if (save_data_outputs) {
    saveRDS(ehet_map_data, data_file("karte_geometrien.rds"))
  }

  map_plot <- ggplot() +
    # Die komplette 2025-Gemeindeflaeche wird grau unterlegt. Dadurch sind
    # nicht im jeweiligen Testfit enthaltene Regionen sichtbar statt weiss.
    geom_sf(
      data = gemeinde_geometrien,
      fill = "grey88",
      color = NA
    ) +
    geom_sf(
      data = ehet_map_data %>% filter(is.na(ehet_index)),
      fill = "grey82",
      color = NA
    ) +
    geom_sf(
      data = ehet_map_data %>% filter(!is.na(ehet_index)),
      aes(fill = ehet_index),
      color = NA
    ) +
    scale_fill_gradientn(
      colors = c("#f7fbff", "#9ecae1", "#3182bd", "#08519c"),
      labels = percent_format(accuracy = 1),
      name = "EHet relativ"
    ) +
    coord_sf(datum = NA) +
    labs(
      title = "Heterogenitaet der lokalen Uebergangsmatrizen",
      subtitle = "Grau: nicht im Testfit enthalten oder keine passende kleinraeumige Gemeindegeometrie im VG250-Stand 01.01.2025",
      caption = "Index = 0.5 * Summe absoluter Zellabweichungen von der globalen Matrix / Wahlberechtigte"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey35"),
      plot.caption = element_text(color = "grey45", hjust = 0)
    )

  ggsave(
    chart_file("deutschlandkarte.png"),
    map_plot,
    width = 9,
    height = 11,
    dpi = 300,
    bg = "white"
  )

  message("EHet-Karte gespeichert unter: ", chart_file("deutschlandkarte.png"))
}
