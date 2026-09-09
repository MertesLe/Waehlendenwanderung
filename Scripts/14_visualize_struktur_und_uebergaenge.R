# Ausgewaehlte Strukturvariablen und lokale nslphom-Uebergaenge fuer Ostdeutschland kartieren.

library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(tidyr)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")

# Hier die zu visualisierenden Strukturvariablen auswaehlen.
strukturvariablen <- c(
  "distanz_staatsgrenze_km_2023",
  "einwohnerdichte_2023"
)

# Hier die zu visualisierenden nslphom-Uebergaenge auswaehlen.
# CDU und CSU werden im Workflow gemeinsam als Union ausgewiesen.
uebergaenge <- tibble::tribble(
  ~from,    ~to,   ~kennzahl,
  "Union",  "AfD", "transition_probability"
)

# Lesbare Kartentitel fuer bekannte Strukturvariablen festlegen.
struktur_labels <- c(
  distanz_staatsgrenze_km_2023 = "Entfernung zur auslaendischen Staatsgrenze (km)",
  einwohnerdichte_2023 = "Einwohnerdichte (Einwohner je km2)",
  supermarktEntfernung_2023 = "Entfernung zum naechsten Supermarkt (m)",
  alterMean_2023 = "Durchschnittsalter der Bevoelkerung (Jahre)",
  wanderungssaldo_2023 = "Wanderungssaldo je 1.000 Einwohner",
  pendler50_2023 = "Beschaeftigte mit mindestens 50 km Arbeitsweg (Prozent)",
  steuereinnahmen_2023 = "Steuereinnahmen je Einwohner (Euro)",
  haushaltsgroesseMean_2023 = "Durchschnittliche Haushaltsgroesse (Personen)",
  anteilHaushalteNiedrigesEinkommen_2023 = "Haushalte mit niedrigem Einkommen (Prozent)",
  arbeitslosenanteilErwerbsfaehige_2023 = "Arbeitslose an der Bevoelkerung von 15 bis unter 65 (Prozent)"
)

struktur_path <- file.path(
  data_dir_cleaned,
  "vorlaeufig_inkar_kovariaten_2023.rds"
)
transition_path <- file.path(
  data_dir_model_nslphom_ost,
  "vorlaeufig_transition_matrices_long.rds"
)
geometry_path <- paste0(
  "Data/raw/gebiete_visualisierung/",
  "vg250_01-01.utm32s.gpkg.ebenen/vg250_ebenen_0101/DE_VG250.gpkg"
)
geometry_layer <- "vg250_gem"
chart_dir <- "Charts/Strukturvariablen"

dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)

required_files <- c(struktur_path, transition_path, geometry_path)
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop("Folgende Eingabedateien fehlen: ", paste(missing_files, collapse = ", "))
}

struktur <- readRDS(struktur_path) %>%
  filter(is_ostdeutschland_ohne_berlin(.data$agg_schluessel))

transitions <- readRDS(transition_path) %>%
  filter(is_ostdeutschland_ohne_berlin(.data$agg_schluessel))

missing_strukturvariablen <- setdiff(strukturvariablen, names(struktur))

if (length(missing_strukturvariablen) > 0) {
  stop(
    "Folgende Strukturvariablen fehlen: ",
    paste(missing_strukturvariablen, collapse = ", ")
  )
}

if (any(!vapply(struktur[strukturvariablen], is.numeric, logical(1)))) {
  stop("Alle ausgewaehlten Strukturvariablen muessen numerisch sein.")
}

required_transition_cols <- c(
  "agg_schluessel",
  "from",
  "to",
  "transition_probability",
  "estimated_transition_count"
)
missing_transition_cols <- setdiff(required_transition_cols, names(transitions))

if (length(missing_transition_cols) > 0) {
  stop(
    "Folgende Spalten fehlen in den nslphom-Ergebnissen: ",
    paste(missing_transition_cols, collapse = ", ")
  )
}

allowed_metrics <- c("transition_probability", "estimated_transition_count")

if (any(!uebergaenge$kennzahl %in% allowed_metrics)) {
  stop(
    "kennzahl muss transition_probability oder estimated_transition_count sein."
  )
}

# Nur regulaere ostdeutsche Gemeindeflaechen des Gebietsstands 2025 einlesen.
gemeinde_geometrien <- st_read(
  geometry_path,
  layer = geometry_layer,
  quiet = TRUE
) %>%
  filter(
    .data$GF == 4,
    .data$BSG == 1,
    substr(as.character(.data$AGS), 1, 2) %in% c("12", "13", "14", "15", "16")
  ) %>%
  mutate(gemeindeschluessel = normalize_ags(.data$AGS)) %>%
  select(gemeindeschluessel)

# Aus den Gemeindeschluesseln die Flaeche jeder harmonisierten Aggregation einmalig bilden.
# Historische Schluessel ohne eigene 2025-Geometrie sind unproblematisch, wenn
# ihr heutiger Nachfolger ebenfalls im agg_schluessel enthalten ist.
all_agg_ids <- union(
  unique(struktur$agg_schluessel),
  unique(transitions$agg_schluessel)
)

key_lookup <- tibble(agg_schluessel = all_agg_ids) %>%
  mutate(gemeindeschluessel = lapply(.data$agg_schluessel, split_keys)) %>%
  unnest(gemeindeschluessel) %>%
  filter(.data$gemeindeschluessel %in% gemeinde_geometrien$gemeindeschluessel) %>%
  select(agg_schluessel, gemeindeschluessel)

missing_geometry <- setdiff(all_agg_ids, key_lookup$agg_schluessel)

if (length(missing_geometry) > 0) {
  stop(
    "Fuer folgende Aggregationseinheiten fehlt jede Gemeindegeometrie: ",
    paste(missing_geometry, collapse = "; ")
  )
}

agg_geometrien <- gemeinde_geometrien %>%
  inner_join(key_lookup, by = "gemeindeschluessel") %>%
  group_by(.data$agg_schluessel) %>%
  summarise(.groups = "drop")

# Einen eindeutigen Wert an die bereits vorbereiteten Aggregationsflaechen anfuegen.
build_agg_map <- function(values) {
  values <- values %>%
    select(agg_schluessel, value) %>%
    distinct()

  if (anyDuplicated(values$agg_schluessel)) {
    stop("Eine Aggregationseinheit besitzt mehr als einen Kartenwert.")
  }

  agg_geometrien %>%
    inner_join(values, by = "agg_schluessel")
}

# Dateinamen aus Variablen- und Parteinamen erzeugen.
safe_filename <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

# Eine Ostdeutschlandkarte fuer einen numerischen Wert speichern.
save_map <- function(map_data, title, legend_title, filename, probability = FALSE) {
  legend_labels <- if (probability) {
    scales::label_percent(accuracy = 0.1)
  } else {
    scales::label_number(big.mark = ".", decimal.mark = ",", accuracy = 0.1)
  }

  plot <- ggplot() +
    geom_sf(
      data = gemeinde_geometrien,
      fill = "grey88",
      color = "white",
      linewidth = 0.05
    ) +
    geom_sf(
      data = map_data,
      aes(fill = .data$value),
      color = "white",
      linewidth = 0.08
    ) +
    scale_fill_viridis_c(
      option = "C",
      labels = legend_labels,
      na.value = "grey88",
      name = legend_title
    ) +
    coord_sf(datum = NA, expand = FALSE) +
    labs(
      title = title,
      subtitle = "Brandenburg, Mecklenburg-Vorpommern, Sachsen, Sachsen-Anhalt und Thueringen",
      caption = "Grau: kein Wert vorhanden; Geometriestand: 01.01.2025"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey35"),
      legend.position = "right"
    )

  ggsave(
    file.path(chart_dir, filename),
    plot,
    width = 9,
    height = 8,
    dpi = 300,
    bg = "white"
  )
}

# Fuer jede ausgewaehlte Strukturvariable eine Karte erzeugen.
for (variable in strukturvariablen) {
  values <- struktur %>%
    transmute(
      agg_schluessel,
      value = .data[[variable]]
    )

  variable_label <- if (variable %in% names(struktur_labels)) {
    unname(struktur_labels[[variable]])
  } else {
    variable
  }

  save_map(
    map_data = build_agg_map(values),
    title = variable_label,
    legend_title = variable_label,
    filename = paste0("struktur_", safe_filename(variable), ".png")
  )
}

# Fuer jeden ausgewaehlten Uebergang eine Karte erzeugen.
for (i in seq_len(nrow(uebergaenge))) {
  selection <- uebergaenge[i, ]

  values <- transitions %>%
    filter(
      .data$from == selection$from,
      .data$to == selection$to
    ) %>%
    transmute(
      agg_schluessel,
      value = .data[[selection$kennzahl]]
    )

  if (nrow(values) == 0) {
    stop(
      "Der ausgewaehlte Uebergang kommt nicht vor: ",
      selection$from,
      " -> ",
      selection$to
    )
  }

  is_probability <- selection$kennzahl == "transition_probability"
  metric_label <- if (is_probability) {
    "Uebergangswahrscheinlichkeit"
  } else {
    "Geschaetzte Uebergaenge"
  }

  save_map(
    map_data = build_agg_map(values),
    title = paste(selection$from, "zu", selection$to),
    legend_title = metric_label,
    filename = paste0(
      "uebergang_",
      safe_filename(selection$from),
      "_zu_",
      safe_filename(selection$to),
      "_",
      safe_filename(selection$kennzahl),
      ".png"
    ),
    probability = is_probability
  )
}

message("Karten gespeichert unter: ", chart_dir)
