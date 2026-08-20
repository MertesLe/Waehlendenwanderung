# Abweichungen der AfD-Zielspalte aus EHet auf einer Deutschlandkarte visualisieren.

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")

ensure_data_dirs()

nslphom_output_path <- getOption(
  "waehlendenwanderung.afd_ehet_nslphom_output_path",
  file.path(
    data_dir_model_nslphom,
    "testSymphony5965_random_dual",
    "vorlaeufig_test5965_random_nslphom_dual_endoutput.rds"
  )
)

geometry_path <- getOption(
  "waehlendenwanderung.afd_ehet_geometry_path",
  file.path(
    "Data",
    "raw",
    "gebiete_visualisierung",
    "vg250_01-01.utm32s.gpkg.ebenen",
    "vg250_ebenen_0101",
    "DE_VG250.gpkg"
  )
)

geometry_layer <- getOption("waehlendenwanderung.afd_ehet_geometry_layer", "vg250_gem")
chart_dir <- file.path("Charts", "Homogenitaetsannahmentest")
chart_file <- file.path(
  chart_dir,
  "vorlaeufig_testSymphony5965_random_dual_afd_ehet_deutschlandkarte_agg.png"
)

dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(nslphom_output_path)) {
  stop("Der nslphom-Endoutput wurde nicht gefunden: ", nslphom_output_path)
}

if (!file.exists(geometry_path)) {
  stop("Die Gemeindeflaechen-Datei wurde nicht gefunden: ", geometry_path)
}

if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Das Paket 'sf' muss installiert sein, um die Karte zu erstellen.")
}

nslphom_output <- readRDS(nslphom_output_path)

if (!all(c("local_matrices_long", "EHet", "EHet_ids") %in% names(nslphom_output))) {
  stop("Der nslphom-Endoutput muss local_matrices_long, EHet und EHet_ids enthalten.")
}

ehet_matrix <- as.matrix(nslphom_output$EHet)
ehet_ids <- as.character(nslphom_output$EHet_ids)

if (length(ehet_ids) != nrow(ehet_matrix) || any(is.na(ehet_ids))) {
  stop("EHet_ids passen nicht plausibel zu den Zeilen von EHet.")
}

afd_col <- which(tolower(colnames(ehet_matrix)) == "afd")

if (length(afd_col) != 1) {
  stop("In EHet wurde keine eindeutig benannte AfD-Spalte gefunden.")
}

local_transitions <- nslphom_output$local_matrices_long

required_cols <- c("agg_schluessel", "from", "origin_count")
missing_cols <- setdiff(required_cols, names(local_transitions))

if (length(missing_cols) > 0) {
  stop("local_matrices_long enthaelt nicht alle benoetigten Spalten: ", paste(missing_cols, collapse = ", "))
}

local_ids <- unique(local_transitions$agg_schluessel)

if (!setequal(ehet_ids, local_ids)) {
  stop("Die EHet-Zeilen passen nicht zu den lokalen nslphom-Einheiten.")
}

# EHet ist hier eine Matrix aus Einheiten x Zielgruppen. Fuer die AfD-Karte
# wird nur die AfD-Zielspalte relativ zur Wahlberechtigtenzahl betrachtet.
wahlberechtigte_by_unit <- local_transitions %>%
  distinct(agg_schluessel, from, origin_count) %>%
  group_by(agg_schluessel) %>%
  summarise(
    wahlberechtigte = sum(origin_count, na.rm = TRUE),
    .groups = "drop"
  )

afd_ehet_metrics <- tibble::tibble(
  agg_schluessel = ehet_ids,
  afd_ehet_count = as.numeric(ehet_matrix[, afd_col])
) %>%
  left_join(
    wahlberechtigte_by_unit,
    by = "agg_schluessel"
  ) %>%
  mutate(
    afd_ehet_rel = afd_ehet_count / wahlberechtigte,
    afd_ehet_abs_rel = abs(afd_ehet_rel)
  )

if (any(is.na(afd_ehet_metrics$wahlberechtigte))) {
  stop("Mindestens eine AfD-EHet-Einheit konnte keiner Wahlberechtigtenzahl zugeordnet werden.")
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

if (is.na(ags_col)) {
  stop("In der Geometriedatei wurde keine AGS-Spalte gefunden.")
}

gemeinde_geometrien <- gemeinde_geometrien %>%
  mutate(
    gemeindeschluessel = as.character(.data[[ags_col]])
  )

geometrie_keys <- unique(gemeinde_geometrien$gemeindeschluessel)

afd_ags_long <- afd_ehet_metrics %>%
  select(agg_schluessel, afd_ehet_count, afd_ehet_rel, afd_ehet_abs_rel, wahlberechtigte) %>%
  mutate(
    gemeindeschluessel = lapply(agg_schluessel, split_keys)
  ) %>%
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

geometry_status <- afd_ags_long %>%
  group_by(agg_schluessel) %>%
  summarise(
    n_gemeindeschluessel = n(),
    n_direkte_geometrien = sum(hat_direkte_geometrie),
    n_fehlende_gemeindegeometrien = sum(!hat_direkte_geometrie),
    n_ersatz_geometrien = sum(!hat_direkte_geometrie & !is.na(ersatz_geometrie)),
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

direct_lookup <- afd_ags_long %>%
  filter(hat_direkte_geometrie) %>%
  select(agg_schluessel, gemeindeschluessel) %>%
  distinct()

direct_map <- gemeinde_geometrien %>%
  inner_join(
    direct_lookup,
    by = "gemeindeschluessel"
  ) %>%
  left_join(
    afd_ehet_metrics,
    by = "agg_schluessel"
  ) %>%
  left_join(
    geometry_status,
    by = "agg_schluessel"
  ) %>%
  group_by(agg_schluessel) %>%
  summarise(
    afd_ehet_count = first(afd_ehet_count),
    afd_ehet_rel = first(afd_ehet_rel),
    afd_ehet_abs_rel = first(afd_ehet_abs_rel),
    wahlberechtigte = first(wahlberechtigte),
    n_gemeindeschluessel = first(n_gemeindeschluessel),
    n_fehlende_gemeindegeometrien = first(n_fehlende_gemeindegeometrien),
    geometrie_status = first(geometrie_status),
    .groups = "drop"
  )

missing_substitute_lookup <- afd_ags_long %>%
  group_by(agg_schluessel) %>%
  filter(!any(hat_direkte_geometrie)) %>%
  ungroup() %>%
  filter(!is.na(ersatz_geometrie)) %>%
  distinct(ersatz_geometrie, agg_schluessel) %>%
  left_join(
    afd_ehet_metrics,
    by = "agg_schluessel"
  ) %>%
  group_by(ersatz_geometrie) %>%
  summarise(
    agg_schluessel = paste(sort(unique(agg_schluessel)), collapse = ", "),
    afd_ehet_count = sum(afd_ehet_count, na.rm = TRUE),
    wahlberechtigte = sum(wahlberechtigte, na.rm = TRUE),
    afd_ehet_rel = afd_ehet_count / wahlberechtigte,
    afd_ehet_abs_rel = abs(afd_ehet_rel),
    geometrie_status = "ersatz_geometrie_aggregiert",
    .groups = "drop"
  )

# Berlin/Hamburg-Aggregation: Die Wahldaten liegen kleinteiliger vor als VG250.
# Deshalb werden alle lokalen Einheiten je Gesamtstadt ueber
# sum(EHet_AfD) / sum(Wahlberechtigte) zu einem Gesamtstadtwert aggregiert.
substitute_map <- gemeinde_geometrien %>%
  inner_join(
    missing_substitute_lookup,
    by = c("gemeindeschluessel" = "ersatz_geometrie")
  ) %>%
  group_by(gemeindeschluessel) %>%
  summarise(
    agg_schluessel = first(agg_schluessel),
    afd_ehet_count = first(afd_ehet_count),
    afd_ehet_rel = first(afd_ehet_rel),
    afd_ehet_abs_rel = first(afd_ehet_abs_rel),
    wahlberechtigte = first(wahlberechtigte),
    geometrie_status = first(geometrie_status),
    .groups = "drop"
  )

afd_map_data <- bind_rows(
  direct_map,
  substitute_map
)

# Die Farbskala wird am 99%-Quantil der absoluten AfD-Abweichung gekappt.
# Dadurch bleiben regionale Muster sichtbar, ohne die Extremwerte zu verlieren.
scale_limit <- as.numeric(
  stats::quantile(
    abs(afd_map_data$afd_ehet_rel),
    probs = 0.99,
    na.rm = TRUE
  )
)

if (!is.finite(scale_limit) || scale_limit <= 0) {
  scale_limit <- max(abs(afd_map_data$afd_ehet_rel), na.rm = TRUE)
}

map_plot <- ggplot() +
  geom_sf(
    data = gemeinde_geometrien,
    fill = "grey88",
    color = NA
  ) +
  geom_sf(
    data = afd_map_data %>% filter(is.na(afd_ehet_rel)),
    fill = "grey82",
    color = NA
  ) +
  geom_sf(
    data = afd_map_data %>% filter(!is.na(afd_ehet_rel)),
    aes(fill = afd_ehet_rel),
    color = NA
  ) +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "#f7f7f7",
    high = "#b2182b",
    midpoint = 0,
    limits = c(-scale_limit, scale_limit),
    oob = squish,
    labels = percent_format(accuracy = 1),
    name = "AfD-Abweichung"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "AfD-Abweichung von der homogenen globalen Matrix",
    subtitle = "Rot: mehr AfD-Zielstimmen als homogen erwartet; Blau: weniger. Berlin/Hamburg sind als Gesamtstadt aggregiert.",
    caption = "AfD-Abweichung = EHet_AfD / Wahlberechtigte. Farbskala am 99%-Quantil der absoluten Abweichung gekappt."
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey35"),
    plot.caption = element_text(color = "grey45", hjust = 0)
  )

ggsave(
  chart_file,
  map_plot,
  width = 9,
  height = 11,
  dpi = 300,
  bg = "white"
)

message("AfD-EHet-Karte gespeichert unter: ", chart_file)
