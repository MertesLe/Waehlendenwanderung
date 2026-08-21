# Pruefen, wie genau die lokalen Uebergangsmatrizen die beobachteten Zielstimmen rekonstruieren.

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")

ensure_data_dirs()

nslphom_output_path <- getOption(
  "waehlendenwanderung.local_residual_nslphom_output_path",
  file.path(
    data_dir_model_nslphom_ost,
    "vorlaeufig_nslphom_ost_endoutput.rds"
  )
)

run_label <- getOption(
  "waehlendenwanderung.local_residual_run_label",
  "ostdeutschland_ohne_berlin"
)
run_label <- gsub("[^A-Za-z0-9_]+", "_", run_label)

geometry_path <- getOption(
  "waehlendenwanderung.local_residual_geometry_path",
  file.path(
    "Data",
    "raw",
    "gebiete_visualisierung",
    "vg250_01-01.utm32s.gpkg.ebenen",
    "vg250_ebenen_0101",
    "DE_VG250.gpkg"
  )
)

geometry_layer <- getOption("waehlendenwanderung.local_residual_geometry_layer", "vg250_gem")
residual_tolerance_count <- getOption("waehlendenwanderung.local_residual_tolerance_count", 1e-8)
chart_dir <- file.path("Charts", "homogenitaetsannahme")
output_dir <- file.path(data_dir_validation, "homogenitaetsannahme")

dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

map_file <- file.path(
  chart_dir,
  paste0("vorlaeufig_", run_label, "_lokale_matrix_residuen_deutschlandkarte_agg.png")
)

hist_file <- file.path(
  chart_dir,
  paste0("vorlaeufig_", run_label, "_lokale_matrix_residuen_histogramm.png")
)

metrics_file <- file.path(
  output_dir,
  paste0("vorlaeufig_", run_label, "_lokale_matrix_residuen.rds")
)

summary_file <- file.path(
  output_dir,
  paste0("vorlaeufig_", run_label, "_lokale_matrix_residuen_summary.rds")
)

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

if (!"local_matrices_long" %in% names(nslphom_output)) {
  stop("Der nslphom-Endoutput muss local_matrices_long enthalten.")
}

local_transitions <- nslphom_output$local_matrices_long

required_cols <- c(
  "agg_schluessel",
  "from",
  "to",
  "origin_count",
  "destination_count",
  "estimated_transition_count"
)

missing_cols <- setdiff(required_cols, names(local_transitions))

if (length(missing_cols) > 0) {
  stop("local_matrices_long enthaelt nicht alle benoetigten Spalten: ", paste(missing_cols, collapse = ", "))
}

wahlberechtigte_by_unit <- local_transitions %>%
  distinct(agg_schluessel, from, origin_count) %>%
  group_by(agg_schluessel) %>%
  summarise(
    wahlberechtigte = sum(origin_count, na.rm = TRUE),
    .groups = "drop"
  )

# Lokaler Matrix-Residual: Beobachtete Zielstimmen werden mit den Zielstimmen
# verglichen, die sich aus der lokal geschaetzten Uebergangsmatrix ergeben.
# Bei einem gut randtreuen nslphom-Fit sollten diese Abweichungen nahe null sein.
local_residual_cells <- local_transitions %>%
  group_by(agg_schluessel, to) %>%
  summarise(
    observed_destination_count = first(destination_count),
    expected_destination_count_local = sum(estimated_transition_count, na.rm = TRUE),
    local_residual_count_raw = observed_destination_count - expected_destination_count_local,
    .groups = "drop"
  ) %>%
  mutate(
    # Numerische Rundungsfehler unterhalb der Toleranz sind keine inhaltlichen
    # Abweichungen der lokalen Matrix und werden fuer die Kennzahl auf null gesetzt.
    local_residual_count = if_else(
      abs(local_residual_count_raw) < residual_tolerance_count,
      0,
      local_residual_count_raw
    )
  )

local_residual_metrics <- local_residual_cells %>%
  group_by(agg_schluessel) %>%
  summarise(
    local_residual_abs_sum = sum(abs(local_residual_count), na.rm = TRUE),
    local_residual_abs_half = 0.5 * local_residual_abs_sum,
    max_abs_target_residual = max(abs(local_residual_count), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    wahlberechtigte_by_unit,
    by = "agg_schluessel"
  ) %>%
  mutate(
    local_residual_index = local_residual_abs_half / wahlberechtigte,
    local_residual_index_percent = 100 * local_residual_index,
    n_gemeindeschluessel = lengths(lapply(agg_schluessel, split_keys))
  ) %>%
  arrange(desc(local_residual_index))

if (any(is.na(local_residual_metrics$wahlberechtigte))) {
  stop("Mindestens eine Einheit konnte keiner Wahlberechtigtenzahl zugeordnet werden.")
}

local_residual_summary <- local_residual_metrics %>%
  summarise(
    n_agg = n(),
    mean_local_residual_index = mean(local_residual_index, na.rm = TRUE),
    median_local_residual_index = median(local_residual_index, na.rm = TRUE),
    p95_local_residual_index = quantile(local_residual_index, probs = 0.95, na.rm = TRUE),
    max_local_residual_index = max(local_residual_index, na.rm = TRUE),
    max_abs_target_residual_count = max(max_abs_target_residual, na.rm = TRUE),
    max_abs_target_residual_count_raw = max(abs(local_residual_cells$local_residual_count_raw), na.rm = TRUE),
    residual_tolerance_count = residual_tolerance_count
  )

saveRDS(local_residual_metrics, metrics_file)
saveRDS(local_residual_summary, summary_file)

hist_plot <- ggplot(local_residual_metrics, aes(x = local_residual_index)) +
  geom_histogram(bins = 60, fill = "grey55", color = "white") +
  scale_x_continuous(labels = percent_format(accuracy = 0.000001)) +
  labs(
    title = "Lokaler Matrix-Residual",
    subtitle = "Abweichung zwischen beobachteten Zielstimmen und lokal geschaetzter Matrix",
    x = "0.5 * Summe absoluter Zielabweichungen / Wahlberechtigte",
    y = "Anzahl Aggregationseinheiten"
  ) +
  theme_minimal()

ggsave(
  hist_file,
  hist_plot,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

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

residual_ags_long <- local_residual_metrics %>%
  select(
    agg_schluessel,
    local_residual_abs_half,
    local_residual_index,
    local_residual_index_percent,
    max_abs_target_residual,
    wahlberechtigte
  ) %>%
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

geometry_status <- residual_ags_long %>%
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
      n_ersatz_geometrien > 0 ~ "ersatz_geometrie_aggregiert",
      TRUE ~ "keine_geometrie"
    )
  )

direct_lookup <- residual_ags_long %>%
  filter(hat_direkte_geometrie) %>%
  select(agg_schluessel, gemeindeschluessel) %>%
  distinct()

direct_map <- gemeinde_geometrien %>%
  inner_join(
    direct_lookup,
    by = "gemeindeschluessel"
  ) %>%
  left_join(
    local_residual_metrics,
    by = "agg_schluessel"
  ) %>%
  left_join(
    geometry_status,
    by = "agg_schluessel"
  )

missing_substitute_lookup <- residual_ags_long %>%
  group_by(agg_schluessel) %>%
  filter(!any(hat_direkte_geometrie)) %>%
  ungroup() %>%
  filter(!is.na(ersatz_geometrie)) %>%
  distinct(ersatz_geometrie, agg_schluessel) %>%
  left_join(
    local_residual_metrics,
    by = "agg_schluessel"
  ) %>%
  group_by(ersatz_geometrie) %>%
  summarise(
    agg_schluessel = paste(sort(unique(agg_schluessel)), collapse = ", "),
    local_residual_abs_half = sum(local_residual_abs_half, na.rm = TRUE),
    wahlberechtigte = sum(wahlberechtigte, na.rm = TRUE),
    local_residual_index = local_residual_abs_half / wahlberechtigte,
    max_abs_target_residual = max(max_abs_target_residual, na.rm = TRUE),
    geometrie_status = "ersatz_geometrie_aggregiert",
    .groups = "drop"
  )

# Berlin/Hamburg-Aggregation: Die Wahldaten liegen kleinteiliger vor als VG250.
# Deshalb werden alle lokalen Residuen je Gesamtstadt ueber
# sum(local_residual_abs_half) / sum(Wahlberechtigte) aggregiert.
substitute_map <- gemeinde_geometrien %>%
  inner_join(
    missing_substitute_lookup,
    by = c("gemeindeschluessel" = "ersatz_geometrie")
  ) %>%
  group_by(gemeindeschluessel) %>%
  summarise(
    agg_schluessel = first(agg_schluessel),
    local_residual_abs_half = first(local_residual_abs_half),
    local_residual_index = first(local_residual_index),
    max_abs_target_residual = first(max_abs_target_residual),
    wahlberechtigte = first(wahlberechtigte),
    geometrie_status = first(geometrie_status),
    .groups = "drop"
  )

residual_map_data <- bind_rows(
  direct_map,
  substitute_map
)

scale_limit <- max(residual_map_data$local_residual_index, na.rm = TRUE)

if (!is.finite(scale_limit) || scale_limit <= 0) {
  scale_limit <- 1e-12
}

map_plot <- ggplot() +
  geom_sf(
    data = gemeinde_geometrien,
    fill = "grey88",
    color = NA
  ) +
  geom_sf(
    data = residual_map_data %>% filter(is.na(local_residual_index)),
    fill = "grey82",
    color = NA
  ) +
  geom_sf(
    data = residual_map_data %>% filter(!is.na(local_residual_index)),
    aes(fill = local_residual_index),
    color = NA
  ) +
  scale_fill_gradientn(
    colors = c("#f7fbff", "#9ecae1", "#3182bd", "#08519c"),
    limits = c(0, scale_limit),
    oob = squish,
    labels = percent_format(accuracy = 0.000001),
    name = "Residual relativ"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Residual der lokalen Uebergangsmatrizen",
    subtitle = "Vergleich beobachteter Zielstimmen mit den durch lokale Matrizen reproduzierten Zielstimmen",
    caption = "Index = 0.5 * Summe absoluter lokaler Zielabweichungen / Wahlberechtigte. Berlin/Hamburg sind als Gesamtstadt aggregiert."
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey35"),
    plot.caption = element_text(color = "grey45", hjust = 0)
  )

ggsave(
  map_file,
  map_plot,
  width = 9,
  height = 11,
  dpi = 300,
  bg = "white"
)

message("Lokale Matrix-Residuen gespeichert unter: ", metrics_file)
message("Lokale Matrix-Residual-Karte gespeichert unter: ", map_file)
