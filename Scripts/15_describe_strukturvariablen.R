# Verteilungen, Korrelationen und bivariate Zusammenhaenge der Strukturvariablen untersuchen.

library(dplyr)
library(ggplot2)
library(tidyr)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")
source("Functions/regression_functions.R", encoding = "UTF-8")

ensure_data_dirs()

# Hier den zu untersuchenden lokalen nslphom-Uebergang festlegen.
ziel_herkunft <- "Union"
ziel_partei <- "AfD"
ziel_kennzahl <- "transition_probability"

struktur_path <- file.path(
  data_dir_cleaned,
  "inkar_kovariaten_2023.rds"
)
transition_path <- file.path(
  data_dir_model_nslphom_ost,
  "transition_matrices_long.rds"
)
model_data_path <- file.path(
  data_dir_model_regression_ost,
  "modell_afd_zufluss_daten.rds"
)
output_dir <- file.path(
  data_dir_model_regression_ost,
  "deskriptiv"
)
chart_dir <- file.path(
  "Charts",
  "Strukturvariablen",
  "deskriptiv"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)

required_files <- c(struktur_path, transition_path, model_data_path)
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop("Folgende Eingabedateien fehlen: ", paste(missing_files, collapse = ", "))
}

struktur <- readRDS(struktur_path) %>%
  filter(is_ostdeutschland_ohne_berlin(.data$agg_schluessel)) %>%
  arrange(.data$agg_schluessel)

strukturvariablen <- get_structure_covariates(struktur)

# Kurze, lesbare Bezeichnungen fuer Tabellen und Grafiken festlegen.
variablen_labels <- c(
  einwohnerdichte_2023 = "Einwohnerdichte",
  supermarktEntfernung_2023 = "Entfernung zum Supermarkt",
  alterMean_2023 = "Durchschnittsalter",
  wanderungssaldo_2023 = "Wanderungssaldo",
  pendler50_2023 = "Fernpendleranteil",
  steuereinnahmen_2023 = "Steuereinnahmen",
  haushaltsgroesseMean_2023 = "Haushaltsgroesse",
  anteilHaushalteNiedrigesEinkommen_2023 = "Einkommensschwache Haushalte",
  arbeitslosenanteilErwerbsfaehige_2023 = "Arbeitslosenanteil",
  distanz_staatsgrenze_km_2023 = "Distanz zur Staatsgrenze"
)

fehlende_labels <- setdiff(strukturvariablen, names(variablen_labels))

if (length(fehlende_labels) > 0) {
  variablen_labels[fehlende_labels] <- fehlende_labels
}

# Strukturwerte in ein Longformat fuer Kennzahlen und Grafiken bringen.
struktur_long <- struktur %>%
  select(.data$agg_schluessel, all_of(strukturvariablen)) %>%
  pivot_longer(
    cols = all_of(strukturvariablen),
    names_to = "variable",
    values_to = "wert"
  ) %>%
  mutate(
    variable_label = unname(variablen_labels[.data$variable]),
    variable_label = factor(
      .data$variable_label,
      levels = unname(variablen_labels[strukturvariablen])
    )
  )

# Lage, Streuung, Quantile und fehlende Werte je Strukturvariable berechnen.
struktur_summary <- struktur_long %>%
  group_by(.data$variable, .data$variable_label) %>%
  summarise(
    n = sum(!is.na(.data$wert)),
    n_missing = sum(is.na(.data$wert)),
    mittelwert = mean(.data$wert, na.rm = TRUE),
    standardabweichung = sd(.data$wert, na.rm = TRUE),
    minimum = min(.data$wert, na.rm = TRUE),
    q05 = quantile(.data$wert, 0.05, na.rm = TRUE),
    q25 = quantile(.data$wert, 0.25, na.rm = TRUE),
    median = median(.data$wert, na.rm = TRUE),
    q75 = quantile(.data$wert, 0.75, na.rm = TRUE),
    q95 = quantile(.data$wert, 0.95, na.rm = TRUE),
    maximum = max(.data$wert, na.rm = TRUE),
    .groups = "drop"
  )

# Pearson-Korrelationen der Strukturvariablen berechnen.
korrelation <- stats::cor(
  struktur[strukturvariablen],
  use = "pairwise.complete.obs",
  method = "pearson"
)

korrelation_long <- as.data.frame(as.table(korrelation), stringsAsFactors = FALSE) %>%
  transmute(
    variable_x = as.character(.data$Var1),
    variable_y = as.character(.data$Var2),
    variable_x_label = unname(variablen_labels[.data$variable_x]),
    variable_y_label = unname(variablen_labels[.data$variable_y]),
    korrelation = as.numeric(.data$Freq)
  )

# Den ausgewaehlten lokalen Uebergang aus dem Ost-nslphom-Ergebnis auswaehlen.
transitions <- readRDS(transition_path)

assert_final_nslphom_groups(
  unique(c(transitions$from, transitions$to)),
  "Die Uebergangsdaten der deskriptiven Analyse"
)

if (!ziel_kennzahl %in% names(transitions)) {
  stop("Die Zielkennzahl fehlt im nslphom-Output: ", ziel_kennzahl)
}

zielvariable <- transitions %>%
  filter(
    .data$from == .env$ziel_herkunft,
    .data$to == .env$ziel_partei,
    is_ostdeutschland_ohne_berlin(.data$agg_schluessel)
  ) %>%
  transmute(
    agg_schluessel,
    zielwert = .data[[ziel_kennzahl]],
    origin_count
  ) %>%
  arrange(.data$agg_schluessel)

if (nrow(zielvariable) == 0) {
  stop(
    "Der ausgewaehlte Uebergang wurde nicht gefunden: ",
    ziel_herkunft,
    " -> ",
    ziel_partei
  )
}

if (anyDuplicated(zielvariable$agg_schluessel) > 0) {
  stop("Der ausgewaehlte Uebergang ist nicht eindeutig je agg_schluessel.")
}

# Strukturwerte und nslphom-Zielwert ueber denselben agg_schluessel verbinden.
scatter_data <- struktur_long %>%
  left_join(
    zielvariable,
    by = "agg_schluessel"
  )

if (any(is.na(scatter_data$zielwert))) {
  stop("Nicht alle Strukturzeilen konnten mit dem nslphom-Zielwert verbunden werden.")
}

# Bivariate Pearson-Korrelationen zum ausgewaehlten Uebergang dokumentieren.
ziel_korrelationen <- scatter_data %>%
  group_by(.data$variable, .data$variable_label) %>%
  summarise(
    n = sum(complete.cases(.data$wert, .data$zielwert)),
    korrelation = cor(.data$wert, .data$zielwert, use = "complete.obs"),
    .groups = "drop"
  )

# Alle deskriptiven Tabellen gemeinsam als ein RDS-Analyseobjekt speichern.
deskriptiv_output <- list(
  settings = tibble(
    analysegebiet = "Ostdeutschland ohne Berlin",
    ziel_herkunft = ziel_herkunft,
    ziel_partei = ziel_partei,
    ziel_kennzahl = ziel_kennzahl,
    n_agg = nrow(struktur),
    strukturvariablen = paste(strukturvariablen, collapse = ", ")
  ),
  struktur_summary = struktur_summary,
  korrelation = korrelation,
  korrelation_long = korrelation_long,
  ziel_korrelationen = ziel_korrelationen
)

saveRDS(
  deskriptiv_output,
  file.path(output_dir, "strukturvariablen_deskriptiv.rds")
)

# Alle Verteilungen mit eigenen Achsenskalierungen in einer Grafik darstellen.
plot_verteilungen <- ggplot(struktur_long, aes(x = .data$wert)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30,
    fill = "#4472C4",
    color = "white"
  ) +
  geom_density(color = "#B22222", linewidth = 0.7, na.rm = TRUE) +
  facet_wrap(
    vars(.data$variable_label),
    scales = "free",
    ncol = 2
  ) +
  labs(
    title = "Verteilungen der Strukturvariablen",
    subtitle = "Ostdeutschland ohne Berlin",
    x = "Wert",
    y = "Dichte"
  ) +
  theme_minimal() +
  theme(
    panel.spacing = grid::unit(1, "lines"),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey95", color = NA)
  )

ggsave(
  file.path(chart_dir, "strukturvariablen_verteilungen.png"),
  plot_verteilungen,
  width = 12,
  height = 14,
  dpi = 300
)

# Die im Regressionsmodell verwendeten z-standardisierten Kovariaten direkt einlesen.
model_data <- readRDS(model_data_path)
strukturvariablen_z <- paste0(strukturvariablen, "_z")
fehlende_z_variablen <- setdiff(strukturvariablen_z, names(model_data))

if (length(fehlende_z_variablen) > 0) {
  stop(
    "Folgende z-standardisierte Kovariaten fehlen im Regressionsinput: ",
    paste(fehlende_z_variablen, collapse = ", ")
  )
}

# Jede Aggregationseinheit steht wegen der Herkunftsmodelle mehrfach in model_data.
# Fuer die Verteilungen wird sie genau einmal mit ihren Modellkovariaten verwendet.
struktur_z <- model_data %>%
  select(.data$agg_schluessel, all_of(strukturvariablen_z)) %>%
  distinct()

if (anyDuplicated(struktur_z$agg_schluessel) > 0) {
  stop("Eine Aggregationseinheit besitzt unterschiedliche z-standardisierte Kovariaten.")
}

struktur_z_long <- struktur_z %>%
  pivot_longer(
    cols = all_of(strukturvariablen_z),
    names_to = "variable_z",
    values_to = "wert_standardisiert"
  ) %>%
  mutate(
    variable = sub("_z$", "", .data$variable_z),
    variable_label = factor(
      unname(variablen_labels[.data$variable]),
      levels = unname(variablen_labels[strukturvariablen])
    )
  )

# Die Verteilungen der tatsaechlich modellierten z-Werte auf gemeinsamer x-Achse darstellen.
plot_verteilungen_z <- ggplot(
  struktur_z_long,
  aes(x = .data$wert_standardisiert)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30,
    fill = "#4472C4",
    color = "white"
  ) +
  geom_density(color = "#B22222", linewidth = 0.7, na.rm = TRUE) +
  geom_vline(xintercept = 0, color = "grey35", linewidth = 0.35) +
  facet_wrap(
    vars(.data$variable_label),
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    title = "Verteilungen der z-standardisierten Strukturvariablen",
    subtitle = "Im Regressionsmodell verwendete Oststichprobe ohne Berlin",
    x = "Z-standardisierter Wert",
    y = "Dichte"
  ) +
  theme_minimal() +
  theme(
    panel.spacing = grid::unit(1, "lines"),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey95", color = NA)
  )

ggsave(
  file.path(chart_dir, "strukturvariablen_verteilungen_z_standardisiert.png"),
  plot_verteilungen_z,
  width = 12,
  height = 14,
  dpi = 300
)

# Einen gemeinsamen standardisierten Boxplot zum Erkennen von Ausreissern erzeugen.
boxplot_data <- struktur_long %>%
  group_by(.data$variable) %>%
  mutate(wert_standardisiert = as.numeric(scale(.data$wert))) %>%
  ungroup()

plot_boxplots <- ggplot(
  boxplot_data,
  aes(x = .data$wert_standardisiert, y = .data$variable_label)
) +
  geom_boxplot(fill = "#9DC3E6", outlier.alpha = 0.35) +
  geom_vline(xintercept = 0, color = "grey45", linewidth = 0.3) +
  labs(
    title = "Standardisierte Strukturvariablen",
    subtitle = "Boxplots fuer Ostdeutschland ohne Berlin",
    x = "Standardisierter Wert",
    y = NULL
  ) +
  theme_minimal()

ggsave(
  file.path(chart_dir, "strukturvariablen_boxplots.png"),
  plot_boxplots,
  width = 10,
  height = 6.5,
  dpi = 300
)

# Korrelationsmatrix mit Richtung und Staerke der Zusammenhaenge darstellen.
plot_korrelation <- ggplot(
  korrelation_long,
  aes(
    x = .data$variable_x_label,
    y = .data$variable_y_label,
    fill = .data$korrelation
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", .data$korrelation)), size = 3) +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Pearson-r"
  ) +
  coord_equal() +
  labs(
    title = "Korrelation der Strukturvariablen",
    subtitle = "Ostdeutschland ohne Berlin",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

ggsave(
  file.path(chart_dir, "strukturvariablen_korrelation.png"),
  plot_korrelation,
  width = 10,
  height = 9,
  dpi = 300
)

# Alle bivariaten Zusammenhaenge mit eigenen x-Achsen in einer Grafik darstellen.
plot_scatter <- ggplot(
  scatter_data,
  aes(x = .data$wert, y = .data$zielwert)
) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(
    aes(weight = .data$origin_count),
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "#B22222"
  ) +
  facet_wrap(
    vars(.data$variable_label),
    scales = "free_x",
    ncol = 2
  ) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(
    title = paste(
      "Lokaler Uebergang",
      label_party_group(ziel_herkunft),
      "->",
      label_party_group(ziel_partei)
    ),
    subtitle = "Punkte ungewichtet, Regressionslinien nach Herkunftsstaerken gewichtet",
    x = "Strukturvariable",
    y = "Geschaetzte Uebergangswahrscheinlichkeit"
  ) +
  theme_minimal() +
  theme(
    panel.spacing = grid::unit(1, "lines"),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey95", color = NA)
  )

ggsave(
  file.path(
    chart_dir,
    paste0("scatter_", ziel_herkunft, "_to_", ziel_partei, ".png")
  ),
  plot_scatter,
  width = 12,
  height = 14,
  dpi = 300
)

message("Deskriptive Strukturvariablenanalyse gespeichert unter: ", chart_dir)
