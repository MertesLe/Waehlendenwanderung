# Annahmen der gewichteten linearen AfD-Zuflussmodelle pruefen und visualisieren.

library(dplyr)
library(ggplot2)
library(tidyr)

source("paths.R", encoding = "UTF-8")
source("Functions/regression_functions.R", encoding = "UTF-8")

ensure_data_dirs()

model_data_path <- file.path(
  data_dir_model_regression_ost,
  "modell_afd_zufluss_daten.rds"
)
output_dir <- file.path(
  data_dir_model_regression_ost,
  "regressionsdiagnostik"
)
chart_dir <- file.path(
  "Charts",
  "Regression",
  "ostdeutschland",
  "regressionsdiagnostik"
)
linearity_chart_dir <- file.path(chart_dir, "linearitaet")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(linearity_chart_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(model_data_path)) {
  stop(
    "Der Regressionsinput fehlt. Fuehre zuerst Scripts/04_model_transitions.R aus: ",
    model_data_path
  )
}

model_data <- readRDS(model_data_path)
strukturvariablen <- get_structure_covariates(model_data)
strukturvariablen_z <- paste0(strukturvariablen, "_z")

required_cols <- c(
  "agg_schluessel",
  "from",
  "to",
  "transition_probability",
  "origin_count",
  strukturvariablen_z
)
missing_cols <- setdiff(required_cols, names(model_data))

if (length(missing_cols) > 0) {
  stop("Im Regressionsinput fehlen Spalten: ", paste(missing_cols, collapse = ", "))
}

if (anyDuplicated(model_data[c("agg_schluessel", "from", "to")]) > 0) {
  stop("Der Regressionsinput ist nicht eindeutig je agg_schluessel und Uebergang.")
}

# Kurze Variablennamen fuer die Linearitaetsgrafiken festlegen.
variablen_labels <- c(
  einwohnerdichte_2023_z = "Einwohnerdichte",
  supermarktEntfernung_2023_z = "Entfernung zum Supermarkt",
  alterMean_2023_z = "Durchschnittsalter",
  wanderungssaldo_2023_z = "Wanderungssaldo",
  pendler50_2023_z = "Fernpendleranteil",
  steuereinnahmen_2023_z = "Steuereinnahmen",
  haushaltsgroesseMean_2023_z = "Haushaltsgroesse",
  anteilHaushalteNiedrigesEinkommen_2023_z = "Einkommensschwache Haushalte",
  arbeitslosenanteilErwerbsfaehige_2023_z = "Arbeitslosenanteil",
  distanz_staatsgrenze_km_2023_z = "Distanz zur Staatsgrenze"
)
missing_labels <- setdiff(strukturvariablen_z, names(variablen_labels))

if (length(missing_labels) > 0) {
  variablen_labels[missing_labels] <- sub("_z$", "", missing_labels)
}

# Fuer jede Herkunftsgruppe dasselbe gewichtete lineare Modell wie im Hauptskript schaetzen.
origin_groups <- sort(unique(model_data$from))
model_fits <- stats::setNames(vector("list", length(origin_groups)), origin_groups)

for (origin in origin_groups) {
  origin_data <- model_data %>%
    filter(.data$from == .env$origin) %>%
    arrange(.data$agg_schluessel)

  model_fits[[origin]] <- lm(
    reformulate(
      strukturvariablen_z,
      response = "transition_probability"
    ),
    data = origin_data,
    weights = origin_data$origin_count,
    model = TRUE,
    x = TRUE,
    y = TRUE
  )
}

# Residuen, Hebelwerte und Cook-Distanzen jeder Aggregationseinheit zusammenfassen.
diagnostic_rows <- bind_rows(lapply(origin_groups, function(origin) {
  fit <- model_fits[[origin]]
  origin_data <- model_data %>%
    filter(.data$from == .env$origin) %>%
    arrange(.data$agg_schluessel)

  tibble(
    agg_schluessel = origin_data$agg_schluessel,
    from = origin,
    observed = origin_data$transition_probability,
    fitted = fitted(fit),
    residual = residuals(fit),
    standardized_residual = rstandard(fit),
    studentized_residual = rstudent(fit),
    leverage = hatvalues(fit),
    cooks_distance = cooks.distance(fit),
    origin_count = origin_data$origin_count
  )
}))

# Breusch-Pagan-Test auf verbleibende Heteroskedastizitaet nach Beruecksichtigung der Gewichte.
# Dieser Test betrifft die Regressionsannahme der Homoskedastizitaet. Die nslphom-
# Homogenitaetsannahme wird getrennt ueber EHet in Scripts/09 und 10 untersucht.
breusch_pagan <- bind_rows(lapply(origin_groups, function(origin) {
  fit <- model_fits[[origin]]
  auxiliary_data <- as.data.frame(fit$model[strukturvariablen_z])
  normalized_weights <- fit$weights / mean(fit$weights)
  auxiliary_data$weighted_residual_sq <- (
    sqrt(normalized_weights) * residuals(fit)
  )^2
  auxiliary_fit <- lm(
    reformulate(strukturvariablen_z, response = "weighted_residual_sq"),
    data = auxiliary_data
  )
  statistic <- nrow(auxiliary_data) * summary(auxiliary_fit)$r.squared
  degrees_freedom <- length(strukturvariablen_z)

  tibble(
    from = origin,
    test = "Breusch-Pagan",
    statistic = statistic,
    df = degrees_freedom,
    p_value = pchisq(statistic, df = degrees_freedom, lower.tail = FALSE),
    auffaellig_p_0_05 = p_value < 0.05
  )
}))

# Ramsey-RESET-Test als globalen Hinweis auf Nichtlinearitaet oder fehlende Modellterme berechnen.
reset_tests <- bind_rows(lapply(origin_groups, function(origin) {
  fit <- model_fits[[origin]]
  reset_data <- fit$model
  reset_data$fitted_sq <- fitted(fit)^2
  reset_data$fitted_cube <- fitted(fit)^3
  reset_fit <- lm(
    reformulate(
      c(strukturvariablen_z, "fitted_sq", "fitted_cube"),
      response = "transition_probability"
    ),
    data = reset_data,
    weights = reset_data$`(weights)`
  )
  comparison <- anova(fit, reset_fit)

  tibble(
    from = origin,
    test = "Ramsey RESET",
    statistic = comparison$F[[2]],
    df_numerator = comparison$Df[[2]],
    df_denominator = df.residual(reset_fit),
    p_value = comparison$`Pr(>F)`[[2]],
    auffaellig_p_0_05 = p_value < 0.05
  )
}))

# Varianzinflationsfaktoren berechnen, um Multikollinearitaet zu erkennen.
vif_results <- bind_rows(lapply(origin_groups, function(origin) {
  fit <- model_fits[[origin]]
  predictor_data <- as.data.frame(fit$model[strukturvariablen_z])

  bind_rows(lapply(strukturvariablen_z, function(variable) {
    other_variables <- setdiff(strukturvariablen_z, variable)
    auxiliary_fit <- lm(
      reformulate(other_variables, response = variable),
      data = predictor_data
    )
    vif <- 1 / (1 - summary(auxiliary_fit)$r.squared)

    tibble(
      from = origin,
      variable = variable,
      variable_label = unname(variablen_labels[variable]),
      vif = vif,
      auffaellig_vif_5 = vif >= 5,
      stark_auffaellig_vif_10 = vif >= 10
    )
  }))
}))

# Residuenverteilung und zentrale Modellkennzahlen je Herkunftsgruppe dokumentieren.
model_summary <- bind_rows(lapply(origin_groups, function(origin) {
  fit <- model_fits[[origin]]
  rows <- diagnostic_rows %>% filter(.data$from == .env$origin)
  shapiro <- shapiro.test(rows$standardized_residual)
  n <- nrow(rows)
  n_parameters <- length(coef(fit))
  standardized <- rows$standardized_residual

  tibble(
    from = origin,
    n = n,
    n_parameters = n_parameters,
    r_squared = summary(fit)$r.squared,
    adjusted_r_squared = summary(fit)$adj.r.squared,
    residual_skewness = mean(standardized^3),
    residual_excess_kurtosis = mean(standardized^4) - 3,
    shapiro_w = unname(shapiro$statistic),
    shapiro_p_value = shapiro$p.value,
    n_predictions_below_0 = sum(rows$fitted < 0),
    n_predictions_above_1 = sum(rows$fitted > 1),
    n_abs_studentized_residual_gt_3 = sum(abs(rows$studentized_residual) > 3),
    n_high_leverage = sum(rows$leverage > 2 * n_parameters / n),
    n_high_cooks_distance = sum(rows$cooks_distance > 4 / n)
  )
}))

# Die jeweils einflussreichsten Einheiten fuer eine gezielte fachliche Kontrolle ausgeben.
influential_units <- diagnostic_rows %>%
  group_by(.data$from) %>%
  arrange(desc(.data$cooks_distance), .by_group = TRUE) %>%
  slice_head(n = 20) %>%
  ungroup()

# Partielle Residuen zeigen den bedingten Zusammenhang jeder Kovariate bei konstanten anderen Variablen.
# Die bivariaten Scatterplots in Skript 15 bleiben eine Vorpruefung, reichen hierfuer aber nicht aus.
partial_residuals <- bind_rows(lapply(origin_groups, function(origin) {
  fit <- model_fits[[origin]]
  origin_data <- model_data %>%
    filter(.data$from == .env$origin) %>%
    arrange(.data$agg_schluessel)

  bind_rows(lapply(strukturvariablen_z, function(variable) {
    variable_beta <- coef(fit)[[variable]]
    variable_label <- unname(variablen_labels[variable])
    predictor_values <- origin_data[[variable]]

    tibble(
      agg_schluessel = origin_data$agg_schluessel,
      from = origin,
      variable = variable,
      variable_label = variable_label,
      predictor = predictor_values,
      partial_residual = residuals(fit) + variable_beta * predictor_values,
      origin_count = origin_data$origin_count
    )
  }))
})) %>%
  mutate(
    variable_label = factor(
      .data$variable_label,
      levels = unname(variablen_labels[strukturvariablen_z])
    )
  )

# Alle Diagnosetabellen und die rekonstruierten Modelle gemeinsam speichern.
diagnostic_output <- list(
  settings = tibble(
    analysegebiet = "Ostdeutschland ohne Berlin",
    response = "transition_probability",
    weights = "origin_count",
    n_models = length(model_fits),
    strukturvariablen = paste(strukturvariablen, collapse = ", "),
    hinweis_unabhaengigkeit = paste(
      "Raeumliche Unabhaengigkeit wird hier nicht formal getestet.",
      "Dafuer ist ein eigener Moran-I-Test mit begruendeter Nachbarschaftsmatrix erforderlich."
    )
  ),
  model_summary = model_summary,
  breusch_pagan = breusch_pagan,
  reset_tests = reset_tests,
  vif = vif_results,
  influential_units = influential_units,
  diagnostic_rows = diagnostic_rows,
  partial_residuals = partial_residuals,
  model_fits = model_fits
)

saveRDS(
  diagnostic_output,
  file.path(output_dir, "regressionsannahmen_afd_zufluss.rds")
)

# Residuen gegen vorhergesagte Werte zur Beurteilung von Form und Streuung darstellen.
plot_residual_fitted <- ggplot(
  diagnostic_rows,
  aes(x = .data$fitted, y = .data$standardized_residual)
) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#B22222") +
  facet_wrap(vars(.data$from), scales = "free_x") +
  labs(
    title = "Standardisierte Residuen gegen Vorhersagen",
    subtitle = "Gewichtete lineare Modelle der AfD-Zufluesse",
    x = "Vorhergesagte Uebergangswahrscheinlichkeit",
    y = "Standardisiertes Residuum"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

ggsave(
  file.path(chart_dir, "residuen_gegen_vorhersage.png"),
  plot_residual_fitted,
  width = 12,
  height = 7,
  dpi = 300
)

# Q-Q-Plots zur visuellen Beurteilung der Residuen-Normalitaet erzeugen.
plot_qq <- ggplot(diagnostic_rows, aes(sample = .data$standardized_residual)) +
  stat_qq(alpha = 0.35, size = 0.9) +
  stat_qq_line(color = "#B22222", linewidth = 0.7) +
  facet_wrap(vars(.data$from), scales = "free") +
  labs(
    title = "Q-Q-Plots der standardisierten Residuen",
    subtitle = "Abweichungen an den Raendern sind bei grossen Stichproben besonders sichtbar",
    x = "Theoretische Quantile",
    y = "Beobachtete Quantile"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

ggsave(
  file.path(chart_dir, "qq_plots.png"),
  plot_qq,
  width = 12,
  height = 7,
  dpi = 300
)

# Scale-Location-Plot zur visuellen Kontrolle konstanter bedingter Residuenstreuung erzeugen.
plot_scale_location <- diagnostic_rows %>%
  mutate(sqrt_abs_standardized_residual = sqrt(abs(.data$standardized_residual))) %>%
  ggplot(aes(x = .data$fitted, y = .data$sqrt_abs_standardized_residual)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#B22222") +
  facet_wrap(vars(.data$from), scales = "free_x") +
  labs(
    title = "Scale-Location-Plot",
    subtitle = "Eine etwa horizontale rote Linie spricht fuer konstante Residuenstreuung",
    x = "Vorhergesagte Uebergangswahrscheinlichkeit",
    y = "Wurzel des absoluten standardisierten Residuums"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

ggsave(
  file.path(chart_dir, "scale_location.png"),
  plot_scale_location,
  width = 12,
  height = 7,
  dpi = 300
)

# Einflussreiche Beobachtungen ueber Leverage, studentisierte Residuen und Cook-Distanz zeigen.
plot_influence <- ggplot(
  diagnostic_rows,
  aes(
    x = .data$leverage,
    y = .data$studentized_residual,
    size = .data$cooks_distance
  )
) +
  geom_point(alpha = 0.35, color = "#4472C4") +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "#B22222") +
  facet_wrap(vars(.data$from), scales = "free_x") +
  scale_size_continuous(range = c(0.5, 5)) +
  labs(
    title = "Einflussdiagnostik",
    subtitle = "Punktgroesse entspricht der Cook-Distanz",
    x = "Leverage",
    y = "Studentisiertes Residuum",
    size = "Cook-Distanz"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

ggsave(
  file.path(chart_dir, "einflussdiagnostik.png"),
  plot_influence,
  width = 12,
  height = 7,
  dpi = 300
)

# Fuer jede Herkunftsgruppe partielle Residuenplots aller Kovariaten als ein Bild speichern.
for (origin in origin_groups) {
  origin_partial <- partial_residuals %>% filter(.data$from == .env$origin)

  plot_partial <- ggplot(
    origin_partial,
    aes(x = .data$predictor, y = .data$partial_residual)
  ) +
    geom_point(alpha = 0.22, size = 0.7) +
    geom_smooth(
      aes(weight = .data$origin_count),
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      color = "grey35",
      linewidth = 0.6
    ) +
    geom_smooth(
      aes(weight = .data$origin_count),
      method = "loess",
      formula = y ~ x,
      se = FALSE,
      color = "#B22222",
      linewidth = 0.7
    ) +
    facet_wrap(vars(.data$variable_label), scales = "free", ncol = 2) +
    labs(
      title = paste("Partielle Residuen fuer", origin, "-> AfD"),
      subtitle = "Rot: lokale Glaettung; grau: angenommener linearer Zusammenhang",
      x = "Standardisierte Strukturvariable",
      y = "Partielles Residuum"
    ) +
    theme_minimal() +
    theme(
      panel.spacing = grid::unit(1, "lines"),
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey95", color = NA)
    )

  safe_origin <- gsub("[^A-Za-z0-9]+", "_", origin)
  ggsave(
    file.path(
      linearity_chart_dir,
      paste0("partielle_residuen_", safe_origin, "_to_AfD.png")
    ),
    plot_partial,
    width = 12,
    height = 14,
    dpi = 300
  )
}

message("Regressionsdiagnostik gespeichert unter: ", output_dir)
message("Diagnosegrafiken gespeichert unter: ", chart_dir)
