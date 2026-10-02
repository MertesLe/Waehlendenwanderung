# Sensitivitaet der AfD-Zuflussmodelle gegenueber hohen Leverage-Werten pruefen.
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")

model_data <- readRDS(file.path(data_dir_model_regression_ost, "modell_afd_zufluss_daten.rds"))
model_fits <- readRDS(file.path(data_dir_model_regression_ost, "modell_afd_zufluss_fits.rds"))
geometry_path <- "Data/raw/gebiete_visualisierung/vg250_01-01.utm32s.gpkg.ebenen/vg250_ebenen_0101/DE_VG250.gpkg"
chart_dir <- "Charts/Regression/leverage_ausschluss"
output_dir <- file.path(data_dir_validation, "regression_ostdeutschland")
dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

origin_groups <- sort(unique(model_data$from))
stopifnot(setequal(names(model_fits), origin_groups))

# Leverage anhand der gespeicherten Hauptmodelle bestimmen; je Modell die obersten 3 % markieren.
leverage_rows <- bind_rows(lapply(origin_groups, function(origin) {
  rows <- model_data %>%
    filter(.data$from == .env$origin) %>%
    arrange(.data$agg_schluessel)
  fit <- model_fits[[origin]]
  stopifnot(nrow(rows) == stats::nobs(fit))
  stopifnot(isTRUE(all.equal(
    unname(stats::model.response(stats::model.frame(fit))),
    rows$transition_probability,
    tolerance = 1e-12
  )))
  tibble(
    agg_schluessel = rows$agg_schluessel,
    from = origin,
    leverage = unname(stats::hatvalues(fit)),
    cooks_distance = unname(stats::cooks.distance(fit)),
    studentized_residual = unname(stats::rstudent(fit)),
    origin_count = rows$origin_count
  )
}))

# dfbeta liefert beta_voll minus beta_ohne_Einheit fuer jeden Koeffizienten.
# Alle Einzelentfernungen pruefen; fuer die zehn groessten Cook-Faelle je Modell
# auch die vollstaendigen Koeffizientenaenderungen behalten.
single_unit_effects <- lapply(origin_groups, function(origin) {
  rows <- model_data %>%
    filter(.data$from == .env$origin) %>%
    arrange(.data$agg_schluessel)
  fit <- model_fits[[origin]]
  beta <- stats::coef(fit)
  dfbeta_values <- stats::dfbeta(fit)
  stopifnot(nrow(dfbeta_values) == nrow(rows), ncol(dfbeta_values) == length(beta))
  beta_without <- sweep(-dfbeta_values, 2, beta, "+")
  changed_sign <- sweep(sign(beta_without), 2, sign(beta), "!=") &
    is.finite(beta_without)
  changed_sign[, "(Intercept)"] <- FALSE
  changed_positions <- which(changed_sign, arr.ind = TRUE)

  sign_changes <- tibble(
    agg_schluessel = rows$agg_schluessel[changed_positions[, 1]],
    from = origin,
    term = names(beta)[changed_positions[, 2]],
    beta_full = unname(beta[changed_positions[, 2]]),
    beta_without = beta_without[changed_positions],
    cooks_distance = unname(stats::cooks.distance(fit)[changed_positions[, 1]])
  )

  # Die erkannten Vorzeichenwechsel durch echte Neuschaetzungen kontrollieren.
  for (i in seq_len(nrow(sign_changes))) {
    row_index <- match(sign_changes$agg_schluessel[[i]], rows$agg_schluessel)
    refit <- stats::lm(formula(fit), data = rows[-row_index, ], weights = origin_count)
    stopifnot(isTRUE(all.equal(
      unname(stats::coef(refit)[sign_changes$term[[i]]]),
      sign_changes$beta_without[[i]],
      tolerance = 1e-7
    )))
  }

  top_cook_indices <- head(order(stats::cooks.distance(fit), decreasing = TRUE), 10)
  positions <- expand.grid(
    row_index = top_cook_indices,
    term_index = seq_along(beta),
    KEEP.OUT.ATTRS = FALSE
  )
  positions <- positions[names(beta)[positions$term_index] != "(Intercept)", ]
  loo_betas <- beta_without[as.matrix(positions)]
  top_cook_coefficients <- tibble(
    agg_schluessel = rows$agg_schluessel[positions$row_index],
    from = origin,
    term = names(beta)[positions$term_index],
    beta_full = unname(beta[positions$term_index]),
    beta_without = loo_betas,
    difference = loo_betas - unname(beta[positions$term_index]),
    sign_change = changed_sign[as.matrix(positions)]
  )
  list(sign_changes = sign_changes, top_cook_coefficients = top_cook_coefficients)
})
single_unit_sign_changes <- bind_rows(lapply(single_unit_effects, `[[`, "sign_changes"))
top_cook_coefficients <- bind_rows(lapply(single_unit_effects, `[[`, "top_cook_coefficients"))

top_n <- ceiling(0.03 * dplyr::n_distinct(model_data$agg_schluessel))
selected_rows <- leverage_rows %>%
  group_by(.data$from) %>%
  slice_max(.data$leverage, n = top_n, with_ties = FALSE) %>%
  ungroup()
excluded_ids <- unique(selected_rows$agg_schluessel)

# Dieselben Einheiten aus allen fuenf Modellen entfernen und die linearen Modelle neu schaetzen.
sensitivity <- lapply(origin_groups, function(origin) {
  all_rows <- model_data %>%
    filter(.data$from == .env$origin) %>%
    arrange(.data$agg_schluessel)
  kept_rows <- all_rows %>%
    filter(!.data$agg_schluessel %in% excluded_ids)
  full_fit <- model_fits[[origin]]
  reduced_fit <- stats::lm(
    formula(model_fits[[origin]]),
    data = kept_rows,
    weights = origin_count
  )

  # Beide Fits auf denselben verbleibenden Zeilen vergleichen; RMSE ist hier Trainingsfehler.
  full_prediction_kept <- unname(stats::predict(full_fit, newdata = kept_rows))
  reduced_prediction_kept <- unname(stats::fitted(reduced_fit))
  full_beta <- stats::coef(full_fit)
  reduced_beta <- stats::coef(reduced_fit)
  stopifnot(identical(names(full_beta), names(reduced_beta)))

  diagnostics_full <- tibble(
    agg_schluessel = all_rows$agg_schluessel,
    from = origin,
    from_label = label_party_group(origin),
    scenario = "Alle Einheiten",
    fitted = unname(stats::fitted(full_fit)),
    standardized_residual = unname(stats::rstandard(full_fit))
  )
  diagnostics_reduced <- tibble(
    agg_schluessel = kept_rows$agg_schluessel,
    from = origin,
    from_label = label_party_group(origin),
    scenario = "Ohne Leverage-Einheiten",
    fitted = reduced_prediction_kept,
    standardized_residual = unname(stats::rstandard(reduced_fit))
  )

  list(
    diagnostics = bind_rows(diagnostics_full, diagnostics_reduced),
    coefficients = tibble(
      from = origin,
      from_label = label_party_group(origin),
      term = names(full_beta),
      beta_full = unname(full_beta),
      beta_without = unname(reduced_beta),
      difference = unname(reduced_beta - full_beta),
      sign_change = sign(full_beta) != sign(reduced_beta)
    ),
    model_comparison = tibble(
      from = origin,
      n_full = nrow(all_rows),
      n_without = nrow(kept_rows),
      n_excluded = nrow(all_rows) - nrow(kept_rows),
      weighted_rmse_full_on_kept = sqrt(stats::weighted.mean(
        (kept_rows$transition_probability - full_prediction_kept)^2,
        kept_rows$origin_count
      )),
      weighted_rmse_without_on_kept = sqrt(stats::weighted.mean(
        (kept_rows$transition_probability - reduced_prediction_kept)^2,
        kept_rows$origin_count
      )),
      mean_abs_prediction_change_on_kept = mean(abs(
        reduced_prediction_kept - full_prediction_kept
      )),
      n_abs_standardized_residual_gt_3_full = sum(abs(stats::rstandard(full_fit)) > 3),
      n_abs_standardized_residual_gt_3_without = sum(abs(stats::rstandard(reduced_fit)) > 3)
    )
  )
})

diagnostics_comparison <- bind_rows(lapply(sensitivity, `[[`, "diagnostics")) %>%
  mutate(scenario = factor(.data$scenario,
                           levels = c("Alle Einheiten", "Ohne Leverage-Einheiten")))
diagnostics <- diagnostics_comparison %>%
  filter(.data$scenario == "Ohne Leverage-Einheiten")
coefficient_comparison <- bind_rows(lapply(sensitivity, `[[`, "coefficients"))
model_comparison <- bind_rows(lapply(sensitivity, `[[`, "model_comparison"))

plot_qq <- ggplot(diagnostics, aes(sample = .data$standardized_residual)) +
  stat_qq(alpha = 0.35, size = 0.9) +
  stat_qq_line(color = "#B22222", linewidth = 0.7) +
  facet_wrap(vars(.data$from_label), scales = "free") +
  labs(x = "Theoretische Quantile", y = "Beobachtete Quantile") +
  theme_minimal(base_size = 16)
ggsave(file.path(chart_dir, "qq_nach_leverage_ausschluss.png"),
       plot_qq, width = 12, height = 7, dpi = 300, bg = "white")

plot_residuals <- ggplot(diagnostics, aes(x = .data$fitted, y = .data$standardized_residual)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#B22222") +
  facet_wrap(vars(.data$from_label), scales = "free_x") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3),
                     guide = guide_axis(check.overlap = TRUE)) +
  labs(x = "Vorhergesagte \u00dcbergangswahrscheinlichkeit", y = "Standardisiertes Residuum") +
  theme_minimal(base_size = 16) +
  theme(panel.spacing.x = grid::unit(1.5, "lines"))
ggsave(file.path(chart_dir, "residuen_nach_leverage_ausschluss.png"),
       plot_residuals, width = 12, height = 7, dpi = 300, bg = "white")

# Beide Stichproben nebeneinander zeigen: Ausschluss und Neuschaetzung wirken gemeinsam.
qq_comparison <- ggplot(diagnostics_comparison,
                        aes(sample = .data$standardized_residual)) +
  stat_qq(alpha = 0.35, size = 0.65) +
  stat_qq_line(color = "#B22222", linewidth = 0.6) +
  facet_grid(rows = vars(.data$scenario), cols = vars(.data$from_label)) +
  labs(x = "Theoretische Quantile", y = "Beobachtete Quantile") +
  theme_minimal(base_size = 16) +
  theme(panel.spacing.x = grid::unit(1, "lines"))
ggsave(file.path(chart_dir, "qq_vor_nach_leverage_ausschluss.png"),
       qq_comparison, width = 16, height = 8, dpi = 300, bg = "white")

residual_comparison <- ggplot(
  diagnostics_comparison,
  aes(x = .data$fitted, y = .data$standardized_residual)
) +
  geom_point(alpha = 0.22, size = 0.65) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
              color = "#B22222", linewidth = 0.7) +
  facet_grid(rows = vars(.data$scenario), cols = vars(.data$from_label),
             scales = "free_x") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3),
                     guide = guide_axis(check.overlap = TRUE)) +
  labs(x = "Vorhergesagte \u00dcbergangswahrscheinlichkeit",
       y = "Standardisiertes Residuum") +
  theme_minimal(base_size = 16) +
  theme(panel.spacing.x = grid::unit(1.5, "lines"))
ggsave(file.path(chart_dir, "residuen_vor_nach_leverage_ausschluss.png"),
       residual_comparison, width = 16, height = 8, dpi = 300, bg = "white")

# Ein Koeffizientenpaar je Kovariate und Herkunftsmodell direkt vergleichen.
variable_labels <- c(
  einwohnerdichte_2023_z = "Einwohnerdichte",
  supermarktEntfernung_2023_z = "Supermarktentfernung",
  alterMean_2023_z = "Durchschnittsalter",
  wanderungssaldo_2023_z = "Wanderungssaldo",
  pendler50_2023_z = "Fernpendleranteil",
  steuereinnahmen_2023_z = "Steuereinnahmen",
  haushaltsgroesseMean_2023_z = "Haushaltsgr\u00f6\u00dfe",
  anteilHaushalteNiedrigesEinkommen_2023_z = "Niedrigeinkommensanteil",
  arbeitslosenanteilErwerbsfaehige_2023_z = "Arbeitslosenanteil",
  distanz_staatsgrenze_km_2023_z = "Grenzdistanz"
)
coefficient_plot_data <- coefficient_comparison %>%
  filter(.data$term != "(Intercept)") %>%
  mutate(
    variable = if_else(.data$term %in% names(variable_labels),
                       unname(variable_labels[.data$term]), .data$term),
    variable = factor(.data$variable, levels = rev(unname(variable_labels))),
    beta_full_pp = 100 * .data$beta_full,
    beta_without_pp = 100 * .data$beta_without
  )
coefficient_points <- coefficient_plot_data %>%
  select("from_label", "variable", "beta_full_pp", "beta_without_pp") %>%
  pivot_longer(c("beta_full_pp", "beta_without_pp"),
               names_to = "scenario", values_to = "beta_pp") %>%
  mutate(scenario = recode(.data$scenario,
                           beta_full_pp = "Alle Einheiten",
                           beta_without_pp = "Ohne Leverage-Einheiten"))

coefficient_plot <- ggplot(coefficient_plot_data, aes(y = .data$variable)) +
  geom_vline(xintercept = 0, color = "grey55", linewidth = 0.4) +
  geom_segment(aes(x = .data$beta_full_pp, xend = .data$beta_without_pp,
                   yend = .data$variable), color = "grey55", linewidth = 0.5) +
  geom_point(data = coefficient_points,
             aes(x = .data$beta_pp, color = .data$scenario), size = 2) +
  facet_wrap(vars(.data$from_label), ncol = 2, scales = "free_x") +
  scale_color_manual(values = c("Alle Einheiten" = "#333333",
                                "Ohne Leverage-Einheiten" = "#2F75B5"),
                     name = NULL) +
  scale_x_continuous(labels = scales::label_number(decimal.mark = ","),
                     breaks = scales::breaks_pretty(n = 4)) +
  labs(x = "Koeffizient (Prozentpunkte je Standardabweichung)", y = NULL) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom", panel.spacing = grid::unit(1.5, "lines"))
ggsave(file.path(chart_dir, "koeffizienten_vor_nach_leverage_ausschluss.png"),
       coefficient_plot, width = 14, height = 13, dpi = 300, bg = "white")

# Die 2025-Gemeindeflaechen auf die Schluessel der harmonisierten Analyseeinheiten abbilden.
municipalities <- sf::st_read(geometry_path, layer = "vg250_gem", quiet = TRUE) %>%
  filter(
    .data$GF == 4,
    .data$BSG == 1,
    substr(as.character(.data$AGS), 1, 2) %in% c("12", "13", "14", "15", "16")
  ) %>%
  mutate(gemeindeschluessel = normalize_ags(.data$AGS), gemeindename = as.character(.data$GEN)) %>%
  select("gemeindeschluessel", "gemeindename")

key_lookup <- tibble(agg_schluessel = unique(model_data$agg_schluessel)) %>%
  mutate(gemeindeschluessel = lapply(.data$agg_schluessel, split_keys)) %>%
  unnest(gemeindeschluessel)

unit_names <- key_lookup %>%
  left_join(sf::st_drop_geometry(municipalities), by = "gemeindeschluessel") %>%
  group_by(.data$agg_schluessel) %>%
  summarise(
    name = paste(sort(unique(stats::na.omit(.data$gemeindename))), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(name = if_else(.data$name == "", .data$agg_schluessel, .data$name))

excluded_names <- unit_names %>%
  filter(.data$agg_schluessel %in% excluded_ids) %>%
  arrange(.data$agg_schluessel) %>%
  mutate(number = row_number(), label = sprintf("%02d  %s", .data$number, .data$name))

# Cook-Distanz kombiniert Residuen und Einfluss auf den Fit. Die fuenf groessten
# Werte je Modell zeigen; ein Wert ueber 1 ist hier nur ein Diagnosehinweis.
single_unit_sign_changes <- single_unit_sign_changes %>%
  left_join(unit_names, by = "agg_schluessel") %>%
  mutate(selected_by_leverage = .data$agg_schluessel %in% excluded_ids) %>%
  arrange(.data$from, desc(.data$cooks_distance))
top_cook_coefficients <- top_cook_coefficients %>%
  left_join(unit_names, by = "agg_schluessel")

cook_extremes <- leverage_rows %>%
  group_by(.data$from) %>%
  slice_max(.data$cooks_distance, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(unit_names, by = "agg_schluessel") %>%
  left_join(
    single_unit_sign_changes %>%
      count(.data$from, .data$agg_schluessel, name = "n_sign_changes"),
    by = c("from", "agg_schluessel")
  ) %>%
  mutate(
    from_label = label_party_group(.data$from),
    n_sign_changes = coalesce(.data$n_sign_changes, 0L),
    selected_by_leverage = .data$agg_schluessel %in% excluded_ids,
    plot_id = paste(.data$from, .data$agg_schluessel, sep = "__"),
    plot_name = if_else(nchar(.data$name) > 48,
                        paste0(substr(.data$name, 1, 45), "..."), .data$name)
  )

cook_labels <- stats::setNames(cook_extremes$plot_name, cook_extremes$plot_id)
cook_plot <- ggplot(cook_extremes,
                    aes(x = reorder(.data$plot_id, .data$cooks_distance),
                        y = .data$cooks_distance)) +
  geom_col(aes(fill = .data$n_sign_changes > 0), width = 0.75) +
  coord_flip() +
  facet_wrap(vars(.data$from_label), scales = "free_y", ncol = 2) +
  scale_x_discrete(labels = function(x) unname(cook_labels[x])) +
  scale_fill_manual(
    values = c(`FALSE` = "#2F75B5", `TRUE` = "#B22222"),
    labels = c(`FALSE` = "Nein", `TRUE` = "Ja"),
    name = "Vorzeichenwechsel bei\nEinzelausschluss"
  ) +
  labs(x = NULL, y = "Cook-Distanz") +
  theme_minimal(base_size = 16) +
  theme(panel.spacing = grid::unit(1.5, "lines"), legend.position = "bottom")
ggsave(file.path(chart_dir, "cook_distanz_top5_je_modell.png"),
       cook_plot, width = 15, height = 13, dpi = 300, bg = "white")

# Markierung je Modell und Einflusswerte der 47 Einheiten fuer die fachliche Pruefung sichern.
excluded_units <- selected_rows %>%
  group_by(.data$agg_schluessel) %>%
  summarise(
    n_models_top_3_percent = n(),
    selected_in_models = paste(.data$from, collapse = ", "),
    .groups = "drop"
  ) %>%
  left_join(
    leverage_rows %>%
      filter(.data$agg_schluessel %in% excluded_ids) %>%
      group_by(.data$agg_schluessel) %>%
      summarise(
        max_leverage = max(.data$leverage),
        max_cooks_distance = max(.data$cooks_distance),
        max_abs_studentized_residual = max(abs(.data$studentized_residual)),
        .groups = "drop"
      ),
    by = "agg_schluessel"
  ) %>%
  left_join(excluded_names %>% select("agg_schluessel", "number", "name"),
            by = "agg_schluessel") %>%
  arrange(.data$number)

high_cook_not_selected <- leverage_rows %>%
  filter(!.data$agg_schluessel %in% excluded_ids) %>%
  slice_max(.data$cooks_distance, n = 20) %>%
  left_join(unit_names, by = "agg_schluessel") %>%
  arrange(desc(.data$cooks_distance))

saveRDS(
  list(
    selection = tibble(
      rule = "Je Herkunftsmodell oberste 3 Prozent Leverage; Vereinigung der Einheiten",
      n_top_per_model = top_n,
      n_excluded_unique = length(excluded_ids),
      n_units_full = dplyr::n_distinct(model_data$agg_schluessel)
    ),
    excluded_units = excluded_units,
    selected_model_rows = selected_rows,
    cook_extremes = cook_extremes %>%
      select(-"plot_id", -"plot_name"),
    top_cook_coefficient_changes = top_cook_coefficients,
    single_unit_sign_changes = single_unit_sign_changes,
    high_cook_not_selected = high_cook_not_selected,
    coefficient_comparison = coefficient_comparison,
    model_comparison = model_comparison,
    diagnostic_rows = diagnostics_comparison
  ),
  file.path(output_dir, "leverage_sensitivitaet_afd_zufluss.rds")
)

map_units <- municipalities %>%
  inner_join(key_lookup, by = "gemeindeschluessel") %>%
  group_by(.data$agg_schluessel) %>%
  summarise(.groups = "drop") %>%
  left_join(excluded_names %>% select("agg_schluessel", "number"),
            by = "agg_schluessel")

if (!setequal(excluded_ids, map_units$agg_schluessel[!is.na(map_units$number)])) {
  stop("Fuer mindestens eine ausgeschlossene Einheit fehlt die 2025-Gemeindegeometrie.")
}

label_points <- map_units %>%
  filter(!is.na(.data$number)) %>%
  sf::st_point_on_surface()

map_plot <- ggplot() +
  geom_sf(data = map_units, fill = "grey90", color = "white", linewidth = 0.06) +
  geom_sf(data = filter(map_units, !is.na(.data$number)),
          fill = "#2F75B5", color = "white", linewidth = 0.12) +
  geom_sf_label(data = label_points, aes(label = .data$number),
                size = 4, linewidth = 0, fill = "white", alpha = 0.9) +
  coord_sf(datum = NA, expand = FALSE) +
  theme_void(base_size = 16)

# Die nummerierten Gemeinden in einer Spalte auffuehren; lange Namen erhalten mehr Zeilenhoehe.
wrapped <- lapply(excluded_names$label, function(x) paste(strwrap(x, width = 42), collapse = "\n"))
line_count <- lengths(strsplit(unlist(wrapped), "\n"))
block_height <- line_count + 0.35
label_y <- 0.95 * (1 - (cumsum(block_height) - block_height / 2) / sum(block_height))
legend_data <- excluded_names %>%
  mutate(label = unlist(wrapped), y = label_y)

legend_plot <- ggplot(legend_data, aes(x = 0, y = .data$y, label = .data$label)) +
  geom_text(hjust = 0, vjust = 0.5, size = 5.2, lineheight = 0.95) +
  annotate("text", x = 0, y = 0.995,
           label = "Blau: ausgeschlossen   Grau: verbleibende Einheiten",
           hjust = 0, vjust = 1, size = 4.8) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void(base_size = 16) +
  theme(plot.margin = margin(10, 10, 10, 10))

map_path <- file.path(chart_dir, "ausgeschlossene_leverage_einheiten_ostdeutschland.png")
grDevices::png(map_path, width = 5400, height = 4800, res = 300, bg = "white")
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(
  1, 2, widths = grid::unit(c(0.56, 0.44), "null")
)))
print(map_plot, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1), newpage = FALSE)
print(legend_plot, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2), newpage = FALSE)
grDevices::dev.off()

message(length(excluded_ids), " Einheiten ausgeschlossen; ",
        sum(coefficient_comparison$sign_change & coefficient_comparison$term != "(Intercept)"),
        " Kovariaten mit Vorzeichenwechsel beim gemeinsamen Ausschluss; ",
        nrow(single_unit_sign_changes),
        " Vorzeichenwechsel beim Entfernen jeweils einer Einheit. Grafiken: ", chart_dir)
