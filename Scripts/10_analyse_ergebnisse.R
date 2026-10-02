# Ergebnisanalyse der fuenf AfD-Zuflussmodelle ohne erneute Schaetzung.

library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)

model_dir <- "Data/modeloutput/regression/ostdeutschland"
bootstrap_dir <- "Data/modeloutput/bootstrap"
chart_dir <- "Charts/Regression/ergebnisse"
dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)

model_data <- readRDS(file.path(model_dir, "modell_afd_zufluss_daten.rds"))
model_fits <- readRDS(file.path(model_dir, "modell_afd_zufluss_fits.rds"))
main_coefficients <- readRDS(file.path(model_dir, "modell_afd_zufluss_coefficients.rds"))
bootstrap_intervals <- readRDS(file.path(bootstrap_dir, "bootstrap_beta_intervals.rds"))
bootstrap_settings <- readRDS(file.path(bootstrap_dir, "bootstrap_settings.rds"))

origin_order <- c("Union", "SPD", "Nichtwaehler", "Andere", "LINKE_GRUENE")
origin_labels <- c(
  Union = "Union", SPD = "SPD", Nichtwaehler = "Nichtw\u00e4hlende",
  Andere = "Andere", LINKE_GRUENE = "Linke/Gr\u00fcne"
)
term_labels <- c(
  einwohnerdichte_2023_z = "Einwohnerdichte",
  supermarktEntfernung_2023_z = "Supermarktentfernung",
  alterMean_2023_z = "Durchschnittsalter",
  wanderungssaldo_2023_z = "Wanderungssaldo",
  pendler50_2023_z = "Fernpendleranteil",
  steuereinnahmen_2023_z = "Kommunale Steuerkraft im Kreis",
  haushaltsgroesseMean_2023_z = "Haushaltsgr\u00f6\u00dfe",
  anteilHaushalteNiedrigesEinkommen_2023_z = "Niedrige Haushaltseinkommen",
  arbeitslosenanteilErwerbsfaehige_2023_z = "Arbeitslosenanteil",
  distanz_staatsgrenze_km_2023_z = "Entfernung zur Staatsgrenze"
)
term_order <- names(term_labels)

stopifnot(
  setequal(unique(model_data$from), origin_order),
  all(table(model_data$from) == 1078),
  nrow(bootstrap_intervals) == 50,
  all(bootstrap_intervals$n_success == bootstrap_settings$n_bootstrap[[1]]),
  bootstrap_settings$sample_size[[1]] == 1078
)

# Verteilungen und gewichtete Modellanpassung je Herkunft zusammenstellen.
distribution <- model_data %>%
  group_by(from) %>%
  summarise(
    n = n(),
    minimum = min(transition_probability),
    q25 = as.numeric(quantile(transition_probability, 0.25)),
    median = median(transition_probability),
    q75 = as.numeric(quantile(transition_probability, 0.75)),
    maximum = max(transition_probability),
    weighted_mean = weighted.mean(transition_probability, origin_count),
    .groups = "drop"
  )

fit_metrics <- tibble(from = names(model_fits)) %>%
  rowwise() %>%
  mutate(
    r_squared = summary(model_fits[[from]])$r.squared,
    weighted_rmse = sqrt(weighted.mean(
      residuals(model_fits[[from]])^2,
      weights(model_fits[[from]])
    )),
    predictions_outside_unit_interval = sum(
      fitted(model_fits[[from]]) < 0 | fitted(model_fits[[from]]) > 1
    )
  ) %>%
  ungroup()

# Hauptkoeffizienten mit Perzentilintervallen der vollstaendigen Bootstrapkette verbinden.
coefficients <- main_coefficients %>%
  filter(term != "(Intercept)") %>%
  select(from, term, estimate) %>%
  inner_join(
    bootstrap_intervals %>%
      select(from, term, median_estimate, ci_lower, ci_upper, n_success),
    by = c("from", "term"), relationship = "one-to-one"
  ) %>%
  mutate(
    excludes_zero = ci_lower > 0 | ci_upper < 0,
    from_label = factor(origin_labels[from], levels = origin_labels[origin_order]),
    term_label = factor(term_labels[term], levels = rev(term_labels[term_order])),
    estimate_pp = 100 * estimate,
    ci_lower_pp = 100 * ci_lower,
    ci_upper_pp = 100 * ci_upper
  )
stopifnot(nrow(coefficients) == 50, !anyNA(coefficients$term_label))

# Oberstes Viertel der Einheiten je Herkunft mit den uebrigen Einheiten vergleichen.
# Dieser ungewichtete Profilvergleich ist deskriptiv und nicht das Regressionsmodell.
profile <- model_data %>%
  group_by(from) %>%
  mutate(high_transition = transition_probability >= quantile(transition_probability, 0.75)) %>%
  ungroup() %>%
  select(from, high_transition, all_of(term_order)) %>%
  pivot_longer(all_of(term_order), names_to = "term", values_to = "z_value") %>%
  group_by(from, term, high_transition) %>%
  summarise(mean_z = mean(z_value), n = n(), .groups = "drop") %>%
  pivot_wider(names_from = high_transition, values_from = c(mean_z, n)) %>%
  mutate(
    difference_z = mean_z_TRUE - mean_z_FALSE,
    from_label = factor(origin_labels[from], levels = origin_labels[origin_order]),
    term_label = factor(term_labels[term], levels = rev(term_labels[term_order]))
  )

saveRDS(
  list(distribution = distribution, fit_metrics = fit_metrics,
       coefficients = coefficients, high_transition_profile = profile,
       bootstrap_settings = bootstrap_settings),
  file.path(model_dir, "ergebnisanalyse_afd_zufluss.rds")
)

forest_plot <- function(groups, filename, height) {
  plot_data <- coefficients %>% filter(from %in% groups)
  p <- ggplot(plot_data, aes(y = term_label)) +
    geom_vline(xintercept = 0, color = "grey65", linewidth = 0.4) +
    geom_segment(aes(x = ci_lower_pp, xend = ci_upper_pp, yend = term_label),
                 color = "#3F6996", linewidth = 0.9) +
    geom_point(aes(x = estimate_pp), color = "#162C42", size = 2.2) +
    facet_grid(from_label ~ ., scales = "fixed") +
    labs(x = "Hauptsch\u00e4tzung und 95-%-Bootstrapintervall (Prozentpunkte je 1 SD)", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      strip.text.y = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(10, 15, 10, 10)
    )
  ggsave(file.path(chart_dir, filename), p, width = 8.1, height = height,
         dpi = 250, bg = "white")
}

forest_plot(c("Union", "SPD", "Nichtwaehler"), "koeffizienten_union_spd_nichtwaehlende.png", 6.2)
forest_plot(c("Andere", "LINKE_GRUENE"), "koeffizienten_andere_linke_gruene.png", 6.8)

profile_plot <- ggplot(profile, aes(x = from_label, y = term_label, fill = difference_z)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(aes(label = sprintf("%+.1f", difference_z)), size = 3.3) +
  scale_fill_gradient2(low = "#4479A8", mid = "white", high = "#C7654F", midpoint = 0,
                       name = "Differenz\n(Standardabw.)") +
  labs(x = "Herkunftsgruppe", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10), legend.position = "right")
ggsave(file.path(chart_dir, "profil_oberes_viertel.png"), profile_plot,
       width = 9.3, height = 6.2, dpi = 250, bg = "white")

# Dieselben harmonisierten Flaechen fuer Struktur- und Uebergangskarten benutzen.
geometry_path <- paste0(
  "Data/raw/gebiete_visualisierung/",
  "vg250_01-01.utm32s.gpkg.ebenen/vg250_ebenen_0101/DE_VG250.gpkg"
)
if (!file.exists(geometry_path)) stop("Gemeindegeometrien fehlen: ", geometry_path)

municipalities <- st_read(geometry_path, layer = "vg250_gem", quiet = TRUE) %>%
  filter(GF == 4, BSG == 1, substr(as.character(AGS), 1, 2) %in% c("12", "13", "14", "15", "16")) %>%
  mutate(gemeindeschluessel = sprintf("%08s", as.character(AGS))) %>%
  select(gemeindeschluessel)

key_lookup <- model_data %>%
  distinct(agg_schluessel) %>%
  mutate(gemeindeschluessel = strsplit(agg_schluessel, ",\\s*")) %>%
  unnest(gemeindeschluessel)
stopifnot(all(unique(model_data$agg_schluessel) %in% key_lookup$agg_schluessel))

agg_geometry <- municipalities %>%
  inner_join(key_lookup, by = "gemeindeschluessel") %>%
  group_by(agg_schluessel) %>%
  summarise(.groups = "drop")
stopifnot(setequal(agg_geometry$agg_schluessel, unique(model_data$agg_schluessel)))

save_pair_map <- function(variable, origin, label, filename, probability = FALSE) {
  values <- model_data %>%
    filter(from == origin) %>%
    transmute(agg_schluessel, value = .data[[variable]])
  map_data <- agg_geometry %>% inner_join(values, by = "agg_schluessel")
  stopifnot(nrow(map_data) == 1078, !anyNA(map_data$value))
  labels <- if (probability) scales::label_percent(accuracy = 1) else
    scales::label_number(big.mark = ".", decimal.mark = ",", accuracy = 0.1)
  p <- ggplot(map_data) +
    geom_sf(aes(fill = value), color = "white", linewidth = 0.03) +
    scale_fill_viridis_c(option = "C", labels = labels, name = label) +
    coord_sf(datum = NA, expand = FALSE) +
    theme_void(base_size = 11) +
    theme(legend.position = "bottom", legend.title = element_text(size = 11),
          legend.text = element_text(size = 9),
          plot.margin = margin(5, 5, 5, 5))
  ggsave(file.path(chart_dir, filename), p, width = 5.8, height = 6.2,
         dpi = 250, bg = "white")
  p
}

map_pairs <- tibble::tribble(
  ~variable, ~origin, ~label, ~filename, ~probability,
  "anteilHaushalteNiedrigesEinkommen_2023", "Union", "Haushalte mit niedrigem Einkommen (%)", "karte_niedrigeinkommen.png", FALSE,
  "transition_probability", "Union", "Union \u2192 AfD (%)", "karte_union_afd.png", TRUE,
  "arbeitslosenanteilErwerbsfaehige_2023", "Nichtwaehler", "Arbeitslosenanteil (%)", "karte_arbeitslosenanteil.png", FALSE,
  "transition_probability", "Nichtwaehler", "Nichtw\u00e4hlende \u2192 AfD (%)", "karte_nichtwaehlende_afd.png", TRUE,
  "distanz_staatsgrenze_km_2023", "Andere", "Entfernung zur Staatsgrenze (km)", "karte_grenzdistanz.png", FALSE,
  "transition_probability", "Andere", "Andere \u2192 AfD (%)", "karte_andere_afd.png", TRUE
)
map_plots <- vector("list", nrow(map_pairs))
for (i in seq_len(nrow(map_pairs))) {
  map_plots[[i]] <- save_pair_map(
    map_pairs$variable[i], map_pairs$origin[i], map_pairs$label[i],
    map_pairs$filename[i], map_pairs$probability[i]
  )
}

# Jedes Kartenpaar als eine Abbildung exportieren, damit die beiden Flaechen
# auch in Word und PDF sicher nebeneinander bleiben.
pair_names <- c(
  "karten_niedrigeinkommen_union_afd.png",
  "karten_arbeitslosenanteil_nichtwaehlende_afd.png",
  "karten_grenzdistanz_andere_afd.png"
)
for (pair in seq_along(pair_names)) {
  grDevices::png(file.path(chart_dir, pair_names[[pair]]),
                 width = 2800, height = 1550, res = 250, bg = "white")
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
  print(map_plots[[2 * pair - 1]],
        vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1), newpage = FALSE)
  print(map_plots[[2 * pair]],
        vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2), newpage = FALSE)
  grid::popViewport()
  grDevices::dev.off()
}

print(distribution)
print(fit_metrics)
print(coefficients %>% filter(excludes_zero) %>%
        select(from, term, estimate_pp, ci_lower_pp, ci_upper_pp))
message("Ergebnisanalyse und Grafiken gespeichert unter: ", chart_dir)
