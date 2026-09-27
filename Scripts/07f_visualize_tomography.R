# Das oekologische Inferenzproblem als zweidimensionalen Tomography Plot darstellen.

library(dplyr)
library(ggplot2)
library(scales)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")

charts_dir <- file.path("Charts", "Tomography")
dir.create(charts_dir, recursive = TRUE, showWarnings = FALSE)

input2021_path <- file.path(data_dir_cleaned, "nslphom_input_2021.rds")
input2025_path <- file.path(data_dir_cleaned, "nslphom_input_2025.rds")
fit_path <- getOption(
  "waehlendenwanderung.tomography_fit_path",
  file.path(data_dir_model_nslphom_ost, "nslphom_ost_endoutput.rds")
)
n_selected <- getOption("waehlendenwanderung.tomography_n_selected", 12L)

required_files <- c(input2021_path, input2025_path, fit_path)
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop("Fuer den Tomography Plot fehlen Dateien: ", paste(missing_files, collapse = ", "))
}

# Dieselben vorbereiteten Ost-Inputs wie in der Hauptschaetzung einlesen.
input2021 <- readRDS(input2021_path) %>%
  filter(is_ostdeutschland_ohne_berlin(.data$agg_schluessel)) %>%
  arrange(.data$agg_schluessel)
input2025 <- readRDS(input2025_path) %>%
  filter(is_ostdeutschland_ohne_berlin(.data$agg_schluessel)) %>%
  arrange(.data$agg_schluessel)

stopifnot(
  identical(input2021$agg_schluessel, input2025$agg_schluessel),
  all(c("Union", "AfD") %in% names(input2021)),
  all(c("Union", "AfD") %in% names(input2025))
)

groups2021 <- setdiff(names(input2021), "agg_schluessel")
groups2025 <- setdiff(names(input2025), "agg_schluessel")
stopifnot(identical(groups2021, groups2025))

# Das 6x6-Problem nur fuer die Illustration auf Union/uebrige Gruppen und
# AfD/uebrige Gruppen reduzieren. X und T sind beobachtete Randanteile.
tomography <- tibble(
  agg_schluessel = input2021$agg_schluessel,
  total = rowSums(input2021[groups2021]),
  X = input2021$Union / total,
  T = input2025$AfD / total
) %>%
  filter(.data$total > 0, .data$X > 0, .data$X < 1) %>%
  mutate(
    b_min = pmax(0, (.data$T - (1 - .data$X)) / .data$X),
    b_max = pmin(1, .data$T / .data$X),
    w_at_b_min = pmin(1, pmax(0, (.data$T - .data$X * .data$b_min) / (1 - .data$X))),
    w_at_b_max = pmin(1, pmax(0, (.data$T - .data$X * .data$b_max) / (1 - .data$X)))
  )

if (nrow(tomography) != nrow(input2021)) {
  warning("Einheiten ohne auswertbaren Union-Anteil wurden im Tomography Plot ausgelassen.")
}

# Lokale und globale nslphom-Schaetzungen auf dasselbe 2x2-Problem projizieren.
fit_output <- readRDS(fit_path)
local_matrices <- fit_output$local_matrices_long
global_matrix <- fit_output$global_matrix

required_local_cols <- c(
  "agg_schluessel", "from", "to", "transition_probability",
  "origin_count", "estimated_transition_count"
)

if (is.null(local_matrices) || !all(required_local_cols %in% names(local_matrices))) {
  stop("Der nslphom-Endoutput enthaelt keine vollstaendigen lokalen Matrizen.")
}

fit_groups <- sort(unique(c(as.character(local_matrices$from), as.character(local_matrices$to))))

if (!setequal(fit_groups, groups2021)) {
  stop(
    "Der gespeicherte Ost-Fit verwendet nicht dieselben Gruppen wie die aktuellen Inputs. ",
    "Fuehre zuerst Scripts/07_estimate_transitions.R erneut aus. Fit: ",
    paste(fit_groups, collapse = ", "), "; Input: ", paste(groups2021, collapse = ", ")
  )
}

# b_i ist Union -> AfD. w_i fasst alle anderen Herkunftsgruppen gewichtet zusammen.
local_points <- local_matrices %>%
  mutate(from = as.character(.data$from), to = as.character(.data$to)) %>%
  group_by(.data$agg_schluessel) %>%
  summarise(
    b_local = .data$transition_probability[.data$from == "Union" & .data$to == "AfD"][1],
    non_union_to_afd = sum(
      .data$estimated_transition_count[.data$from != "Union" & .data$to == "AfD"],
      na.rm = TRUE
    ),
    non_union_total = sum(
      .data$origin_count[!duplicated(.data$from) & .data$from != "Union"],
      na.rm = TRUE
    ),
    w_local = .data$non_union_to_afd / .data$non_union_total,
    .groups = "drop"
  )

global_point <- global_matrix %>%
  mutate(from = as.character(.data$from), to = as.character(.data$to)) %>%
  summarise(
    b_global = .data$transition_probability[.data$from == "Union" & .data$to == "AfD"][1],
    non_union_to_afd = sum(
      .data$estimated_transition_count[.data$from != "Union" & .data$to == "AfD"],
      na.rm = TRUE
    ),
    non_union_total = sum(
      .data$origin_count[!duplicated(.data$from) & .data$from != "Union"],
      na.rm = TRUE
    ),
    w_global = .data$non_union_to_afd / .data$non_union_total
  )

tomography <- tomography %>%
  left_join(local_points, by = "agg_schluessel") %>%
  mutate(
    reconstructed_T = .data$X * .data$b_local + (1 - .data$X) * .data$w_local,
    projection_error = abs(.data$T - .data$reconstructed_T)
  )

if (
  anyNA(tomography$b_local) ||
    anyNA(tomography$w_local) ||
    max(tomography$projection_error, na.rm = TRUE) > 1e-6
) {
  stop(
    "Die lokalen nslphom-Punkte liegen nicht ausreichend genau auf den ",
    "Tomography-Linien. Maximaler Projektionsfehler: ",
    signif(max(tomography$projection_error, na.rm = TRUE), 4)
  )
}

# Alle zulaessigen Linien sehr transparent darstellen. Die Ueberlagerung zeigt,
# wie stark die unbekannten Uebergaenge trotz bekannter Raender unterbestimmt sind.
tomography_plot <- ggplot(tomography) +
  geom_segment(
    aes(x = .data$b_min, y = .data$w_at_b_min, xend = .data$b_max, yend = .data$w_at_b_max),
    colour = "#244F73",
    alpha = 0.035,
    linewidth = 0.35
  ) +
  geom_point(
    data = global_point,
    aes(x = .data$b_global, y = .data$w_global),
    colour = "#B51E36",
    size = 3
  ) +
  annotate(
    "text",
    x = global_point$b_global,
    y = global_point$w_global,
    label = expression(P^G),
    hjust = -0.35,
    vjust = -0.6,
    colour = "#B51E36",
    size = 4.5
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    x = "P(AfD 2025 | Union 2021)",
    y = "P(AfD 2025 | \u00fcbrige Gruppen 2021)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(charts_dir, "tomography_constraints_ostdeutschland.png"),
  tomography_plot,
  width = 8.5,
  height = 7,
  dpi = 320
)

# Gleichmaessig ueber den Union-Anteil verteilte Einheiten fuer eine lesbare
# schematische Darstellung des global-lokalen nslphom-Prinzips auswaehlen.
ordered_units <- tomography %>% arrange(.data$X, .data$T)
selected_rows <- unique(round(seq(
  1,
  nrow(ordered_units),
  length.out = min(as.integer(n_selected), nrow(ordered_units))
)))
selected_units <- ordered_units[selected_rows, ]

principle_plot <- ggplot(selected_units) +
  geom_segment(
    aes(x = .data$b_min, y = .data$w_at_b_min, xend = .data$b_max, yend = .data$w_at_b_max),
    colour = "#8B98A5",
    linewidth = 0.65,
    alpha = 0.75
  ) +
  geom_segment(
    aes(
      x = global_point$b_global,
      y = global_point$w_global,
      xend = .data$b_local,
      yend = .data$w_local
    ),
    colour = "#B51E36",
    linewidth = 0.45,
    linetype = "dashed",
    alpha = 0.55
  ) +
  geom_point(
    aes(x = .data$b_local, y = .data$w_local),
    colour = "#244F73",
    size = 2.4
  ) +
  geom_point(
    data = global_point,
    aes(x = .data$b_global, y = .data$w_global),
    colour = "#B51E36",
    size = 3.6
  ) +
  annotate(
    "text",
    x = global_point$b_global,
    y = global_point$w_global,
    label = expression(P^G),
    hjust = -0.35,
    vjust = -0.7,
    colour = "#B51E36",
    size = 4.5
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    x = "P(AfD 2025 | Union 2021)",
    y = "P(AfD 2025 | \u00fcbrige Gruppen 2021)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(charts_dir, "tomography_nslphom_principle_ostdeutschland.png"),
  principle_plot,
  width = 8.5,
  height = 7,
  dpi = 320
)

message("Tomography-Grafiken gespeichert unter: ", charts_dir)
