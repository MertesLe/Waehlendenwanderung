# Globale ungeblockte Uebergangsmatrizen als Sankey-aehnliche Flussgrafik darstellen.

library(dplyr)
library(ggplot2)

source("paths.R", encoding = "UTF-8")

charts_dir <- "Charts"
dir.create(charts_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Das Paket 'ggplot2' ist nicht installiert.")
}

ost_output_path <- getOption(
  "waehlendenwanderung.global_matrix_ost_path",
  file.path(data_dir_model_nslphom_ost, "vorlaeufig_nslphom_ost_endoutput.rds")
)

deutschland_output_path <- getOption(
  "waehlendenwanderung.global_matrix_deutschland_path",
  file.path(data_dir_model_nslphom_deutschland, "vorlaeufig_nslphom_deutschland_endoutput.rds")
)

party_order <- c(
  "Union",
  "AfD",
  "BSW",
  "GRUNE",
  "Die_Linke",
  "SPD",
  "FDP",
  "Andere",
  "Nichtwaehler"
)

party_colours <- c(
  AfD = "#58B9E8",
  BSW = "#F06D2F",
  Union = "#111111",
  GRUNE = "#54B82A",
  Die_Linke = "#C85A9B",
  SPD = "#D7193F",
  FDP = "#FFD500",
  Andere = "#6E6E6E",
  Nichtwaehler = "#BDBDBD"
)

# Interne Gruppennamen in gut lesbare Beschriftungen fuer die Grafik umwandeln.
label_group <- function(x) {
  dplyr::recode(
    x,
    GRUNE = "GR\u00dcNE",
    Die_Linke = "DIE LINKE",
    Nichtwaehler = "Nichtw\u00e4hler",
    .default = x
  )
}

# Absolute Stimmenzahlen mit deutschen Tausendertrennzeichen formatieren.
format_count <- function(x) {
  format(
    round(x),
    big.mark = ".",
    decimal.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

# Vorhandene Kategorien entsprechend der festgelegten Parteienreihenfolge sortieren.
ordered_categories <- function(categories) {
  categories <- unique(categories)
  known <- party_order[party_order %in% categories]
  unknown <- sort(setdiff(categories, party_order))

  c(known, unknown)
}

# Absolute globale Uebergangsmatrix aus einem gespeicherten Fit extrahieren.
get_matrix <- function(fit) {
  if (!is.null(fit[["VTM.votes"]])) {
    return(fit[["VTM.votes"]])
  }

  if (!is.null(fit[["VTM.complete.votes"]])) {
    return(fit[["VTM.complete.votes"]])
  }

  stop("Im Fit-Objekt wurde keine globale Stimmenmatrix gefunden.")
}

# Absolute Matrizen mehrerer Fits zellweise zu einer Gesamtmatrix addieren.
aggregate_matrices <- function(fits) {
  matrices <- lapply(fits, get_matrix)
  row_order <- ordered_categories(unique(unlist(lapply(matrices, rownames), use.names = FALSE)))
  col_order <- ordered_categories(unique(unlist(lapply(matrices, colnames), use.names = FALSE)))

  total_matrix <- matrix(
    0,
    nrow = length(row_order),
    ncol = length(col_order),
    dimnames = list(row_order, col_order)
  )

  for (matrix in matrices) {
    total_matrix[rownames(matrix), colnames(matrix)] <-
      total_matrix[rownames(matrix), colnames(matrix)] + matrix
  }

  total_matrix
}

# Globale absolute Uebergangsmatrix aus einem ungeblockten Endoutput rekonstruieren.
endoutput_to_matrix <- function(path) {
  if (!file.exists(path)) {
    stop("Der nslphom-Endoutput fehlt: ", path)
  }

  output <- readRDS(path)

  if (!"global_matrix" %in% names(output)) {
    stop("Der nslphom-Endoutput enthaelt keine global_matrix: ", path)
  }

  required_cols <- c("from", "to", "estimated_transition_count")
  missing_cols <- setdiff(required_cols, names(output$global_matrix))

  if (length(missing_cols) > 0) {
    stop(
      "global_matrix enthaelt nicht alle benoetigten Spalten: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  flows <- output$global_matrix %>%
    transmute(
      from = as.character(.data$from),
      to = as.character(.data$to),
      value = as.numeric(.data$estimated_transition_count)
    )

  row_order <- ordered_categories(flows$from)
  col_order <- ordered_categories(flows$to)

  matrix(
    0,
    nrow = length(row_order),
    ncol = length(col_order),
    dimnames = list(row_order, col_order)
  ) %>%
    {
      out <- .

      for (i in seq_len(nrow(flows))) {
        out[flows$from[[i]], flows$to[[i]]] <-
          out[flows$from[[i]], flows$to[[i]]] + flows$value[[i]]
      }

      out
    }
}

# Anzahl der Aggregationseinheiten aus einem ungeblockten Endoutput lesen.
endoutput_n_units <- function(path) {
  output <- readRDS(path)

  if ("settings" %in% names(output) && "n_units" %in% names(output$settings)) {
    return(as.integer(output$settings$n_units[[1]]))
  }

  if ("EHet_ids" %in% names(output)) {
    return(length(output$EHet_ids))
  }

  NA_integer_
}

# Matrixzellen in eine Tabelle von Flussbreiten zwischen Herkunft und Ziel umformen.
matrix_to_flows <- function(matrix, block_id) {
  as.data.frame(as.table(matrix), stringsAsFactors = FALSE) %>%
    transmute(
      nslphom_block = block_id,
      from = as.character(Var1),
      to = as.character(Var2),
      value = as.numeric(Freq)
    ) %>%
    filter(
      !is.na(value),
      value > 0
    )
}

# Vertikale Anfangs- und Endpositionen der Parteienbalken mit Abstaenden berechnen.
calculate_bar_positions <- function(totals, category_order, stack_height, gap_size = 0) {
  bars <- tibble(category = category_order) %>%
    left_join(totals, by = "category") %>%
    mutate(
      total = if_else(is.na(total), 0, total)
    ) %>%
    filter(total > 0)

  side_height <- sum(bars$total, na.rm = TRUE) + max(nrow(bars) - 1, 0) * gap_size
  top_offset <- max((stack_height - side_height) / 2, 0)

  bars %>%
    mutate(
      position = row_number(),
      y_max = stack_height - top_offset - lag(cumsum(total), default = 0) - (position - 1) * gap_size,
      y_min = y_max - total,
      y_mid = (y_min + y_max) / 2,
      label = label_group(category)
    )
}

# Jeden Fluss innerhalb der Balken in konsistenter Parteienreihenfolge stapeln.
add_flow_positions <- function(flows, left_bars, right_bars, left_order, right_order) {
  left_lookup <- left_bars %>%
    select(from = category, left_top = y_max)
  right_lookup <- right_bars %>%
    select(to = category, right_top = y_max)

  flows %>%
    mutate(
      from_rank = match(from, left_order),
      to_rank = match(to, right_order)
    ) %>%
    left_join(left_lookup, by = "from") %>%
    left_join(right_lookup, by = "to") %>%
    group_by(from) %>%
    arrange(to_rank, .by_group = TRUE) %>%
    mutate(
      left_y_max = left_top - lag(cumsum(value), default = 0),
      left_y_min = left_top - cumsum(value)
    ) %>%
    ungroup() %>%
    group_by(to) %>%
    arrange(from_rank, .by_group = TRUE) %>%
    mutate(
      right_y_max = right_top - lag(cumsum(value), default = 0),
      right_y_min = right_top - cumsum(value)
    ) %>%
    ungroup() %>%
    mutate(
      flow_id = row_number()
    )
}

# Gekruemmte Polygonpunkte fuer jeden Uebergangsfluss zwischen beiden Wahlen erzeugen.
make_ribbon_data <- function(positioned_flows, n_points = 80L) {
  bind_rows(lapply(seq_len(nrow(positioned_flows)), function(i) {
    flow <- positioned_flows[i, ]
    t <- seq(0, 1, length.out = n_points)
    bend_t <- pmin(pmax((t - 0.03) / 0.94, 0), 1)
    bend <- 3 * bend_t^2 - 2 * bend_t^3

    x <- 0.105 + t * 0.79
    left_center <- (flow$left_y_min + flow$left_y_max) / 2
    right_center <- (flow$right_y_min + flow$right_y_max) / 2
    half_width <- flow$value / 2
    y_center <- left_center + bend * (right_center - left_center)

    y_top <- y_center + half_width
    y_bottom <- y_center - half_width

    tibble(
      flow_id = flow$flow_id,
      from = flow$from,
      to = flow$to,
      value = flow$value,
      x = c(x, rev(x)),
      y = c(y_top, rev(y_bottom))
    )
  }))
}

# Vollstaendige Flussgrafik fuer eine globale oder blockbezogene Matrix erstellen.
make_block_plot <- function(
  block_id,
  fit = NULL,
  checks = NULL,
  matrix = NULL,
  title = NULL,
  subtitle_prefix = "Globale blockweise nslphom-Matrix",
  n_units_override = NA_integer_
) {
  if (is.null(matrix)) {
    matrix <- get_matrix(fit)
  }

  flows <- matrix_to_flows(matrix, block_id)
  total_value <- sum(flows$value, na.rm = TRUE)

  left_order <- ordered_categories(flows$from)
  right_order <- ordered_categories(flows$to)
  bar_gap <- total_value * 0.012
  stack_height <- total_value + (max(length(left_order), length(right_order)) - 1) * bar_gap

  left_bars <- flows %>%
    group_by(category = from) %>%
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
    calculate_bar_positions(left_order, stack_height, bar_gap) %>%
    mutate(
      side = "left",
      xmin = 0.08,
      xmax = 0.105
    )

  right_bars <- flows %>%
    group_by(category = to) %>%
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
    calculate_bar_positions(right_order, stack_height, bar_gap) %>%
    mutate(
      side = "right",
      xmin = 0.895,
      xmax = 0.92
    )

  positioned_flows <- add_flow_positions(
    flows = flows,
    left_bars = left_bars,
    right_bars = right_bars,
    left_order = left_order,
    right_order = right_order
  )

  ribbon_data <- make_ribbon_data(positioned_flows)
  bars <- bind_rows(left_bars, right_bars)
  present_groups <- unique(c(ribbon_data$to, bars$category))
  plot_colours <- party_colours[names(party_colours) %in% present_groups]
  missing_colours <- setdiff(present_groups, names(plot_colours))

  if (length(missing_colours) > 0) {
    extra_colours <- grDevices::hcl.colors(length(missing_colours), "Dark 3")
    names(extra_colours) <- missing_colours
    plot_colours <- c(plot_colours, extra_colours)
  }

  n_units <- n_units_override

  if (is.na(n_units) && !is.null(checks)) {
    block_check <- checks %>% filter(nslphom_block == block_id)

    if (nrow(block_check) > 0 && "n_units" %in% names(block_check)) {
      n_units <- block_check$n_units[[1]]
    }
  }

  if (is.null(title)) {
    title <- paste0("W\u00e4hlerwanderung 2021 -> 2025: ", block_id)
  }

  subtitle <- paste0(
    subtitle_prefix,
    ", ",
    ifelse(is.na(n_units), "", paste0("n = ", n_units, " Einheiten, ")),
    "gesch\u00e4tzte \u00dcbergangsmasse = ",
    format_count(total_value)
  )

  ggplot() +
    geom_polygon(
      data = ribbon_data,
      aes(x = x, y = y, group = flow_id, fill = to),
      alpha = 0.58,
      colour = NA
    ) +
    geom_rect(
      data = bars,
      aes(xmin = xmin, xmax = xmax, ymin = y_min, ymax = y_max, fill = category),
      colour = "white",
      linewidth = 0.25
    ) +
    geom_text(
      data = left_bars,
      aes(x = 0.065, y = y_mid, label = label),
      hjust = 1,
      size = 3.4,
      colour = "#555555",
      lineheight = 0.9
    ) +
    geom_text(
      data = right_bars,
      aes(x = 0.935, y = y_mid, label = label),
      hjust = 0,
      size = 3.4,
      colour = "#555555",
      lineheight = 0.9
    ) +
    annotate(
      "text",
      x = 0.092,
      y = stack_height * 1.055,
      label = "2021",
      fontface = "bold",
      size = 4.2
    ) +
    annotate(
      "text",
      x = 0.908,
      y = stack_height * 1.055,
      label = "2025",
      fontface = "bold",
      size = 4.2
    ) +
    scale_fill_manual(values = plot_colours, guide = "none") +
    coord_cartesian(
      xlim = c(-0.08, 1.08),
      ylim = c(0, stack_height * 1.09),
      clip = "off"
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = "Breite der Baender = geschaetzte absolute Uebergangsmasse. Die 2025-Inputs wurden je Einheit auf die 2021-Gesamtmasse skaliert."
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", colour = "#203A8F", size = 15),
      plot.subtitle = element_text(colour = "#555555", size = 10, margin = margin(t = 3, b = 10)),
      plot.caption = element_text(colour = "#777777", size = 8, hjust = 0),
      plot.margin = margin(12, 72, 12, 72)
    )
}

pdf_path <- file.path(charts_dir, "nslphom_global_matrizen_ungeblockt.pdf")
grDevices::pdf(pdf_path, width = 12, height = 7, onefile = TRUE)

message("Erzeuge globale Matrix fuer Ostdeutschland ohne Berlin.")
ost_matrix <- endoutput_to_matrix(ost_output_path)
ost_plot <- make_block_plot(
  block_id = "Ostdeutschland ohne Berlin",
  matrix = ost_matrix,
  title = "W\u00e4hlerwanderung 2021 -> 2025: Ostdeutschland ohne Berlin",
  subtitle_prefix = "Ungeblockte nslphom_dual-OSQP-Schaetzung",
  n_units_override = endoutput_n_units(ost_output_path)
)

ggplot2::ggsave(
  filename = file.path(charts_dir, "nslphom_global_matrix_ostdeutschland_ungeblockt.png"),
  plot = ost_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

print(ost_plot)

message("Erzeuge globale Matrix fuer Deutschland.")
deutschland_matrix <- endoutput_to_matrix(deutschland_output_path)
deutschland_plot <- make_block_plot(
  block_id = "Deutschland",
  matrix = deutschland_matrix,
  title = "W\u00e4hlerwanderung 2021 -> 2025: Deutschland",
  subtitle_prefix = "Ungeblockte nslphom_dual-OSQP-Schaetzung",
  n_units_override = endoutput_n_units(deutschland_output_path)
)

ggplot2::ggsave(
  filename = file.path(charts_dir, "nslphom_global_matrix_deutschland_ungeblockt.png"),
  plot = deutschland_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

print(deutschland_plot)

grDevices::dev.off()

message("Fertig. Sammel-PDF gespeichert unter: ", pdf_path)
