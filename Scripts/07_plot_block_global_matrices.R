library(dplyr)
library(ggplot2)

source("paths.R", encoding = "UTF-8")

charts_dir <- "Charts"
dir.create(charts_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Das Paket 'ggplot2' ist nicht installiert.")
}

fit_path <- file.path(data_dir_model_nslphom, "vorlaeufig_nslphom_fit.rds")

if (!file.exists(fit_path)) {
  stop(
    "Die blockweisen nslphom-Fits fehlen. Erwartet wurde: ",
    fit_path
  )
}

party_order <- c(
  "CDU",
  "CSU",
  "AfD",
  "BSW",
  "GRUNE",
  "Die_Linke",
  "SPD",
  "FDP",
  "Andere",
  "Nichtwaehler",
  "NET_ENTRIES",
  "NET_EXITS"
)

party_colours <- c(
  AfD = "#58B9E8",
  BSW = "#F06D2F",
  CDU = "#111111",
  CSU = "#005CA9",
  GRUNE = "#54B82A",
  Die_Linke = "#C85A9B",
  SPD = "#D7193F",
  FDP = "#FFD500",
  Andere = "#6E6E6E",
  Nichtwaehler = "#BDBDBD",
  NET_ENTRIES = "#A0A0A0",
  NET_EXITS = "#4F4F4F"
)

block_labels <- c(
  "010+020+031+032+033+034+040" = "Nordblock: SH, HH, NI, HB",
  "051" = "NRW: Düsseldorf",
  "053" = "NRW: Köln",
  "055" = "NRW: Münster",
  "057" = "NRW: Detmold",
  "059" = "NRW: Arnsberg",
  "064" = "Hessen: Darmstadt",
  "065" = "Hessen: Gießen",
  "066" = "Hessen: Kassel",
  "071" = "Rheinland-Pfalz: Koblenz",
  "072" = "Rheinland-Pfalz: Trier",
  "073" = "Rheinland-Pfalz: Rheinhessen-Pfalz",
  "081" = "Baden-Württemberg: Stuttgart",
  "082" = "Baden-Württemberg: Karlsruhe",
  "083" = "Baden-Württemberg: Freiburg",
  "084" = "Baden-Württemberg: Tübingen",
  "091" = "Bayern: Oberbayern",
  "092" = "Bayern: Niederbayern",
  "093+094" = "Bayern: Oberpfalz/Oberfranken",
  "095" = "Bayern: Mittelfranken",
  "096" = "Bayern: Unterfranken",
  "097" = "Bayern: Schwaben",
  "100" = "Saarland",
  "111+112+120" = "Berlin/Brandenburg",
  "130" = "Mecklenburg-Vorpommern",
  "145" = "Sachsen: Chemnitz",
  "146" = "Sachsen: Dresden",
  "147" = "Sachsen: Leipzig",
  "150" = "Sachsen-Anhalt",
  "160" = "Thüringen"
)

label_group <- function(x) {
  dplyr::recode(
    x,
    GRUNE = "GRÜNE",
    Die_Linke = "DIE LINKE",
    Nichtwaehler = "Nichtwähler",
    NET_ENTRIES = "neu/wieder\nwahlberechtigt",
    NET_EXITS = "nicht mehr\nwahlberechtigt",
    .default = x
  )
}

format_count <- function(x) {
  format(
    round(x),
    big.mark = ".",
    decimal.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

ordered_categories <- function(categories) {
  categories <- unique(categories)
  known <- party_order[party_order %in% categories]
  unknown <- sort(setdiff(categories, party_order))

  c(known, unknown)
}

get_matrix <- function(fit) {
  if (!is.null(fit[["VTM.complete.votes"]])) {
    return(fit[["VTM.complete.votes"]])
  }

  if (!is.null(fit[["VTM.votes"]])) {
    return(fit[["VTM.votes"]])
  }

  stop("Im Fit-Objekt wurde keine globale Stimmenmatrix gefunden.")
}

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
    block_label <- ifelse(
      block_id %in% names(block_labels),
      block_labels[[block_id]],
      paste("Block", block_id)
    )

    title <- paste0("Wählerwanderung 2021 -> 2025: ", block_label)
  }

  subtitle <- paste0(
    subtitle_prefix,
    ", ",
    ifelse(is.na(n_units), "", paste0("n = ", n_units, " Einheiten, ")),
    "geschätzte Übergangsmasse = ",
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
      caption = "Breite der Bänder = geschätzte absolute Übergangsmasse. NET_ENTRIES/NET_EXITS entstehen durch new_and_exit_voters = 'raw'."
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", colour = "#203A8F", size = 15),
      plot.subtitle = element_text(colour = "#555555", size = 10, margin = margin(t = 3, b = 10)),
      plot.caption = element_text(colour = "#777777", size = 8, hjust = 0),
      plot.margin = margin(12, 72, 12, 72)
    )
}

nslphom_fit <- readRDS(fit_path)
checks <- nslphom_fit$checks
fits <- nslphom_fit$fits

if (is.null(names(fits)) || any(names(fits) == "")) {
  stop("Die Fits brauchen benannte Block-IDs.")
}

pdf_path <- file.path(charts_dir, "nslphom_global_matrizen_blockweise.pdf")
grDevices::pdf(pdf_path, width = 12, height = 7, onefile = TRUE)

message("Erzeuge aggregierte Deutschlandmatrix aus absoluten Block-Uebergangszahlen.")
national_matrix <- aggregate_matrices(fits)
national_n_units <- if (!is.null(checks) && "n_units" %in% names(checks)) {
  sum(checks$n_units, na.rm = TRUE)
} else {
  NA_integer_
}

national_plot <- make_block_plot(
  block_id = "Deutschland",
  matrix = national_matrix,
  title = "Wählerwanderung 2021 -> 2025: Deutschland aggregiert",
  subtitle_prefix = "Aus blockweisen nslphom-Schätzungen aggregierte nationale Matrix",
  n_units_override = national_n_units
)

ggplot2::ggsave(
  filename = file.path(charts_dir, "nslphom_global_matrix_deutschland_aggregiert.png"),
  plot = national_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

print(national_plot)

for (block_id in names(fits)) {
  message("Erzeuge Grafik fuer Block ", block_id)

  plot <- make_block_plot(
    block_id = block_id,
    fit = fits[[block_id]],
    checks = checks
  )

  print(plot)
}

grDevices::dev.off()

message("Fertig. Sammel-PDF gespeichert unter: ", pdf_path)
