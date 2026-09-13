# Verlauf von HETe ueber nslphom_dual-Iterationen untersuchen und ein iter_max bestimmen.

library(dplyr)
library(tidyr)
library(ggplot2)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")
source("Functions/nslphom_functions.R", encoding = "UTF-8")

ensure_data_dirs()
check_lphom_available()

if (!exists("nslphom_dual", where = asNamespace("lphom"), inherits = FALSE)) {
  stop("lphom::nslphom_dual() ist in der installierten lphom-Version nicht verfuegbar.")
}

output_dir <- file.path(data_dir_validation, "iter_max")
chart_dir <- file.path("Charts", "iter_max")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)

threshold <- getOption("waehlendenwanderung.party_threshold", 0.12)
max_iter <- as.integer(getOption("waehlendenwanderung.itermax_dual_max_iter", 100L))
solver <- getOption(
  "waehlendenwanderung.itermax_dual_solver",
  getOption("waehlendenwanderung.nslphom_solver", "osqp")
)
solver <- match.arg(solver, c("osqp", "lp_solve", "symphony"))
osqp_local_solver <- if (solver == "osqp") {
  match.arg(
    getOption("waehlendenwanderung.osqp_local_solver", "lp_solve"),
    c("lp_solve", "symphony", "osqp")
  )
} else {
  NA_character_
}
selection_mode <- getOption("waehlendenwanderung.itermax_dual_selection", "all")
selection_mode <- match.arg(selection_mode, c("all", "first", "last", "random"))
n_units <- getOption("waehlendenwanderung.itermax_dual_units", Inf)
seed <- getOption("waehlendenwanderung.itermax_dual_seed", 42L)
run_label <- getOption(
  "waehlendenwanderung.itermax_dual_run_label",
  paste0(
    "ost_dual_",
    solver,
    if (solver == "osqp") paste0("_local_", osqp_local_solver) else "",
    "_",
    if (selection_mode == "all") "all" else paste0(selection_mode, "_", n_units)
  )
)
plot_matrix_type <- getOption("waehlendenwanderung.itermax_dual_plot_matrix_type", "weighted")
plot_matrix_type <- match.arg(plot_matrix_type, c("weighted", "average"))

if (max_iter < 1L) {
  stop("max_iter muss mindestens 1 sein.")
}

if (solver == "osqp") {
  check_osqp_available()
}

# Fuer die iter_max-Diagnose wird tol absichtlich auf -Inf gesetzt.
# Dadurch bricht nslphom nicht vorzeitig ab und die Iterationen 1 bis max_iter
# entsprechen den ersten max_iter Iterationen eines kuerzeren Laufes.
sequence_tol <- getOption("waehlendenwanderung.itermax_dual_tol", -Inf)

# Aggregationseinheiten fuer die Iterationsdiagnose vollstaendig oder als Teilmenge auswaehlen.
select_itermax_ids <- function(ids, selection_mode, n_units, seed) {
  if (selection_mode == "all") {
    return(ids)
  }

  n_units <- as.integer(n_units)
  if (is.na(n_units) || n_units <= 0L) {
    stop("n_units muss fuer first, last oder random eine positive ganze Zahl sein.")
  }
  if (n_units > length(ids)) {
    stop("n_units ist groesser als die Zahl verfuegbarer Aggregationseinheiten.")
  }

  switch(
    selection_mode,
    first = head(ids, n_units),
    last = tail(ids, n_units),
    random = {
      set.seed(seed)
      sample(ids, size = n_units, replace = FALSE)
    }
  )
}

# Einen nslphom-Lauf ausfuehren und lokale Matrizen jeder Iteration erhalten.
nslphom_with_unit_sequences <- function(
    votes_election1,
    votes_election2,
    iter.max = 100L,
    min.first = FALSE,
    integers = FALSE,
    solver = "osqp",
    tol = -Inf) {
  if (iter.max < 0 || iter.max %% 1 > 0) {
    stop("iter.max must be a positive integer")
  }
  if (!isFALSE(integers)) {
    stop("Dieses Diagnose-Skript ist fuer kontinuierliche nslphom-Schaetzungen geschrieben.")
  }

  # Der OSQP-Hauptlauf verwendet einen eigenen nslphom-Wrapper. Dessen bereits
  # berechnete lokale Iterationsfolge wird hier nur fuer die Diagnose behalten.
  if (solver == "osqp") {
    fit <- nslphom_osqp(
      votes_election1 = votes_election1,
      votes_election2 = votes_election2,
      new_and_exit_voters = "simultaneous",
      apriori = NULL,
      uniform = TRUE,
      iter.max = iter.max,
      min.first = min.first,
      structural_zeros = NULL,
      integers = integers,
      distance.local = "abs",
      verbose = FALSE,
      burnin = 0L,
      tol = tol,
      keep_unit_sequences = TRUE
    )

    unit_sequence_array <- fit$VTM.votes.units.sequence
    votes_units_sequence <- vector("list", fit$iter + 1L)

    for (iteration_index in seq_len(fit$iter)) {
      sequence_slice <- unit_sequence_array[, , , iteration_index + 1L, drop = FALSE]
      dim(sequence_slice) <- dim(unit_sequence_array)[1L:3L]
      votes_units_sequence[[iteration_index + 1L]] <- sequence_slice
    }

    dimnames_by_unit <- c(
      dimnames(fit$VTM.complete),
      list(rownames(fit$origin))
    )
    for (i in seq_along(votes_units_sequence)) {
      if (!is.null(votes_units_sequence[[i]])) {
        dimnames(votes_units_sequence[[i]]) <- dimnames_by_unit
      }
    }

    return(list(
      origin = fit$origin,
      destination = fit$destination,
      HETe.sequence = fit$HETe.sequence,
      VTM.votes.units.sequence = votes_units_sequence,
      iter = fit$iter,
      inputs = fit$inputs
    ))
  }

  lphom_unit <- get_lphom_internal("lp_solver_local")(
    uniform = TRUE,
    distance.local = "abs"
  )
  lphom_inic <- lphom::lphom(
    votes_election1 = votes_election1,
    votes_election2 = votes_election2,
    new_and_exit_voters = "simultaneous",
    apriori = NULL,
    lambda = 0.5,
    uniform = TRUE,
    structural_zeros = NULL,
    integers = integers,
    verbose = FALSE,
    solver = solver
  )

  lphom0 <- lphom_inic
  iter <- 0L
  dif.max <- Inf
  VTM.iter <- lphom0$VTM.complete <- lphom_inic$VTM.complete
  HETe.sequence <- lphom_inic$HETe
  votes_units_sequence <- vector("list", iter.max + 1L)

  while (iter < iter.max && dif.max > tol) {
    VTM_units <- votos_units <- array(
      NA_real_,
      c(dim(lphom_inic$VTM.complete), nrow(lphom_inic$origin))
    )

    for (i in seq_len(nrow(lphom_inic$origin))) {
      VTM_units[, , i] <- lphom_unit(
        lphom.object = lphom0,
        iii = i,
        solver = solver
      )
      votos_units[, , i] <- VTM_units[, , i] /
        rowSums(VTM_units[, , i]) *
        lphom_inic$origin[i, ]
      VTM_units[lphom_inic$origin[i, ] == 0L, , i] <- 0L
    }

    votos_units[is.na(votos_units)] <- 0L
    VTM_votos_homogeneos <- get_lphom_internal("HET_MT.votos_MT.prop_Y")(votos_units)
    VTM.complete <- VTM_votos_homogeneos$MT.pro

    iter <- iter + 1L
    dif.max <- max(abs(VTM.complete - VTM.iter))
    VTM.iter <- lphom0$VTM.complete <- VTM.complete
    HETe.sequence <- c(HETe.sequence, VTM_votos_homogeneos$HET)
    votes_units_sequence[[iter + 1L]] <- votos_units

    if (min.first && HETe.sequence[[iter + 1L]] > HETe.sequence[[iter]]) {
      dif.max <- -Inf
    }
  }

  dimnames_by_unit <- c(
    dimnames(lphom_inic$VTM.complete),
    list(rownames(lphom_inic$origin))
  )
  votes_units_sequence <- votes_units_sequence[seq_len(iter + 1L)]

  for (i in seq_along(votes_units_sequence)) {
    if (!is.null(votes_units_sequence[[i]])) {
      dimnames(votes_units_sequence[[i]]) <- dimnames_by_unit
    }
  }

  list(
    origin = lphom_inic$origin,
    destination = lphom_inic$destination,
    HETe.sequence = HETe.sequence,
    VTM.votes.units.sequence = votes_units_sequence,
    iter = iter,
    inputs = list(
      iter.max = iter.max,
      min.first = min.first,
      integers = integers,
      solver = solver,
      tol = tol
    )
  )
}

# Beide Richtungen iterieren und fuer jedes moegliche iter_max den Dual-HETe rekonstruieren.
nslphom_dual_with_itermax_sequence <- function(
    votes_election1,
    votes_election2,
    iter.max = 100L,
    solver = "osqp",
    tol = -Inf) {
  object12 <- nslphom_with_unit_sequences(
    votes_election1 = votes_election1,
    votes_election2 = votes_election2,
    iter.max = iter.max,
    solver = solver,
    tol = tol
  )
  object21 <- nslphom_with_unit_sequences(
    votes_election1 = votes_election2,
    votes_election2 = votes_election1,
    iter.max = iter.max,
    solver = solver,
    tol = tol
  )

  max_available_iter <- min(object12$iter, object21$iter)

  if (max_available_iter < iter.max) {
    warning(
      "Mindestens eine Richtung wurde vor max_iter beendet. ",
      "Die Auswertung nutzt nur Iterationen bis ",
      max_available_iter,
      "."
    )
  }

  make_dual_hete <- function(votes12, votes21, hete12, hete21) {
    votes_average <- (votes12 + votes21) / 2

    if (hete12 == 0 && hete21 == 0) {
      votes_weighted <- votes_average
    } else if (hete12 == 0) {
      votes_weighted <- votes12
    } else if (hete21 == 0) {
      votes_weighted <- votes21
    } else {
      votes_weighted <- (votes12 * hete12^-1 + votes21 * hete21^-1) /
        (hete12^-1 + hete21^-1)
    }

    tibble(
      HETe_dual_average = get_lphom_internal("HET_MT.votos_MT.prop_Y")(votes_average)$HET,
      HETe_dual_weighted = get_lphom_internal("HET_MT.votos_MT.prop_Y")(votes_weighted)$HET
    )
  }

  sequence_table <- bind_rows(lapply(seq_len(max_available_iter), function(current_iter_max) {
    candidate_index_12 <- seq(2L, current_iter_max + 1L)
    candidate_index_21 <- seq(2L, current_iter_max + 1L)
    selected_index_12 <- candidate_index_12[
      which.min(object12$HETe.sequence[candidate_index_12])
    ]
    selected_index_21 <- candidate_index_21[
      which.min(object21$HETe.sequence[candidate_index_21])
    ]

    votes12 <- object12$VTM.votes.units.sequence[[selected_index_12]]
    votes21 <- aperm(
      object21$VTM.votes.units.sequence[[selected_index_21]],
      c(2L, 1L, 3L)
    )
    hete12 <- object12$HETe.sequence[[selected_index_12]]
    hete21 <- object21$HETe.sequence[[selected_index_21]]
    selected_hete <- make_dual_hete(votes12, votes21, hete12, hete21)

    votes12_at_iteration <- object12$VTM.votes.units.sequence[[current_iter_max + 1L]]
    votes21_at_iteration <- aperm(
      object21$VTM.votes.units.sequence[[current_iter_max + 1L]],
      c(2L, 1L, 3L)
    )
    hete12_at_iteration <- object12$HETe.sequence[[current_iter_max + 1L]]
    hete21_at_iteration <- object21$HETe.sequence[[current_iter_max + 1L]]
    iteration_hete <- make_dual_hete(
      votes12_at_iteration,
      votes21_at_iteration,
      hete12_at_iteration,
      hete21_at_iteration
    )

    tibble(
      iter_max = current_iter_max,
      HETe_12_at_iteration = hete12_at_iteration,
      HETe_21_at_iteration = hete21_at_iteration,
      selected_iter_12 = selected_index_12 - 1L,
      selected_iter_21 = selected_index_21 - 1L,
      HETe_12_selected = hete12,
      HETe_21_selected = hete21,
      HETe_dual_average = selected_hete$HETe_dual_average,
      HETe_dual_weighted = selected_hete$HETe_dual_weighted,
      HETe_dual_average_at_iteration = iteration_hete$HETe_dual_average,
      HETe_dual_weighted_at_iteration = iteration_hete$HETe_dual_weighted
    )
  }))

  list(
    sequence = sequence_table,
    nslphom_object_12 = object12,
    nslphom_object_21 = object21
  )
}

# Dieselbe finale Oststichprobe ohne Berlin wie im Hauptlauf herstellen.
inputs <- read_prepared_nslphom_inputs()
ost_ids <- inputs$input2021$agg_schluessel[
  is_ostdeutschland_ohne_berlin(inputs$input2021$agg_schluessel)
]
inputs$input2021 <- inputs$input2021 %>% filter(.data$agg_schluessel %in% ost_ids)
inputs$input2025 <- inputs$input2025 %>% filter(.data$agg_schluessel %in% ost_ids)
inputs$input_long <- inputs$input_long %>% filter(.data$agg_schluessel %in% ost_ids)
inputs$input_checks <- inputs$input_checks %>% filter(.data$agg_schluessel %in% ost_ids)

if (nrow(inputs$input2021) == 0L || any(substr(ost_ids, 1L, 2L) == "11")) {
  stop("Die Ostdeutschland-Filterung ohne Berlin ist nicht plausibel.")
}

validation <- validate_prepared_nslphom_inputs(inputs, threshold = threshold)
all_ids <- inputs$input2021$agg_schluessel
selected_ids <- select_itermax_ids(all_ids, selection_mode, n_units, seed)

input2021_selected <- inputs$input2021 %>%
  filter(agg_schluessel %in% selected_ids) %>%
  arrange(match(agg_schluessel, selected_ids))

input2025_selected <- inputs$input2025 %>%
  filter(agg_schluessel %in% selected_ids) %>%
  arrange(match(agg_schluessel, selected_ids))

stopifnot(identical(input2021_selected$agg_schluessel, input2025_selected$agg_schluessel))
stopifnot(all(is_ostdeutschland_ohne_berlin(input2021_selected$agg_schluessel)))

origin_counts <- make_count_matrix(input2021_selected)
destination_counts <- make_count_matrix(input2025_selected)

settings <- tibble(
  run_label = run_label,
  threshold = threshold,
  max_iter = max_iter,
  sequence_tol = sequence_tol,
  solver = solver,
  osqp_local_solver = osqp_local_solver,
  selection_mode = selection_mode,
  n_units = nrow(origin_counts),
  n_units_available_ost = length(all_ids),
  seed = if (selection_mode == "random") seed else NA_integer_,
  analysis_region = "ostdeutschland_ohne_berlin",
  included_state_prefixes = "12, 13, 14, 15, 16",
  berlin_included = FALSE,
  groups = paste(validation$group_names, collapse = ", "),
  keep_parties = paste(validation$kept_parties, collapse = ", "),
  lphom_package_version = as.character(utils::packageVersion("lphom")),
  osqp_package_version = if (solver == "osqp") as.character(utils::packageVersion("osqp")) else NA_character_,
  osqp_max_iter = if (solver == "osqp") as.integer(getOption("waehlendenwanderung.osqp_max_iter", 100000L)) else NA_integer_,
  osqp_eps_abs = if (solver == "osqp") getOption("waehlendenwanderung.osqp_eps_abs", 1e-3) else NA_real_,
  osqp_eps_rel = if (solver == "osqp") getOption("waehlendenwanderung.osqp_eps_rel", 1e-3) else NA_real_,
  osqp_polishing = if (solver == "osqp") isTRUE(getOption("waehlendenwanderung.osqp_polishing", TRUE)) else NA,
  note = paste(
    "iter_max wird aus einem vollstaendigen nslphom_dual-Lauf rekonstruiert;",
    "je iter_max wird pro Richtung die bis dahin beste HETe-Iteration verwendet."
  )
)

saveRDS(settings, file.path(output_dir, paste0(run_label, "_settings.rds")))

message(
  "Starte iter_max-Diagnose fuer nslphom_dual mit ",
  nrow(origin_counts),
  " Aggregationseinheiten, max_iter = ",
  max_iter,
  " und Solver ",
  solver,
  if (solver == "osqp") paste0(" (lokal: ", osqp_local_solver, ")") else "",
  "."
)

# Einmal bis max_iter rechnen und daraus alle kuerzeren iter_max-Ergebnisse rekonstruieren.
dual_sequence <- nslphom_dual_with_itermax_sequence(
  votes_election1 = as.data.frame(origin_counts),
  votes_election2 = as.data.frame(destination_counts),
  iter.max = max_iter,
  solver = solver,
  tol = sequence_tol
)

sequence_table <- dual_sequence$sequence %>%
  mutate(
    run_label = run_label,
    .before = 1
  )

target_col <- if (plot_matrix_type == "weighted") {
  "HETe_dual_weighted"
} else {
  "HETe_dual_average"
}

# Das beste im untersuchten Bereich beobachtete iter_max festhalten.
best_iter <- sequence_table %>%
  slice_min(.data[[target_col]], n = 1, with_ties = FALSE) %>%
  transmute(
    run_label,
    target = target_col,
    best_iter_max = iter_max,
    min_HETe = .data[[target_col]],
    selected_iter_12,
    selected_iter_21,
    HETe_12_selected,
    HETe_21_selected
  )

if (best_iter$best_iter_max == max(sequence_table$iter_max)) {
  warning(
    "Das kleinste beobachtete HETe liegt bei max_iter = ",
    max_iter,
    ". Fuer eine belastbare Einschaetzung sollte der Suchbereich erhoeht werden."
  )
}

# Gewaehltes HETe je iter_max und HETe der exakten Iteration gemeinsam lang formatieren.
plot_data <- sequence_table %>%
  select(
    iter_max,
    HETe_dual_weighted,
    HETe_dual_average,
    HETe_dual_weighted_at_iteration,
    HETe_dual_average_at_iteration
  ) %>%
  pivot_longer(
    cols = starts_with("HETe_dual_"),
    names_to = "series",
    values_to = "HETe"
  ) %>%
  mutate(
    matrix_type = case_when(
      grepl("weighted", series) ~ "dual_weighted",
      grepl("average", series) ~ "dual_average",
      TRUE ~ NA_character_
    ),
    hete_type = if_else(
      grepl("_at_iteration$", series),
      "HETe exakt in Iteration",
      "HETe bei iter_max"
    )
  )

best_for_plot <- best_iter %>%
  mutate(
    iter_max = best_iter_max,
    HETe = min_HETe,
    matrix_type = if_else(target == "HETe_dual_weighted", "dual_weighted", "dual_average")
  )

iter_plot <- ggplot(plot_data, aes(x = iter_max, y = HETe, color = matrix_type, linetype = hete_type)) +
  geom_line(linewidth = 0.7) +
  geom_point(
    data = best_for_plot,
    aes(x = iter_max, y = HETe, color = matrix_type),
    inherit.aes = FALSE,
    size = 2.2
  ) +
  geom_vline(
    data = best_for_plot,
    aes(xintercept = iter_max),
    inherit.aes = FALSE,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  scale_color_manual(
    values = c(
      dual_weighted = "#1f4e79",
      dual_average = "#8c510a"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "HETe bei iter_max" = "solid",
      "HETe exakt in Iteration" = "longdash"
    )
  ) +
  labs(
    title = "nslphom_dual: HETe nach Iteration und iter_max",
    subtitle = paste0(
      "Run: ",
      run_label,
      "; Auswahlkriterium: ",
      plot_matrix_type
    ),
    x = "iter_max / Iteration",
    y = "HETe",
    color = "Matrix",
    linetype = "HETe-Verlauf"
  ) +
  theme_minimal()

# Richtungsspezifisch zeigen, ob HETe bereits stabil ist oder nur ein einzelnes
# spaetes Minimum die kombinierte Dual-Kurve bestimmt.
direction_plot_data <- sequence_table %>%
  select(
    iter_max,
    HETe_12_at_iteration,
    HETe_21_at_iteration,
    HETe_12_selected,
    HETe_21_selected
  ) %>%
  pivot_longer(
    cols = -iter_max,
    names_to = "series",
    values_to = "HETe"
  ) %>%
  mutate(
    direction = if_else(grepl("_12_", series), "2021 -> 2025", "2025 -> 2021"),
    hete_type = if_else(
      grepl("_at_iteration$", series),
      "HETe exakt in Iteration",
      "Bis iter_max ausgewaehltes HETe"
    )
  )

direction_plot <- ggplot(
  direction_plot_data,
  aes(x = iter_max, y = HETe, color = direction, linetype = hete_type)
) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = c("2021 -> 2025" = "#1f4e79", "2025 -> 2021" = "#b2182b")) +
  scale_linetype_manual(
    values = c(
      "Bis iter_max ausgewaehltes HETe" = "solid",
      "HETe exakt in Iteration" = "longdash"
    )
  ) +
  labs(
    title = "nslphom_dual: HETe der beiden Schaetzrichtungen",
    subtitle = paste0("Run: ", run_label),
    x = "iter_max / Iteration",
    y = "HETe",
    color = "Richtung",
    linetype = "HETe-Verlauf"
  ) +
  theme_minimal()

saveRDS(sequence_table, file.path(output_dir, paste0(run_label, "_hete_sequence.rds")))
saveRDS(best_iter, file.path(output_dir, paste0(run_label, "_best_iter_max.rds")))

ggsave(
  filename = file.path(chart_dir, paste0(run_label, "_hete_iter_max.png")),
  plot = iter_plot,
  width = 9,
  height = 5.5,
  dpi = 300
)

ggsave(
  filename = file.path(chart_dir, paste0(run_label, "_hete_directions.png")),
  plot = direction_plot,
  width = 9,
  height = 5.5,
  dpi = 300
)

if (interactive()) {
  print(iter_plot)
  print(direction_plot)
}

message(
  "Bestes im Suchbereich beobachtetes iter_max fuer ",
  plot_matrix_type,
  ": iter_max = ",
  best_iter$best_iter_max,
  " mit HETe = ",
  signif(best_iter$min_HETe, 5),
  "."
)
message("Ergebnisse gespeichert unter: ", output_dir)
message("Grafiken gespeichert unter: ", chart_dir)
