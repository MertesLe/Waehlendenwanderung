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
solver <- getOption("waehlendenwanderung.itermax_dual_solver", "symphony")
solver <- match.arg(solver, c("lp_solve", "symphony"))
selection_mode <- getOption("waehlendenwanderung.itermax_dual_selection", "all")
selection_mode <- match.arg(selection_mode, c("all", "first", "last", "random"))
n_units <- getOption("waehlendenwanderung.itermax_dual_units", Inf)
seed <- getOption("waehlendenwanderung.itermax_dual_seed", 42L)
run_label <- getOption(
  "waehlendenwanderung.itermax_dual_run_label",
  paste0("dual_", selection_mode, "_", if (is.finite(n_units)) n_units else "all")
)
plot_matrix_type <- getOption("waehlendenwanderung.itermax_dual_plot_matrix_type", "weighted")
plot_matrix_type <- match.arg(plot_matrix_type, c("weighted", "average"))

if (max_iter < 1L) {
  stop("max_iter muss mindestens 1 sein.")
}

# Fuer die iter_max-Diagnose wird tol absichtlich auf -Inf gesetzt.
# Dadurch bricht nslphom nicht vorzeitig ab und die Iterationen 1 bis max_iter
# entsprechen den ersten max_iter Iterationen eines kuerzeren Laufes.
sequence_tol <- getOption("waehlendenwanderung.itermax_dual_tol", -Inf)

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

nslphom_with_unit_sequences <- function(
    votes_election1,
    votes_election2,
    iter.max = 100L,
    min.first = FALSE,
    integers = FALSE,
    solver = "symphony",
    tol = -Inf) {
  if (iter.max < 0 || iter.max %% 1 > 0) {
    stop("iter.max must be a positive integer")
  }
  if (!isFALSE(integers)) {
    stop("Dieses Diagnose-Skript ist fuer kontinuierliche nslphom-Schaetzungen geschrieben.")
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

nslphom_dual_with_itermax_sequence <- function(
    votes_election1,
    votes_election2,
    iter.max = 100L,
    solver = "symphony",
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

inputs <- read_prepared_nslphom_inputs()
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

origin_counts <- make_count_matrix(input2021_selected)
destination_counts <- make_count_matrix(input2025_selected)

settings <- tibble(
  run_label = run_label,
  threshold = threshold,
  max_iter = max_iter,
  sequence_tol = sequence_tol,
  solver = solver,
  selection_mode = selection_mode,
  n_units = nrow(origin_counts),
  seed = if (selection_mode == "random") seed else NA_integer_,
  groups = paste(validation$group_names, collapse = ", "),
  keep_parties = paste(validation$kept_parties, collapse = ", "),
  note = paste(
    "iter_max wird aus einem vollstaendigen nslphom_dual-Lauf rekonstruiert;",
    "je iter_max wird pro Richtung die bis dahin beste HETe-Iteration verwendet."
  )
)

saveRDS(settings, file.path(output_dir, paste0("vorlaeufig_", run_label, "_settings.rds")))

message(
  "Starte iter_max-Diagnose fuer nslphom_dual mit ",
  nrow(origin_counts),
  " Aggregationseinheiten, max_iter = ",
  max_iter,
  " und Solver ",
  solver,
  "."
)

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
      "; Ziel fuer Optimum: ",
      plot_matrix_type
    ),
    x = "iter_max / Iteration",
    y = "HETe",
    color = "Matrix",
    linetype = "HETe-Verlauf"
  ) +
  theme_minimal()

saveRDS(sequence_table, file.path(output_dir, paste0("vorlaeufig_", run_label, "_hete_sequence.rds")))
saveRDS(best_iter, file.path(output_dir, paste0("vorlaeufig_", run_label, "_best_iter_max.rds")))

ggsave(
  filename = file.path(chart_dir, paste0("vorlaeufig_", run_label, "_hete_iter_max.png")),
  plot = iter_plot,
  width = 9,
  height = 5.5,
  dpi = 300
)

message(
  "Beste Iterationszahl fuer ",
  plot_matrix_type,
  ": iter_max = ",
  best_iter$best_iter_max,
  " mit HETe = ",
  signif(best_iter$min_HETe, 5),
  "."
)
message("Ergebnisse gespeichert unter: ", output_dir)
message("Grafik gespeichert unter: ", chart_dir)
