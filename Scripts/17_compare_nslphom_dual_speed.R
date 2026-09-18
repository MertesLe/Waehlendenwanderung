# Laufzeit und Ergebnisse von nslphom_dual mit OSQP und lp_solve vergleichen.

library(dplyr)
library(ggplot2)
library(tidyr)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")
source("Functions/nslphom_functions.R", encoding = "UTF-8")

ensure_data_dirs()
check_lphom_available()
check_osqp_available()

threshold <- getOption("waehlendenwanderung.party_threshold", 0.12)
iter_max <- getOption("waehlendenwanderung.speed_comparison_iter_max", 3L)
tol <- getOption("waehlendenwanderung.speed_comparison_tol", 1e-5)
n_repetitions <- getOption("waehlendenwanderung.speed_comparison_repetitions", 3L)
n_test_units <- getOption("waehlendenwanderung.speed_comparison_units", Inf)
selection_mode <- match.arg(
  getOption("waehlendenwanderung.speed_comparison_selection", "all"),
  c("all", "first", "random")
)
seed <- getOption("waehlendenwanderung.speed_comparison_seed", 42L)
osqp_local_solver <- match.arg(
  getOption("waehlendenwanderung.speed_comparison_osqp_local_solver", "lp_solve"),
  c("lp_solve", "symphony", "osqp")
)

if (n_repetitions < 1L || n_repetitions %% 1L != 0L) {
  stop("n_repetitions muss eine positive ganze Zahl sein.")
}

input_paths <- c(
  input2021 = file.path(data_dir_cleaned, "nslphom_input_2021.rds"),
  input2025 = file.path(data_dir_cleaned, "nslphom_input_2025.rds"),
  input_long = file.path(data_dir_cleaned, "nslphom_input_long.rds"),
  party_thresholds = file.path(data_dir_cleaned, "partei_schwellenwerte.rds"),
  input_checks = file.path(data_dir_validation, "nslphom_input_checks.rds")
)

missing_files <- input_paths[!file.exists(input_paths)]

if (length(missing_files) > 0) {
  stop(
    "Zentrale nslphom-Inputdateien fehlen. Fuehre zuerst Skript 01 aus: ",
    paste(missing_files, collapse = ", ")
  )
}

# Dieselben vorbereiteten Ost-Wahldaten wie im Hauptmodell einlesen.
inputs <- list(
  input2021 = readRDS(input_paths[["input2021"]]) %>% arrange(.data$agg_schluessel),
  input2025 = readRDS(input_paths[["input2025"]]) %>% arrange(.data$agg_schluessel),
  input_long = readRDS(input_paths[["input_long"]]),
  party_thresholds = readRDS(input_paths[["party_thresholds"]]),
  input_checks = readRDS(input_paths[["input_checks"]])
)

ost_ids <- inputs$input2021$agg_schluessel[
  is_ostdeutschland_ohne_berlin(inputs$input2021$agg_schluessel)
]

if (selection_mode != "all") {
  if (!is.finite(n_test_units) || n_test_units < 1L) {
    stop("Fuer first oder random muss n_test_units eine positive Zahl sein.")
  }

  n_test_units <- min(as.integer(n_test_units), length(ost_ids))

  if (selection_mode == "first") {
    ost_ids <- head(ost_ids, n_test_units)
  } else {
    set.seed(seed)
    ost_ids <- sort(sample(ost_ids, n_test_units, replace = FALSE))
  }
}

inputs$input2021 <- inputs$input2021 %>% filter(.data$agg_schluessel %in% ost_ids)
inputs$input2025 <- inputs$input2025 %>% filter(.data$agg_schluessel %in% ost_ids)
inputs$input_long <- inputs$input_long %>% filter(.data$agg_schluessel %in% ost_ids)
inputs$input_checks <- inputs$input_checks %>% filter(.data$agg_schluessel %in% ost_ids)

validation <- validate_prepared_nslphom_inputs(inputs, threshold = threshold)
ids <- inputs$input2021$agg_schluessel
origin_counts <- make_count_matrix(inputs$input2021)
destination_counts <- make_count_matrix(inputs$input2025)

run_label <- getOption(
  "waehlendenwanderung.speed_comparison_name",
  paste0(
    "ost_", selection_mode,
    "_n", nrow(origin_counts),
    "_iter", iter_max
  )
)
run_label <- gsub("[^A-Za-z0-9_]+", "_", run_label)

if (!identical(rownames(origin_counts), rownames(destination_counts))) {
  stop("Die Reihenfolge der Aggregationseinheiten unterscheidet sich zwischen beiden Wahlen.")
}

output_dir <- file.path(data_dir_validation, "nslphom_dual_speed_comparison")
chart_dir <- file.path("Charts", "Validierung", "nslphom_dual_speed_comparison")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)

# Nur die fuer den Ergebnisvergleich benoetigten Fitbestandteile im Speicher behalten.
compact_fit <- function(fit) {
  list(
    global_probability = fit$VTM12.w,
    global_votes = fit$VTM.votes.w,
    local_votes = fit$VTM.votes.units.w,
    EHet = fit$EHet,
    HETe_weighted = fit$HETe.w,
    HETe_average = fit$HETe.a,
    iter_12 = fit$nslphom.object.12$iter,
    iter_selected_12 = fit$nslphom.object.12$iter.min,
    iter_21 = fit$nslphom.object.21$iter,
    iter_selected_21 = fit$nslphom.object.21$iter.min
  )
}

runtime_results <- list()
comparison_fits <- list()

old_options <- options(
  waehlendenwanderung.osqp_local_solver = osqp_local_solver
)
on.exit(options(old_options), add = TRUE)

message(
  "Vergleiche beide Dual-Varianten mit ", nrow(origin_counts),
  " Einheiten, ", ncol(origin_counts), " Gruppen, iter_max = ", iter_max,
  " und ", n_repetitions, " Wiederholungen."
)

# Die Reihenfolge abwechseln, damit ein systematischer Warm-up-Vorteil reduziert wird.
for (repetition in seq_len(n_repetitions)) {
  methods <- if (repetition %% 2L == 1L) {
    c("osqp_dual", "lphom_dual")
  } else {
    c("lphom_dual", "osqp_dual")
  }

  for (method in methods) {
    solver <- if (method == "osqp_dual") "osqp" else "lp_solve"
    gc()
    gc(reset = TRUE)

    message("Wiederholung ", repetition, ": starte ", method, ".")
    fit <- NULL
    error_message <- NA_character_

    timing <- system.time({
      fit <- tryCatch(
        fit_nslphom_dual_model(
          origin_counts = origin_counts,
          destination_counts = destination_counts,
          iter_max = iter_max,
          tol = tol,
          solver = solver,
          verbose = FALSE,
          method = method
        ),
        error = function(error) {
          error_message <<- conditionMessage(error)
          NULL
        }
      )
    })

    memory_state <- gc()
    vcells_max_mb <- memory_state["Vcells", "max used"] * 8 / 1024^2

    runtime_results[[length(runtime_results) + 1L]] <- tibble(
      repetition = repetition,
      method = method,
      solver = solver,
      elapsed_seconds = unname(timing[["elapsed"]]),
      user_seconds = unname(timing[["user.self"]]),
      system_seconds = unname(timing[["sys.self"]]),
      r_vcells_max_mb = vcells_max_mb,
      success = !is.null(fit),
      error = error_message
    )

    if (!is.null(fit) && is.null(comparison_fits[[method]])) {
      comparison_fits[[method]] <- compact_fit(fit)
    }

    rm(fit)
    gc()
  }
}

runtime <- bind_rows(runtime_results)

runtime_summary <- runtime %>%
  group_by(.data$method, .data$solver) %>%
  summarise(
    n_runs = n(),
    n_success = sum(.data$success),
    median_elapsed_seconds = median(.data$elapsed_seconds[.data$success]),
    mean_elapsed_seconds = mean(.data$elapsed_seconds[.data$success]),
    min_elapsed_seconds = min(.data$elapsed_seconds[.data$success]),
    max_elapsed_seconds = max(.data$elapsed_seconds[.data$success]),
    median_r_vcells_max_mb = median(.data$r_vcells_max_mb[.data$success]),
    .groups = "drop"
  )

if (all(c("osqp_dual", "lphom_dual") %in% runtime_summary$method)) {
  osqp_time <- runtime_summary$median_elapsed_seconds[
    runtime_summary$method == "osqp_dual"
  ]
  lphom_time <- runtime_summary$median_elapsed_seconds[
    runtime_summary$method == "lphom_dual"
  ]
  speedup_lphom_over_osqp <- lphom_time / osqp_time
} else {
  speedup_lphom_over_osqp <- NA_real_
}

settings <- tibble(
  run_label = run_label,
  analysis_region = "ostdeutschland_ohne_berlin",
  n_units = nrow(origin_counts),
  n_groups = ncol(origin_counts),
  groups = paste(colnames(origin_counts), collapse = ", "),
  threshold = threshold,
  iter_max = iter_max,
  tol = tol,
  n_repetitions = n_repetitions,
  selection_mode = selection_mode,
  seed = seed,
  osqp_local_solver = osqp_local_solver,
  speedup_lphom_over_osqp = speedup_lphom_over_osqp,
  lphom_version = as.character(utils::packageVersion("lphom")),
  osqp_version = as.character(utils::packageVersion("osqp"))
)

saveRDS(
  list(settings = settings, runtime = runtime, runtime_summary = runtime_summary),
  file.path(output_dir, paste0(run_label, "_speed_comparison.rds"))
)

if (!all(c("osqp_dual", "lphom_dual") %in% names(comparison_fits))) {
  stop(
    "Mindestens eine Variante war in keiner Wiederholung erfolgreich. ",
    "Die Laufzeit- und Fehlertabelle wurde dennoch gespeichert."
  )
}

osqp_fit <- comparison_fits[["osqp_dual"]]
lphom_fit <- comparison_fits[["lphom_dual"]]

# Matrizen in identischer Reihenfolge in lange Vergleichstabellen umformen.
if (
  !identical(dimnames(osqp_fit$local_votes), dimnames(lphom_fit$local_votes)) ||
    !identical(dimnames(osqp_fit$global_probability), dimnames(lphom_fit$global_probability))
) {
  stop("Die Dimensionsnamen der beiden Fitvarianten stimmen nicht ueberein.")
}

osqp_local_probability <- local_votes_to_probabilities(osqp_fit$local_votes)
lphom_local_probability <- local_votes_to_probabilities(lphom_fit$local_votes)

local_comparison <- as.data.frame(
  as.table(osqp_local_probability),
  stringsAsFactors = FALSE
) %>%
  transmute(
    from = as.character(.data$Var1),
    to = as.character(.data$Var2),
    agg_schluessel = as.character(.data$Var3),
    osqp_probability = as.numeric(.data$Freq)
  ) %>%
  left_join(
    as.data.frame(as.table(lphom_local_probability), stringsAsFactors = FALSE) %>%
      transmute(
        from = as.character(.data$Var1),
        to = as.character(.data$Var2),
        agg_schluessel = as.character(.data$Var3),
        lphom_probability = as.numeric(.data$Freq)
      ),
    by = c("agg_schluessel", "from", "to")
  ) %>%
  mutate(difference = .data$osqp_probability - .data$lphom_probability)

global_comparison <- as.data.frame(
  as.table(osqp_fit$global_probability),
  stringsAsFactors = FALSE
) %>%
  transmute(
    from = as.character(.data$Var1),
    to = as.character(.data$Var2),
    osqp_probability = as.numeric(.data$Freq)
  ) %>%
  left_join(
    as.data.frame(as.table(lphom_fit$global_probability), stringsAsFactors = FALSE) %>%
      transmute(
        from = as.character(.data$Var1),
        to = as.character(.data$Var2),
        lphom_probability = as.numeric(.data$Freq)
      ),
    by = c("from", "to")
  ) %>%
  mutate(difference = .data$osqp_probability - .data$lphom_probability)

ehet_comparison <- tibble(
  agg_schluessel = rep(ids, times = ncol(osqp_fit$EHet)),
  to = rep(colnames(osqp_fit$EHet), each = length(ids)),
  osqp_ehet = as.vector(osqp_fit$EHet),
  lphom_ehet = as.vector(lphom_fit$EHet)
) %>%
  mutate(difference = .data$osqp_ehet - .data$lphom_ehet)

# Abweichungen der beiden Verfahren fuer die wichtigsten Ergebnisarten zusammenfassen.
summarise_result_difference <- function(scope, left, right) {
  difference <- left - right

  tibble(
    scope = scope,
    n = length(difference),
    mean_abs_difference = mean(abs(difference), na.rm = TRUE),
    median_abs_difference = median(abs(difference), na.rm = TRUE),
    p95_abs_difference = as.numeric(quantile(abs(difference), 0.95, na.rm = TRUE)),
    max_abs_difference = max(abs(difference), na.rm = TRUE),
    rmse = sqrt(mean(difference^2, na.rm = TRUE)),
    correlation = cor(left, right, use = "complete.obs")
  )
}

result_summary <- bind_rows(
  summarise_result_difference(
    "local_transition_probability",
    local_comparison$osqp_probability,
    local_comparison$lphom_probability
  ),
  summarise_result_difference(
    "global_transition_probability",
    global_comparison$osqp_probability,
    global_comparison$lphom_probability
  ),
  summarise_result_difference(
    "local_transition_count",
    as.vector(osqp_fit$local_votes),
    as.vector(lphom_fit$local_votes)
  ),
  summarise_result_difference(
    "global_transition_count",
    as.vector(osqp_fit$global_votes),
    as.vector(lphom_fit$global_votes)
  ),
  summarise_result_difference(
    "EHet",
    ehet_comparison$osqp_ehet,
    ehet_comparison$lphom_ehet
  )
)

fit_diagnostics <- tibble(
  method = c("osqp_dual", "lphom_dual"),
  HETe_weighted = c(osqp_fit$HETe_weighted, lphom_fit$HETe_weighted),
  HETe_average = c(osqp_fit$HETe_average, lphom_fit$HETe_average),
  iter_12 = c(osqp_fit$iter_12, lphom_fit$iter_12),
  iter_selected_12 = c(osqp_fit$iter_selected_12, lphom_fit$iter_selected_12),
  iter_21 = c(osqp_fit$iter_21, lphom_fit$iter_21),
  iter_selected_21 = c(osqp_fit$iter_selected_21, lphom_fit$iter_selected_21)
)

# Pruefen, wie genau die lokalen Matrizen die beobachteten Wahlraender reproduzieren.
make_margin_check <- function(method, local_votes) {
  reconstructed_origin <- t(apply(local_votes, c(1L, 3L), sum))
  reconstructed_destination <- t(apply(local_votes, c(2L, 3L), sum))

  tibble(
    method = method,
    max_origin_margin_error = max(abs(reconstructed_origin - origin_counts)),
    max_destination_margin_error = max(abs(reconstructed_destination - destination_counts)),
    mean_origin_margin_error = mean(abs(reconstructed_origin - origin_counts)),
    mean_destination_margin_error = mean(abs(reconstructed_destination - destination_counts))
  )
}

margin_checks <- bind_rows(
  make_margin_check("osqp_dual", osqp_fit$local_votes),
  make_margin_check("lphom_dual", lphom_fit$local_votes)
)

largest_local_differences <- local_comparison %>%
  arrange(desc(abs(.data$difference))) %>%
  slice_head(n = 100L)

result_output <- list(
  settings = settings,
  runtime = runtime,
  runtime_summary = runtime_summary,
  result_summary = result_summary,
  fit_diagnostics = fit_diagnostics,
  margin_checks = margin_checks,
  global_comparison = global_comparison,
  largest_local_differences = largest_local_differences,
  ehet_comparison = ehet_comparison
)

saveRDS(
  result_output,
  file.path(output_dir, paste0(run_label, "_result_comparison.rds"))
)

# Laufzeiten und lokale Wahrscheinlichkeiten grafisch gegenueberstellen.
runtime_plot <- ggplot(
  runtime %>% filter(.data$success),
  aes(x = .data$method, y = .data$elapsed_seconds, fill = .data$method)
) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.65) +
  geom_point(position = position_jitter(width = 0.05), size = 2) +
  scale_fill_manual(values = c(osqp_dual = "#4472C4", lphom_dual = "#B22222")) +
  labs(
    title = "Laufzeitvergleich der beiden nslphom_dual-Varianten",
    subtitle = paste(nrow(origin_counts), "Aggregationseinheiten und", n_repetitions, "Wiederholungen"),
    x = NULL,
    y = "Laufzeit in Sekunden"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  file.path(chart_dir, paste0(run_label, "_runtime_comparison.png")),
  runtime_plot,
  width = 8,
  height = 6,
  dpi = 300
)

local_plot <- ggplot(
  local_comparison,
  aes(x = .data$lphom_probability, y = .data$osqp_probability)
) +
  geom_point(alpha = 0.12, size = 0.6, color = "#4472C4") +
  geom_abline(intercept = 0, slope = 1, color = "#B22222", linewidth = 0.7) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_x_continuous(labels = scales::label_percent()) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title = "Vergleich der lokalen Uebergangswahrscheinlichkeiten",
    subtitle = "Rote Linie: identische Ergebnisse",
    x = "lphom::nslphom_dual mit lp_solve",
    y = "nslphom_dual_osqp"
  ) +
  theme_minimal()

ggsave(
  file.path(chart_dir, paste0(run_label, "_local_probability_comparison.png")),
  local_plot,
  width = 8,
  height = 8,
  dpi = 300
)

message("Benchmark gespeichert unter: ", output_dir)
message("Grafiken gespeichert unter: ", chart_dir)
print(settings)
print(runtime_summary)
print(result_summary)
print(fit_diagnostics)
print(margin_checks)
