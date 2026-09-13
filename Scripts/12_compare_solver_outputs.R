# Ergebnisse identischer Dual-Schaetzungen mit OSQP und RSymphony vergleichen.

library(dplyr)
library(tidyr)

source("paths.R", encoding = "UTF-8")

ensure_data_dirs()

output_dir <- file.path(data_dir_validation, "solververgleich")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

left_output_path <- getOption(
  "waehlendenwanderung.solververgleich_left_path",
  file.path(
    data_dir_model_nslphom_ost,
    "nslphom_ost_endoutput.rds"
  )
)

right_output_path <- getOption(
  "waehlendenwanderung.solververgleich_right_path",
  file.path(
    data_dir_model_nslphom,
    "ostdeutschland_symphony",
    "nslphom_ost_symphony_endoutput.rds"
  )
)

left_label <- getOption("waehlendenwanderung.solververgleich_left_label", "osqp_dual")
right_label <- getOption("waehlendenwanderung.solververgleich_right_label", "symphony_dual")

comparison_name <- getOption(
  "waehlendenwanderung.solververgleich_name",
  "ostdeutschland_osqp_dual_vs_symphony_dual"
)

comparison_name <- gsub("[^A-Za-z0-9_]+", "_", comparison_name)
diff_tolerance <- getOption("waehlendenwanderung.solververgleich_tolerance", 1e-8)

# Dateinamen fuer die gespeicherten Vergleichstabellen im Zielordner erzeugen.
output_file <- function(name) {
  file.path(output_dir, paste0(comparison_name, "_", name))
}

# Solveroutput laden und seine benoetigten Bestandteile validieren.
read_solver_output <- function(path, label) {
  if (!file.exists(path)) {
    stop("Der ", label, "-Output wurde nicht gefunden: ", path)
  }

  obj <- readRDS(path)

  required <- c("local_matrices_long", "global_matrix", "EHet", "EHet_ids", "settings")
  missing <- setdiff(required, names(obj))

  if (length(missing) > 0) {
    stop(label, "-Output enthaelt nicht alle benoetigten Elemente: ", paste(missing, collapse = ", "))
  }

  obj
}

# Einen Einstellungswert sicher aus einem Outputobjekt auslesen.
get_setting <- function(obj, name) {
  settings <- as.data.frame(obj$settings)

  if (name %in% names(settings)) {
    as.character(settings[[name]][[1]])
  } else {
    NA_character_
  }
}

# EHet-Matrix eines Solveroutputs in ein vergleichbares Longformat umformen.
make_ehet_long <- function(obj, label) {
  ehet_matrix <- as.matrix(obj$EHet)
  ids <- as.character(obj$EHet_ids)

  if (length(ids) != nrow(ehet_matrix)) {
    stop(label, ": EHet_ids passen nicht zu EHet.")
  }

  rownames(ehet_matrix) <- ids

  as.data.frame(as.table(ehet_matrix), stringsAsFactors = FALSE) %>%
    transmute(
      agg_schluessel = as.character(Var1),
      to = as.character(Var2),
      ehet = as.numeric(Freq)
    )
}

# Anzahl und Groesse der Abweichungen einer Solver-Vergleichsspalte zusammenfassen.
summarise_diff <- function(data, diff_col) {
  diff_values <- data[[diff_col]]

  tibble(
    n = length(diff_values),
    n_missing = sum(is.na(diff_values)),
    mean_abs_diff = mean(abs(diff_values), na.rm = TRUE),
    median_abs_diff = median(abs(diff_values), na.rm = TRUE),
    p95_abs_diff = as.numeric(quantile(abs(diff_values), probs = 0.95, na.rm = TRUE)),
    max_abs_diff = max(abs(diff_values), na.rm = TRUE),
    n_diff_over_tolerance = sum(abs(diff_values) > diff_tolerance, na.rm = TRUE)
  )
}

# Beide Outputs laden und zuerst identische Aggregationseinheiten sicherstellen.
left_output <- read_solver_output(left_output_path, left_label)
right_output <- read_solver_output(right_output_path, right_label)

left_ids <- as.character(left_output$EHet_ids)
right_ids <- as.character(right_output$EHet_ids)

same_ids_ordered <- identical(left_ids, right_ids)
same_ids_unordered <- setequal(left_ids, right_ids)

if (!same_ids_ordered) {
  stop(
    "Die beiden Outputs beruhen nicht auf derselben geordneten Auswahl an agg_schluesseln. ",
    "same_ids_unordered = ", same_ids_unordered,
    ". Fuer einen sauberen Solververgleich muessen Auswahl und Reihenfolge identisch sein."
  )
}

# Lokale Matrizen zellgenau ueber Einheit, Herkunft und Ziel vergleichen.
left_local <- left_output$local_matrices_long %>%
  select(
    agg_schluessel,
    from,
    to,
    origin_count,
    destination_count,
    transition_probability,
    estimated_transition_count
  )

right_local <- right_output$local_matrices_long %>%
  select(
    agg_schluessel,
    from,
    to,
    origin_count,
    destination_count,
    transition_probability,
    estimated_transition_count
  )

local_differences <- left_local %>%
  inner_join(
    right_local,
    by = c("agg_schluessel", "from", "to"),
    suffix = c("_left", "_right")
  ) %>%
  mutate(
    origin_count_diff = origin_count_right - origin_count_left,
    destination_count_diff = destination_count_right - destination_count_left,
    transition_probability_diff = transition_probability_right - transition_probability_left,
    estimated_transition_count_diff = estimated_transition_count_right - estimated_transition_count_left
  )

if (nrow(local_differences) != nrow(left_local) || nrow(local_differences) != nrow(right_local)) {
  stop("Die lokalen Matrizen konnten nicht vollstaendig ueber agg_schluessel, from, to verbunden werden.")
}

# Globale Matrizen zellgenau vergleichen.
left_global <- left_output$global_matrix %>%
  select(
    from,
    to,
    transition_probability,
    origin_count,
    destination_count,
    estimated_transition_count
  )

right_global <- right_output$global_matrix %>%
  select(
    from,
    to,
    transition_probability,
    origin_count,
    destination_count,
    estimated_transition_count
  )

global_differences <- left_global %>%
  inner_join(
    right_global,
    by = c("from", "to"),
    suffix = c("_left", "_right")
  ) %>%
  mutate(
    transition_probability_diff = transition_probability_right - transition_probability_left,
    origin_count_diff = origin_count_right - origin_count_left,
    destination_count_diff = destination_count_right - destination_count_left,
    estimated_transition_count_diff = estimated_transition_count_right - estimated_transition_count_left
  )

# Auch die lokalen EHet-Abweichungen beider Solver gegenueberstellen.
left_ehet <- make_ehet_long(left_output, left_label)
right_ehet <- make_ehet_long(right_output, right_label)

ehet_differences <- left_ehet %>%
  inner_join(
    right_ehet,
    by = c("agg_schluessel", "to"),
    suffix = c("_left", "_right")
  ) %>%
  mutate(
    ehet_diff = ehet_right - ehet_left
  )

settings_comparison <- tibble(
  field = c(
    "solver",
    "selection_mode",
    "seed",
    "threshold",
    "iter_max",
    "tol",
    "n_units"
  ),
  left = c(
    get_setting(left_output, "solver"),
    get_setting(left_output, "selection_mode"),
    get_setting(left_output, "seed"),
    get_setting(left_output, "threshold"),
    get_setting(left_output, "iter_max"),
    get_setting(left_output, "tol"),
    as.character(length(left_ids))
  ),
  right = c(
    get_setting(right_output, "solver"),
    get_setting(right_output, "selection_mode"),
    get_setting(right_output, "seed"),
    get_setting(right_output, "threshold"),
    get_setting(right_output, "iter_max"),
    get_setting(right_output, "tol"),
    as.character(length(right_ids))
  ),
  equal = left == right
)

# Maximale und mittlere Differenzen aller verglichenen Ergebnistypen zusammenfassen.
comparison_summary <- bind_rows(
  summarise_diff(local_differences, "transition_probability_diff") %>%
    mutate(scope = "local_transition_probability"),
  summarise_diff(local_differences, "estimated_transition_count_diff") %>%
    mutate(scope = "local_estimated_transition_count"),
  summarise_diff(global_differences, "transition_probability_diff") %>%
    mutate(scope = "global_transition_probability"),
  summarise_diff(global_differences, "estimated_transition_count_diff") %>%
    mutate(scope = "global_estimated_transition_count"),
  summarise_diff(ehet_differences, "ehet_diff") %>%
    mutate(scope = "EHet")
) %>%
  select(scope, everything())

identity_checks <- tibble(
  check = c(
    "same_ids_ordered",
    "same_ids_unordered",
    "same_local_keys_complete",
    "same_global_keys_complete",
    "same_ehet_keys_complete",
    "stored_solver_labels_differ"
  ),
  value = c(
    same_ids_ordered,
    same_ids_unordered,
    nrow(local_differences) == nrow(left_local) && nrow(local_differences) == nrow(right_local),
    nrow(global_differences) == nrow(left_global) && nrow(global_differences) == nrow(right_global),
    nrow(ehet_differences) == nrow(left_ehet) && nrow(ehet_differences) == nrow(right_ehet),
    get_setting(left_output, "solver") != get_setting(right_output, "solver")
  )
)

largest_local_probability_differences <- local_differences %>%
  arrange(desc(abs(transition_probability_diff))) %>%
  head(100)

largest_global_probability_differences <- global_differences %>%
  arrange(desc(abs(transition_probability_diff)))

largest_ehet_differences <- ehet_differences %>%
  arrange(desc(abs(ehet_diff))) %>%
  head(100)

saveRDS(settings_comparison, output_file("settings.rds"))
saveRDS(identity_checks, output_file("identity_checks.rds"))
saveRDS(comparison_summary, output_file("summary.rds"))
saveRDS(largest_local_probability_differences, output_file("largest_local_probability_differences.rds"))
saveRDS(largest_global_probability_differences, output_file("global_differences.rds"))
saveRDS(largest_ehet_differences, output_file("largest_ehet_differences.rds"))

message("Solververgleich gespeichert unter: ", output_dir)
message("Vergleich: ", left_output_path, " vs. ", right_output_path)
message("Wichtig: settings$solver ", left_label, " = ", get_setting(left_output, "solver"),
        "; settings$solver ", right_label, " = ", get_setting(right_output, "solver"))
print(settings_comparison)
print(identity_checks)
print(comparison_summary)
