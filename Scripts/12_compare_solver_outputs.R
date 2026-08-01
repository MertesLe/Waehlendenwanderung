library(dplyr)
library(tidyr)

source("paths.R", encoding = "UTF-8")

ensure_data_dirs()

output_dir <- file.path(data_dir_validation, "solververgleich")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

lp_output_path <- getOption(
  "waehlendenwanderung.solververgleich_lp_path",
  file.path(
    data_dir_model_nslphom,
    "test5000_random_dual",
    "vorlaeufig_test5000_random_nslphom_dual_endoutput.rds"
  )
)

symphony_output_path <- getOption(
  "waehlendenwanderung.solververgleich_symphony_path",
  file.path(
    data_dir_model_nslphom,
    "testSymphony5000_random_dual",
    "vorlaeufig_test5000_random_nslphom_dual_endoutput.rds"
  )
)

comparison_name <- getOption(
  "waehlendenwanderung.solververgleich_name",
  "test5000_random_dual_lp_vs_symphony"
)

comparison_name <- gsub("[^A-Za-z0-9_]+", "_", comparison_name)
diff_tolerance <- getOption("waehlendenwanderung.solververgleich_tolerance", 1e-8)

output_file <- function(name) {
  file.path(output_dir, paste0("vorlaeufig_", comparison_name, "_", name))
}

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

get_setting <- function(obj, name) {
  settings <- as.data.frame(obj$settings)

  if (name %in% names(settings)) {
    as.character(settings[[name]][[1]])
  } else {
    NA_character_
  }
}

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

lp_output <- read_solver_output(lp_output_path, "lp_solve")
symphony_output <- read_solver_output(symphony_output_path, "symphony")

lp_ids <- as.character(lp_output$EHet_ids)
symphony_ids <- as.character(symphony_output$EHet_ids)

same_ids_ordered <- identical(lp_ids, symphony_ids)
same_ids_unordered <- setequal(lp_ids, symphony_ids)

if (!same_ids_ordered) {
  stop(
    "Die beiden Outputs beruhen nicht auf derselben geordneten Auswahl an agg_schluesseln. ",
    "same_ids_unordered = ", same_ids_unordered,
    ". Fuer einen sauberen Solververgleich muessen Auswahl und Reihenfolge identisch sein."
  )
}

lp_local <- lp_output$local_matrices_long %>%
  select(
    agg_schluessel,
    from,
    to,
    origin_count,
    destination_count,
    transition_probability,
    estimated_transition_count
  )

symphony_local <- symphony_output$local_matrices_long %>%
  select(
    agg_schluessel,
    from,
    to,
    origin_count,
    destination_count,
    transition_probability,
    estimated_transition_count
  )

local_differences <- lp_local %>%
  inner_join(
    symphony_local,
    by = c("agg_schluessel", "from", "to"),
    suffix = c("_lp", "_symphony")
  ) %>%
  mutate(
    origin_count_diff = origin_count_symphony - origin_count_lp,
    destination_count_diff = destination_count_symphony - destination_count_lp,
    transition_probability_diff = transition_probability_symphony - transition_probability_lp,
    estimated_transition_count_diff = estimated_transition_count_symphony - estimated_transition_count_lp
  )

if (nrow(local_differences) != nrow(lp_local) || nrow(local_differences) != nrow(symphony_local)) {
  stop("Die lokalen Matrizen konnten nicht vollstaendig ueber agg_schluessel, from, to verbunden werden.")
}

lp_global <- lp_output$global_matrix %>%
  select(
    from,
    to,
    transition_probability,
    origin_count,
    destination_count,
    estimated_transition_count
  )

symphony_global <- symphony_output$global_matrix %>%
  select(
    from,
    to,
    transition_probability,
    origin_count,
    destination_count,
    estimated_transition_count
  )

global_differences <- lp_global %>%
  inner_join(
    symphony_global,
    by = c("from", "to"),
    suffix = c("_lp", "_symphony")
  ) %>%
  mutate(
    transition_probability_diff = transition_probability_symphony - transition_probability_lp,
    origin_count_diff = origin_count_symphony - origin_count_lp,
    destination_count_diff = destination_count_symphony - destination_count_lp,
    estimated_transition_count_diff = estimated_transition_count_symphony - estimated_transition_count_lp
  )

lp_ehet <- make_ehet_long(lp_output, "lp_solve")
symphony_ehet <- make_ehet_long(symphony_output, "symphony")

ehet_differences <- lp_ehet %>%
  inner_join(
    symphony_ehet,
    by = c("agg_schluessel", "to"),
    suffix = c("_lp", "_symphony")
  ) %>%
  mutate(
    ehet_diff = ehet_symphony - ehet_lp
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
  lp_solve = c(
    get_setting(lp_output, "solver"),
    get_setting(lp_output, "selection_mode"),
    get_setting(lp_output, "seed"),
    get_setting(lp_output, "threshold"),
    get_setting(lp_output, "iter_max"),
    get_setting(lp_output, "tol"),
    as.character(length(lp_ids))
  ),
  symphony = c(
    get_setting(symphony_output, "solver"),
    get_setting(symphony_output, "selection_mode"),
    get_setting(symphony_output, "seed"),
    get_setting(symphony_output, "threshold"),
    get_setting(symphony_output, "iter_max"),
    get_setting(symphony_output, "tol"),
    as.character(length(symphony_ids))
  ),
  equal = lp_solve == symphony
)

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
    nrow(local_differences) == nrow(lp_local) && nrow(local_differences) == nrow(symphony_local),
    nrow(global_differences) == nrow(lp_global) && nrow(global_differences) == nrow(symphony_global),
    nrow(ehet_differences) == nrow(lp_ehet) && nrow(ehet_differences) == nrow(symphony_ehet),
    get_setting(lp_output, "solver") != get_setting(symphony_output, "solver")
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
message("Vergleich: ", lp_output_path, " vs. ", symphony_output_path)
message("Wichtig: settings$solver lp_solve = ", get_setting(lp_output, "solver"),
        "; settings$solver symphony = ", get_setting(symphony_output, "solver"))
print(settings_comparison)
print(identity_checks)
print(comparison_summary)
