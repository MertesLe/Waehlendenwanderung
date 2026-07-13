library(dplyr)
library(tidyr)
library(purrr)
library(broom)

source("paths.R", encoding = "UTF-8")

ensure_data_dirs()

transitions <- readRDS(file.path(data_dir_model_nslphom, "vorlaeufig_transition_matrices_long.rds"))
struktur <- readRDS(file.path(data_dir_cleaned, "vorlaeufig_inkar_agg_delta.rds"))

afd_source_summary <- transitions %>%
  filter(to == "AfD") %>%
  group_by(from) %>%
  summarise(
    n_agg = n(),
    estimated_transition_count = sum(estimated_transition_count, na.rm = TRUE),
    origin_count = sum(origin_count, na.rm = TRUE),
    mean_transition_probability = mean(transition_probability, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    source_type = if_else(from == "AfD", "AfD_Bestand", "AfD_Zufluss"),
    share_of_estimated_afd_2025 = estimated_transition_count /
      sum(estimated_transition_count, na.rm = TRUE),
    share_of_estimated_afd_zufluss = if_else(
      from == "AfD",
      NA_real_,
      estimated_transition_count /
        sum(estimated_transition_count[from != "AfD"], na.rm = TRUE)
    )
  ) %>%
  arrange(
    desc(estimated_transition_count)
  )

model_data <- transitions %>%
  filter(
    to == "AfD",
    from != "AfD"
  ) %>%
  left_join(
    struktur,
    by = "agg_schluessel"
  ) %>%
  filter(
    !is.na(delta_arbeitslosigkeit),
    !is.na(delta_auslaenderanteil),
    !is.na(delta_kaufkraft),
    origin_count > 0,
    destination_count > 0
  ) %>%
  mutate(
    afd_2025_source_share = estimated_transition_count / destination_count,
    afd_inflow_label = paste0(from, "_to_AfD"),
    delta_arbeitslosigkeit_z = as.numeric(scale(delta_arbeitslosigkeit)),
    delta_auslaenderanteil_z = as.numeric(scale(delta_auslaenderanteil)),
    delta_kaufkraft_z = as.numeric(scale(delta_kaufkraft))
  )

fit_one_model <- function(data, response_col, weight_col) {
  if (
    nrow(data) < 30 ||
      sd(data[[response_col]], na.rm = TRUE) < 1e-8 ||
      sum(data[[weight_col]], na.rm = TRUE) <= 0
  ) {
    return(tibble(
      term = character(),
      estimate = numeric(),
      std.error = numeric(),
      statistic = numeric(),
      p.value = numeric()
    ))
  }

  fit <- lm(
    stats::reformulate(
      c(
        "delta_arbeitslosigkeit_z",
        "delta_auslaenderanteil_z",
        "delta_kaufkraft_z"
      ),
      response = response_col
    ),
    data = data,
    weights = data[[weight_col]]
  )

  tidy(fit)
}

model_coefficients <- model_data %>%
  group_by(
    from,
    to
  ) %>%
  group_modify(~ fit_one_model(.x, "transition_probability", "origin_count")) %>%
  ungroup() %>%
  mutate(
    model_target = "origin_to_AfD_probability"
  ) %>%
  arrange(
    from,
    to,
    term
  )

afd_source_share_coefficients <- model_data %>%
  group_by(
    from,
    to
  ) %>%
  group_modify(~ fit_one_model(.x, "afd_2025_source_share", "destination_count")) %>%
  ungroup() %>%
  mutate(
    model_target = "share_of_AfD_2025_by_source"
  ) %>%
  arrange(
    from,
    to,
    term
  )

model_checks <- model_data %>%
  group_by(
    from,
    to
  ) %>%
  summarise(
    n_agg = n(),
    sum_origin_count = sum(origin_count, na.rm = TRUE),
    sum_estimated_transition_count = sum(estimated_transition_count, na.rm = TRUE),
    sum_destination_count = sum(destination_count, na.rm = TRUE),
    mean_transition_probability = mean(transition_probability, na.rm = TRUE),
    mean_afd_2025_source_share = mean(afd_2025_source_share, na.rm = TRUE),
    sd_transition_probability = sd(transition_probability, na.rm = TRUE),
    sd_afd_2025_source_share = sd(afd_2025_source_share, na.rm = TRUE),
    .groups = "drop"
  )

all_model_coefficients <- bind_rows(
  model_coefficients,
  afd_source_share_coefficients
) %>%
  arrange(
    model_target,
    from,
    to,
    term
  )

legacy_model_data <- transitions %>%
  left_join(
    struktur,
    by = "agg_schluessel"
  ) %>%
  filter(
    !is.na(delta_arbeitslosigkeit),
    !is.na(delta_auslaenderanteil),
    !is.na(delta_kaufkraft),
    origin_count > 0
  ) %>%
  mutate(
    delta_arbeitslosigkeit_z = as.numeric(scale(delta_arbeitslosigkeit)),
    delta_auslaenderanteil_z = as.numeric(scale(delta_auslaenderanteil)),
    delta_kaufkraft_z = as.numeric(scale(delta_kaufkraft))
  )

legacy_model_coefficients <- legacy_model_data %>%
  group_by(
    from,
    to
  ) %>%
  group_modify(~ fit_one_model(.x, "transition_probability", "origin_count")) %>%
  ungroup() %>%
  arrange(
    from,
    to,
    term
  )

legacy_model_checks <- legacy_model_data %>%
  group_by(
    from,
    to
  ) %>%
  summarise(
    n_agg = n(),
    sum_origin_count = sum(origin_count, na.rm = TRUE),
    sd_response = sd(transition_probability, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(model_data, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_zufluss_daten.rds"))
saveRDS(model_coefficients, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_zufluss_coefficients.rds"))
saveRDS(afd_source_share_coefficients, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_source_share_coefficients.rds"))
saveRDS(all_model_coefficients, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_all_coefficients.rds"))
saveRDS(model_checks, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_zufluss_checks.rds"))
saveRDS(afd_source_summary, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_source_summary.rds"))

write.csv(model_data, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_zufluss_daten.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(model_coefficients, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_zufluss_coefficients.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(afd_source_share_coefficients, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_source_share_coefficients.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(all_model_coefficients, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_all_coefficients.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(model_checks, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_zufluss_checks.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(afd_source_summary, file.path(data_dir_model_regression, "vorlaeufig_modell_afd_source_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")

saveRDS(legacy_model_data, file.path(data_dir_model_regression, "vorlaeufig_modell_daten.rds"))
saveRDS(legacy_model_coefficients, file.path(data_dir_model_regression, "vorlaeufig_modell_coefficients.rds"))
saveRDS(legacy_model_checks, file.path(data_dir_model_regression, "vorlaeufig_modell_checks.rds"))

write.csv(legacy_model_data, file.path(data_dir_model_regression, "vorlaeufig_modell_daten.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(legacy_model_coefficients, file.path(data_dir_model_regression, "vorlaeufig_modell_coefficients.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(legacy_model_checks, file.path(data_dir_model_regression, "vorlaeufig_modell_checks.csv"), row.names = FALSE, fileEncoding = "UTF-8")
