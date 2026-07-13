library(dplyr)
library(tidyr)
library(purrr)
library(broom)

dir.create("Data/cleaned", recursive = TRUE, showWarnings = FALSE)

transitions <- readRDS("Data/cleaned/vorlaeufig_transition_matrices_long.rds")
struktur <- readRDS("Data/cleaned/vorlaeufig_inkar_agg_delta.rds")

model_data <- transitions %>%
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

fit_one_model <- function(data) {
  if (
    nrow(data) < 30 ||
      sd(data$transition_probability, na.rm = TRUE) < 1e-8 ||
      sum(data$origin_count, na.rm = TRUE) <= 0
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
    transition_probability ~
      delta_arbeitslosigkeit_z +
      delta_auslaenderanteil_z +
      delta_kaufkraft_z,
    data = data,
    weights = origin_count
  )

  tidy(fit)
}

model_coefficients <- model_data %>%
  group_by(
    from,
    to
  ) %>%
  group_modify(~ fit_one_model(.x)) %>%
  ungroup() %>%
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
    sd_response = sd(transition_probability, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(model_data, "Data/cleaned/vorlaeufig_modell_daten.rds")
saveRDS(model_coefficients, "Data/cleaned/vorlaeufig_modell_coefficients.rds")
saveRDS(model_checks, "Data/cleaned/vorlaeufig_modell_checks.rds")

write.csv(model_data, "Data/cleaned/vorlaeufig_modell_daten.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(model_coefficients, "Data/cleaned/vorlaeufig_modell_coefficients.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(model_checks, "Data/cleaned/vorlaeufig_modell_checks.csv", row.names = FALSE, fileEncoding = "UTF-8")
