# Zweitstimmen beider Wahlen gruppieren, 2025 skalieren und nslphom-Inputs speichern.

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")
source("Functions/nslphom_input_functions.R", encoding = "UTF-8")

ensure_data_dirs()

party_threshold <- getOption("waehlendenwanderung.party_threshold", 0.12)

wahldaten2021 <- readRDS(file.path(data_dir_cleaned, "wahldaten2021_gemappt.rds"))
wahldaten2025 <- readRDS(file.path(data_dir_cleaned, "wahldaten2025_gemappt.rds"))

# Fuer nslphom werden Zweitstimmen verwendet. CDU und CSU werden vorher zur
# Union zusammengefasst; Parteien bleiben separat, wenn sie bundesweit in
# mindestens einer Wahl den angegebenen Zweitstimmenanteil erreichen.
invalid_col2021 <- first_existing(wahldaten2021, c("^Z_Ung.ltige$"))
valid_col2021 <- first_existing(wahldaten2021, c("^Z_G.ltige$"))
invalid_col2025 <- first_existing(wahldaten2025, c("^Ung.ltige\\.\\.\\.Zweitstimmen$"))
valid_col2025 <- first_existing(wahldaten2025, c("^G.ltige\\.\\.\\.Zweitstimmen$"))

# Get second vote columns
party_cols2021 <- setdiff(
  grep("^Z_", names(wahldaten2021), value = TRUE),
  c(invalid_col2021, valid_col2021)
)
party_cols2025 <- setdiff(
  grep("\\.\\.\\.Zweitstimmen$", names(wahldaten2025), value = TRUE),
  c(invalid_col2025, valid_col2025)
)

# Parteien und Stimmen jedes Jahres einmal vorbereiten.
base2021 <- prepare_vote_year(
  data = wahldaten2021,
  jahr = 2021,
  party_cols = party_cols2021,
  invalid_col = invalid_col2021,
  valid_col = valid_col2021,
  threshold = party_threshold
)
base2025 <- prepare_vote_year(
  data = wahldaten2025,
  jahr = 2025,
  party_cols = party_cols2025,
  invalid_col = invalid_col2025,
  valid_col = valid_col2025,
  threshold = party_threshold
)

keep_parties <- bind_rows(base2021$national, base2025$national) %>%
  filter(.data$keep_party_year) %>%
  pull(.data$party) %>%
  unique() %>%
  sort()

# In beiden Jahren dieselben Parteigruppen bilden.
prepared2021 <- finalize_vote_year(base2021, keep_parties)
prepared2025 <- finalize_vote_year(base2025, keep_parties)

prepared2021$wide <- prepared2021$wide %>% arrange(.data$agg_schluessel)
prepared2025$wide <- prepared2025$wide %>% arrange(.data$agg_schluessel)
stopifnot(identical(prepared2021$wide$agg_schluessel, prepared2025$wide$agg_schluessel))

# Ergebnisse von 2025 auf die Gesamtmasse der jeweiligen Einheit 2021 skalieren.
scaled_inputs <- scale_2025_to_2021(prepared2021, prepared2025)
prepared2021 <- scaled_inputs$prepared2021
prepared2025 <- scaled_inputs$prepared2025
scale_factors <- scaled_inputs$scale_factors

# Finale Datensaetze und Konsistenzchecks bereitstellen.
input2021 <- prepared2021$wide
input2025 <- prepared2025$wide
input_long <- bind_rows(prepared2021$long, prepared2025$long)
party_thresholds <- bind_rows(prepared2021$national, prepared2025$national)
input_checks <- bind_rows(prepared2021$check, prepared2025$check)

input_groups <- setdiff(names(input2021), "agg_schluessel")
stopifnot(!any(c("CDU", "CSU") %in% input_groups))
stopifnot("Union" %in% input_groups)
stopifnot(identical(input_groups, setdiff(names(input2025), "agg_schluessel")))
stopifnot(all(rowSums(input2021[input_groups]) == rowSums(input2025[input_groups])))
stopifnot(all(abs(input_checks$differenz_input_zu_referenz) < 1e-8))
stopifnot(all(abs(input_checks$differenz_stimmen_zu_waehlenden) < 1e-8))

diagnostics <- make_nslphom_input_diagnostics(input2021, input2025)

if (interactive()) {
  plot_nslphom_input_diagnostics(diagnostics)
}

# Finale Inputs und zugehoerige Pruefdaten speichern.
saveRDS(input2021, file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_2021.rds"))
saveRDS(input2025, file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_2025.rds"))
saveRDS(input_long, file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_long.rds"))
saveRDS(party_thresholds, file.path(data_dir_cleaned, "vorlaeufig_partei_schwellenwerte.rds"))
saveRDS(input_checks, file.path(data_dir_validation, "vorlaeufig_nslphom_input_checks.rds"))
saveRDS(scale_factors, file.path(data_dir_validation, "vorlaeufig_nslphom_input_scaling_2025_to_2021.rds"))

message(
  "nslphom-Input gespeichert mit Gruppen: ",
  paste(input_groups, collapse = ", ")
)
