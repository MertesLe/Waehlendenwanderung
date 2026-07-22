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
prepared_inputs <- prepare_nslphom_inputs(
  wahldaten2021 = wahldaten2021,
  wahldaten2025 = wahldaten2025,
  threshold = party_threshold
)

if (interactive()) {
  plot_nslphom_input_diagnostics(prepared_inputs$diagnostics)
}

save_nslphom_inputs(prepared_inputs)

message(
  "nslphom-Input gespeichert mit Gruppen: ",
  paste(setdiff(names(prepared_inputs$prepared2021$wide), "agg_schluessel"), collapse = ", ")
)
