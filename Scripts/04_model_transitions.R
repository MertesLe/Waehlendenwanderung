# AfD-Zufluesse mit den INKAR-Strukturmerkmalen verbinden und getrennte Regressionen schaetzen.

library(dplyr)
library(tidyr)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")
source("Functions/regression_functions.R", encoding = "UTF-8")

ensure_data_dirs()

transitions <- readRDS(file.path(data_dir_model_nslphom_ost, "vorlaeufig_transition_matrices_long.rds"))
struktur <- readRDS(file.path(data_dir_cleaned, "vorlaeufig_inkar_kovariaten_2023.rds"))
struktur_covariates <- get_structure_covariates(struktur)

if (!all(is_ostdeutschland_ohne_berlin(unique(transitions$agg_schluessel)))) {
  stop("Der Regressionsinput enthaelt Einheiten ausserhalb Ostdeutschlands oder Berlin.")
}

# Modelliert werden AfD-Zufluesse aus allen Herkunftsgruppen ausser AfD selbst.
# Die Kovariaten stammen aktuell aus INKAR 2023 und werden z-standardisiert.
model_outputs <- make_transition_model_outputs(
  transitions = transitions,
  struktur = struktur,
  covariates = struktur_covariates
)

write_transition_model_outputs(model_outputs, output_dir = data_dir_model_regression_ost)
