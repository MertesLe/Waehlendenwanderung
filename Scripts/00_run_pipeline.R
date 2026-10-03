# Teilpipeline: Datenaufbereitung, Ostfit und Hauptregression ausfuehren.
# Deutschlandfit, Validierungen, weitere Grafiken, Bootstrap und Ergebnisanalyse laufen separat.

if (!interactive()) {
  View <- function(...) invisible(NULL)
  hist <- function(...) invisible(NULL)
}

source("Scripts/01_mapping_gebiete.R", encoding = "UTF-8")
source("Scripts/02_mapping_wahldaten.R", encoding = "UTF-8")
source("Scripts/03_prepare_nslphom_input.R", encoding = "UTF-8")
source("Scripts/04_cleaning_strukturdaten.R", encoding = "UTF-8")
source("Scripts/05_prepare_inkar_covariates.R", encoding = "UTF-8")

# Die optionale iter_max-Diagnose in Skript 06 zuvor separat auswerten.
# Der Hauptfit verwendet ohne abweichende Option iter_max = 2.
source("Scripts/07_estimate_transitions.R", encoding = "UTF-8")
source("Scripts/08_model_transitions.R", encoding = "UTF-8")

# Der speicherintensive Deutschlandfit mit EHet-Diagnose kann separat ueber
# Scripts/07a_fit_deutschland_homogenitaet.R ausgefuehrt werden.
