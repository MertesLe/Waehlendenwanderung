# Zentrale Skripte der Datenaufbereitung und Modellierung in festgelegter Reihenfolge ausfuehren.

if (!interactive()) {
  View <- function(...) invisible(NULL)
  hist <- function(...) invisible(NULL)
}

source("Scripts/mapping_gebiete.R", encoding = "UTF-8")
source("Scripts/mapping_wahldaten.R", encoding = "UTF-8")
source("Scripts/01_prepare_nslphom_input.R", encoding = "UTF-8")
source("Scripts/02_estimate_transitions.R", encoding = "UTF-8")
source("Scripts/cleaning_strukturdaten.R", encoding = "UTF-8")
source("Scripts/03_prepare_inkar_covariates.R", encoding = "UTF-8")
source("Scripts/04_model_transitions.R", encoding = "UTF-8")

# Der speicherintensive Deutschlandfit mit EHet-Diagnose wird separat ueber
# Scripts/02a_fit_deutschland_homogenitaet.R auf dem leistungsstaerkeren PC ausgefuehrt.
