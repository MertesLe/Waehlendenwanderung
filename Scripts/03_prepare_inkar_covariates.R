library(data.table)
library(dplyr)
library(tidyr)

source("Functions/general_functions.R", encoding = "UTF-8")
source("paths.R", encoding = "UTF-8")

ensure_data_dirs()

inkar_basis_rds <- file.path(data_dir_intermediate, "vorlaeufig_inkar_basis_gemeinden_kreise.rds")
inkar_metadata_rds <- file.path(data_dir_intermediate, "vorlaeufig_inkar_basis_metadata.rds")

if (!file.exists(inkar_basis_rds) || !file.exists(inkar_metadata_rds)) {
  source("Scripts/cleaning_strukturdaten.R", encoding = "UTF-8")
}

inkar <- as_tibble(readRDS(inkar_basis_rds))
metadata <- readRDS(inkar_metadata_rds)
struktur_jahr <- if ("struktur_jahr" %in% names(metadata)) {
  metadata$struktur_jahr[[1]]
} else {
  2023L
}
indikator_config <- if ("inkar_indikatoren" %in% names(metadata)) {
  as_tibble(metadata$inkar_indikatoren)
} else {
  tibble(
    Kuerzel = c(
      "xbev",
      "q_arbeitslosigkeit",
      "q_kaufkraft",
      "a_ausl_bev"
    ),
    variable = c(
      "bevoelkerung",
      "arbeitslosigkeit",
      "kaufkraft",
      "auslaenderanteil"
    ),
    Raumbezug = c(
      "Gemeinden",
      "Gemeinden",
      "Gemeinden",
      "Kreise"
    )
  )
}
gemeinde_config <- indikator_config %>%
  filter(Raumbezug == "Gemeinden")
kreis_config <- indikator_config %>%
  filter(Raumbezug == "Kreise")
struktur_variablen <- setdiff(unique(indikator_config$variable), "bevoelkerung")
struktur_spalten <- paste0(struktur_variablen, "_", struktur_jahr)

if (!"bevoelkerung" %in% indikator_config$variable) {
  stop(
    "Die INKAR-Indikator-Konfiguration muss eine Variable 'bevoelkerung' ",
    "enthalten, weil sie als Aggregationsgewicht verwendet wird."
  )
}

mapping_gemeinden <- readRDS(file.path(data_dir_cleaned, "mapping_gemeinden_final_manuell_validiert.rds"))
all_aggs <- readRDS(file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_2021.rds")) %>%
  select(agg_schluessel) %>%
  distinct() %>%
  arrange(agg_schluessel)

agg_col <- get_agg_col(mapping_gemeinden)
ags_col <- grep("^Gemeindesch", names(mapping_gemeinden), value = TRUE)[1]

mapping_gemeinden <- mapping_gemeinden %>%
  transmute(
    gemeindeschluessel = .data[[ags_col]],
    agg_schluessel = .data[[agg_col]]
  ) %>%
  distinct()

gemeinde_indikatoren <- inkar %>%
  inner_join(
    gemeinde_config,
    by = c("Kuerzel", "Raumbezug")
  ) %>%
  transmute(
    gemeindeschluessel = Kennziffer,
    jahr = Zeitbezug,
    variable,
    wert = Wert
  ) %>%
  group_by(
    gemeindeschluessel,
    jahr,
    variable
  ) %>%
  summarise(
    wert = mean(wert, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = wert
  )

gemeinde_struktur <- gemeinde_indikatoren %>%
  mutate(
    kreis_schluessel = paste0(substr(gemeindeschluessel, 1, 5), "000")
  )

if (nrow(kreis_config) > 0) {
  kreis_indikatoren <- inkar %>%
    inner_join(
      kreis_config,
      by = c("Kuerzel", "Raumbezug")
    ) %>%
    transmute(
      kreis_schluessel = Kennziffer,
      jahr = Zeitbezug,
      variable,
      wert = Wert
    ) %>%
    group_by(
      kreis_schluessel,
      jahr,
      variable
    ) %>%
    summarise(
      wert = mean(wert, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = variable,
      values_from = wert
    )

  gemeinde_struktur <- gemeinde_struktur %>%
    left_join(
      kreis_indikatoren,
      by = c("kreis_schluessel", "jahr")
    )
}

inkar_gemeinden <- unique(gemeinde_struktur$gemeindeschluessel)

mapping_gemeinden <- mapping_gemeinden %>%
  mutate(
    gemeindeschluessel_inkar = case_when(
      gemeindeschluessel %in% inkar_gemeinden ~ gemeindeschluessel,
      substr(gemeindeschluessel, 1, 2) == "02" ~ "02000000",
      substr(gemeindeschluessel, 1, 2) == "11" ~ "11000000",
      TRUE ~ gemeindeschluessel
    )
  )

agg_struktur_long <- mapping_gemeinden %>%
  left_join(
    gemeinde_struktur,
    by = c("gemeindeschluessel_inkar" = "gemeindeschluessel"),
    relationship = "many-to-many"
  ) %>%
  group_by(
    agg_schluessel,
    jahr
  ) %>%
  summarise(
    n_gemeinden_mapping = n_distinct(gemeindeschluessel),
    n_gemeinden_mit_inkar = n_distinct(gemeindeschluessel[!is.na(bevoelkerung)]),
    gewicht_summe = sum(bevoelkerung, na.rm = TRUE),
    across(
      all_of(struktur_variablen),
      ~ weighted_mean_safe(.x, bevoelkerung)
    ),
    .groups = "drop"
  )

agg_struktur_wide_inner <- agg_struktur_long %>%
  filter(jahr == struktur_jahr) %>%
  mutate(
    struktur_jahr = .env$struktur_jahr
  ) %>%
  rename_with(
    ~ paste0(.x, "_", struktur_jahr),
    all_of(struktur_variablen)
  ) %>%
  rename(
    !!paste0("gewicht_summe_", struktur_jahr) := gewicht_summe
  ) %>%
  select(
    agg_schluessel,
    struktur_jahr,
    all_of(struktur_spalten),
    all_of(paste0("gewicht_summe_", struktur_jahr)),
    n_gemeinden_mapping,
    n_gemeinden_mit_inkar
  ) %>%
  arrange(agg_schluessel)

agg_struktur_wide <- all_aggs %>%
  left_join(
    agg_struktur_wide_inner,
    by = "agg_schluessel"
  )

struktur_checks <- agg_struktur_wide %>%
  summarise(
    n_agg = n(),
    across(
      all_of(struktur_spalten),
      ~ sum(is.na(.x)),
      .names = "n_missing_{.col}"
    ),
    struktur_jahr = .env$struktur_jahr
  )

struktur_missing <- agg_struktur_wide %>%
  filter(
    if_any(
      all_of(struktur_spalten),
      is.na
    )
  )

# Interne Plausibilitaetspruefungen, nicht als eigene Datensaetze gespeichert:
# - struktur_checks: Zaehlt fehlende 2023-Kovariaten nach Variable.
# - struktur_missing: Enthielte die betroffenen agg_schluessel; diese Tabelle
#   muss leer sein, damit alle Analyseeinheiten Strukturkovariaten besitzen.
if (nrow(struktur_missing) > 0) {
  stop(
    "INKAR-Aggregation unvollstaendig: struktur_missing enthaelt ",
    nrow(struktur_missing),
    " agg_schluessel ohne vollstaendige 2023-Kovariaten."
  )
}

saveRDS(agg_struktur_long, file.path(data_dir_cleaned, "vorlaeufig_inkar_agg_long.rds"))
saveRDS(agg_struktur_wide, file.path(data_dir_cleaned, "vorlaeufig_inkar_kovariaten_2023.rds"))
