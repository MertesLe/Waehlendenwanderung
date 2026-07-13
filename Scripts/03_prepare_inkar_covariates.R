library(data.table)
library(dplyr)
library(tidyr)

source("functions.R", encoding = "UTF-8")
source("paths.R", encoding = "UTF-8")

ensure_data_dirs()

inkar_basis_rds <- file.path(data_dir_intermediate, "vorlaeufig_inkar_basis_gemeinden_kreise.rds")
inkar_metadata_rds <- file.path(data_dir_intermediate, "vorlaeufig_inkar_basis_metadata.rds")

if (!file.exists(inkar_basis_rds) || !file.exists(inkar_metadata_rds)) {
  source("Scripts/cleaning_strukturdaten.R", encoding = "UTF-8")
}

get_agg_col <- function(data) {
  grep("^agg\\.", names(data), value = TRUE)[1]
}

weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0

  if (!any(ok)) {
    return(NA_real_)
  }

  sum(x[ok] * w[ok]) / sum(w[ok])
}

inkar <- as_tibble(readRDS(inkar_basis_rds))
metadata <- readRDS(inkar_metadata_rds)
struktur_jahr <- if ("struktur_jahr" %in% names(metadata)) {
  metadata$struktur_jahr[[1]]
} else {
  2023L
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
  filter(
    Raumbezug == "Gemeinden",
    Kuerzel %in% c("xbev", "q_arbeitslosigkeit", "q_kaufkraft")
  ) %>%
  transmute(
    gemeindeschluessel = Kennziffer,
    jahr = Zeitbezug,
    variable = recode(
      Kuerzel,
      xbev = "bevoelkerung",
      q_arbeitslosigkeit = "arbeitslosigkeit",
      q_kaufkraft = "kaufkraft"
    ),
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

kreis_auslaender <- inkar %>%
  filter(
    Raumbezug == "Kreise",
    Kuerzel == "a_ausl_bev"
  ) %>%
  transmute(
    kreis_schluessel = Kennziffer,
    jahr = Zeitbezug,
    auslaenderanteil = Wert
  ) %>%
  group_by(
    kreis_schluessel,
    jahr
  ) %>%
  summarise(
    auslaenderanteil = mean(auslaenderanteil, na.rm = TRUE),
    .groups = "drop"
  )

gemeinde_struktur <- gemeinde_indikatoren %>%
  mutate(
    kreis_schluessel = paste0(substr(gemeindeschluessel, 1, 5), "000")
  ) %>%
  left_join(
    kreis_auslaender,
    by = c("kreis_schluessel", "jahr")
  )

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
    arbeitslosigkeit = weighted_mean_safe(arbeitslosigkeit, bevoelkerung),
    auslaenderanteil = weighted_mean_safe(auslaenderanteil, bevoelkerung),
    kaufkraft = weighted_mean_safe(kaufkraft, bevoelkerung),
    .groups = "drop"
  )

agg_struktur_wide_inner <- agg_struktur_long %>%
  filter(jahr == struktur_jahr) %>%
  transmute(
    agg_schluessel,
    struktur_jahr = .env$struktur_jahr,
    arbeitslosigkeit_2023 = arbeitslosigkeit,
    auslaenderanteil_2023 = auslaenderanteil,
    kaufkraft_2023 = kaufkraft,
    gewicht_summe_2023 = gewicht_summe,
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
    n_missing_arbeitslosigkeit_2023 = sum(is.na(arbeitslosigkeit_2023)),
    n_missing_auslaenderanteil_2023 = sum(is.na(auslaenderanteil_2023)),
    n_missing_kaufkraft_2023 = sum(is.na(kaufkraft_2023)),
    struktur_jahr = .env$struktur_jahr
  )

struktur_missing <- agg_struktur_wide %>%
  filter(
    is.na(arbeitslosigkeit_2023) |
      is.na(auslaenderanteil_2023) |
      is.na(kaufkraft_2023)
  )

saveRDS(agg_struktur_long, file.path(data_dir_cleaned, "vorlaeufig_inkar_agg_long.rds"))
saveRDS(agg_struktur_wide, file.path(data_dir_cleaned, "vorlaeufig_inkar_kovariaten_2023.rds"))
saveRDS(struktur_checks, file.path(data_dir_validation, "vorlaeufig_inkar_agg_checks.rds"))
saveRDS(struktur_missing, file.path(data_dir_validation, "vorlaeufig_inkar_agg_missing.rds"))

write.csv(agg_struktur_long, file.path(data_dir_cleaned, "vorlaeufig_inkar_agg_long.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(agg_struktur_wide, file.path(data_dir_cleaned, "vorlaeufig_inkar_kovariaten_2023.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(struktur_checks, file.path(data_dir_validation, "vorlaeufig_inkar_agg_checks.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(struktur_missing, file.path(data_dir_validation, "vorlaeufig_inkar_agg_missing.csv"), row.names = FALSE, fileEncoding = "UTF-8")
