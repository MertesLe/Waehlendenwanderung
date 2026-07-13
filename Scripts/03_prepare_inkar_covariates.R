library(data.table)
library(dplyr)
library(tidyr)

source("functions.R", encoding = "UTF-8")

dir.create("Data/cleaned", recursive = TRUE, showWarnings = FALSE)

if (!file.exists("Data/cleaned/vorlaeufig_inkar_workflow_rohdaten.rds")) {
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

inkar <- as_tibble(readRDS("Data/cleaned/vorlaeufig_inkar_workflow_rohdaten.rds"))
metadata <- readRDS("Data/cleaned/vorlaeufig_inkar_workflow_metadata.rds")
mapping_gemeinden <- readRDS("Data/cleaned/mapping_gemeinden_final_manuell_validiert.rds")
all_aggs <- readRDS("Data/cleaned/vorlaeufig_nslphom_input_2021.rds") %>%
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

jahr_vorwahl <- metadata$jahr_vorwahl[[1]]
jahr_nachwahl_proxy <- metadata$jahr_nachwahl_proxy[[1]]

agg_struktur_wide_inner <- agg_struktur_long %>%
  filter(jahr %in% c(jahr_vorwahl, jahr_nachwahl_proxy)) %>%
  select(
    agg_schluessel,
    jahr,
    arbeitslosigkeit,
    auslaenderanteil,
    kaufkraft
  ) %>%
  pivot_wider(
    names_from = jahr,
    values_from = c(arbeitslosigkeit, auslaenderanteil, kaufkraft),
    names_glue = "{.value}_{jahr}"
  ) %>%
  mutate(
    delta_arbeitslosigkeit = .data[[paste0("arbeitslosigkeit_", jahr_nachwahl_proxy)]] -
      .data[[paste0("arbeitslosigkeit_", jahr_vorwahl)]],
    delta_auslaenderanteil = .data[[paste0("auslaenderanteil_", jahr_nachwahl_proxy)]] -
      .data[[paste0("auslaenderanteil_", jahr_vorwahl)]],
    delta_kaufkraft = .data[[paste0("kaufkraft_", jahr_nachwahl_proxy)]] -
      .data[[paste0("kaufkraft_", jahr_vorwahl)]]
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
    n_missing_delta_arbeitslosigkeit = sum(is.na(delta_arbeitslosigkeit)),
    n_missing_delta_auslaenderanteil = sum(is.na(delta_auslaenderanteil)),
    n_missing_delta_kaufkraft = sum(is.na(delta_kaufkraft)),
    jahr_vorwahl = jahr_vorwahl,
    jahr_nachwahl_proxy = jahr_nachwahl_proxy
  )

struktur_missing <- agg_struktur_wide %>%
  filter(
    is.na(delta_arbeitslosigkeit) |
      is.na(delta_auslaenderanteil) |
      is.na(delta_kaufkraft)
  )

saveRDS(agg_struktur_long, "Data/cleaned/vorlaeufig_inkar_agg_long.rds")
saveRDS(agg_struktur_wide, "Data/cleaned/vorlaeufig_inkar_agg_delta.rds")
saveRDS(struktur_checks, "Data/cleaned/vorlaeufig_inkar_agg_checks.rds")
saveRDS(struktur_missing, "Data/cleaned/vorlaeufig_inkar_agg_missing.rds")

write.csv(agg_struktur_long, "Data/cleaned/vorlaeufig_inkar_agg_long.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(agg_struktur_wide, "Data/cleaned/vorlaeufig_inkar_agg_delta.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(struktur_checks, "Data/cleaned/vorlaeufig_inkar_agg_checks.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(struktur_missing, "Data/cleaned/vorlaeufig_inkar_agg_missing.csv", row.names = FALSE, fileEncoding = "UTF-8")
