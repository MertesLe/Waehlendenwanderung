library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

source("functions.R", encoding = "UTF-8")

pfad <- "Data/raw/gebietsänderungen"

dateien <- list.files(
  pfad,
  pattern = "\\.xlsx$",
  full.names = TRUE
)
dateien <- dateien[!startsWith(basename(dateien), "~$")]

# Zeitraum der Analyse (Zeitpunkt: Gebietsstand als Wahlgrundlage)
start <- as.Date("2021-06-30") # Bundestagswahl 2021
ende  <- as.Date("2024-12-31") # Bundestagswahl 2025

read_changes <- function(datei){
  
  dat <-
    read_excel(
      datei,
      sheet = 2,
      skip = 2,
      col_names = FALSE
    )
  
  names(dat) <-
    c(
      "Kennziffer",
      "Regionaleinheit",
      "ARS_alt",
      "AGS_alt",
      "Name_alt",
      "Typ",
      "Flaeche",
      "Einwohner",
      "ARS_neu",
      "AGS_neu",
      "Name_neu",
      "juristisch",
      "statistisch"
    )
  
  dat %>%
    filter(
      Regionaleinheit == "Gemeinde"
    ) %>%
    mutate(
      
      Typ = as.integer(Typ),
      
      juristisch =
        as.Date(
          juristisch,
          format = "%d.%m.%Y"
        ),
      
      AGS_alt = normalize_ags(AGS_alt),
      AGS_neu = normalize_ags(AGS_neu)
      
    ) %>%
    filter(
      juristisch >= start,
      juristisch < ende
    ) %>%
    filter(
      Typ %in% c(1,2,3)
    ) %>%
    filter(
      Typ != 3 | AGS_alt != AGS_neu
    ) %>% 
    select(
      Datum = juristisch,
      Typ,
      AGS_alt,
      Name_alt,
      AGS_neu,
      Name_neu
    )
}

# Alle Jahre einlesen
gebietsänderungen <-
  map_dfr(
    dateien,
    read_changes
  ) %>%
  distinct() %>%
  arrange(Datum)

gebiets_edges <- gebietsänderungen %>%
  filter(
    !is.na(AGS_alt),
    !is.na(AGS_neu),
    AGS_alt != "",
    AGS_neu != "",
    AGS_alt != AGS_neu
  ) %>%
  distinct() %>%
  mutate(
    agg.schlüssel = purrr::map2_chr(
      AGS_alt,
      AGS_neu,
      ~ collapse_keys(c(.x, .y))
    )
  )

agg <- connected_components(gebiets_edges$agg.schlüssel) %>%
  distinct() %>%
  arrange(agg.schlüssel)

mapping_gebietsänderungen <- agg %>%
  mutate(
    Gemeindeschlüssel = strsplit(
      agg.schlüssel,
      ",\\s*"
    )
  ) %>%
  unnest(Gemeindeschlüssel) %>%
  select(
    Gemeindeschlüssel,
    agg.schlüssel
  ) %>%
  arrange(Gemeindeschlüssel)

dir.create("Data/cleaned", recursive = TRUE, showWarnings = FALSE)
saveRDS(
  mapping_gebietsänderungen,
  file = "Data/cleaned/mapping_gebietsaenderungen.rds"
)
