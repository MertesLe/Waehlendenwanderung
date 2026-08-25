# Amtliche Gemeindegebietsveraenderungen einlesen und zu Aggregationsgruppen verbinden.

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

source("Functions/general_functions.R", encoding = "UTF-8")
source("paths.R", encoding = "UTF-8")

ensure_data_dirs()

pfad <- getOption(
  "waehlendenwanderung.gebietsaenderungen_path",
  "Data/raw/gebiets\u00e4nderungen"
)

dateien <- list.files(
  pfad,
  pattern = "\\.xlsx$",
  full.names = TRUE
)
dateien <- dateien[!startsWith(basename(dateien), "~$")]

# Zeitraum der Analyse (Zeitpunkt: Gebietsstand als Wahlgrundlage)
start <- as.Date("2021-06-30") # Bundestagswahl 2021
ende  <- as.Date("2024-12-31") # Bundestagswahl 2025

# Eine Jahresdatei der Gebietsveraenderungen einlesen und standardisieren.
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
      Flaeche = as.numeric(Flaeche),
      Einwohner = as.numeric(Einwohner),
      
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
      Flaeche,
      Einwohner,
      AGS_neu,
      Name_neu
    )
}

# Alle Jahre einlesen
gebietsaenderungen <-
  map_dfr(
    dateien,
    read_changes
  ) %>%
  distinct() %>%
  arrange(Datum)

gebiets_edges <- gebietsaenderungen %>%
  filter(
    !is.na(AGS_alt),
    !is.na(AGS_neu),
    AGS_alt != "",
    AGS_neu != "",
    AGS_alt != AGS_neu
  ) %>%
  distinct() %>%
  mutate(
    agg_schluessel = purrr::map2_chr(
      AGS_alt,
      AGS_neu,
      ~ collapse_keys(c(.x, .y))
    )
  )

# Gerichtete Alt-neu-Beziehungen fuer spaetere raeumliche Zuordnungen erhalten.
saveRDS(
  gebiets_edges %>%
    select(
      Datum,
      Typ,
      AGS_alt,
      Name_alt,
      Flaeche,
      Einwohner,
      AGS_neu,
      Name_neu
    ),
  file = file.path(
    data_dir_cleaned,
    "mapping_gebietsaenderungen_gerichtet.rds"
  )
)

agg <- connected_components(gebiets_edges$agg_schluessel) %>%
  rename(
    agg_schluessel = all_of("agg.schl\u00fcssel")
  ) %>%
  distinct() %>%
  arrange(agg_schluessel)

mapping_gebietsaenderungen <- agg %>%
  mutate(
    gemeindeschluessel = strsplit(
      agg_schluessel,
      ",\\s*"
    )
  ) %>%
  unnest(gemeindeschluessel) %>%
  select(
    gemeindeschluessel,
    agg_schluessel
  ) %>%
  arrange(gemeindeschluessel) %>%
  rename(
    !!"Gemeindeschl\u00fcssel" := gemeindeschluessel,
    !!"agg.schl\u00fcssel" := agg_schluessel
  )

saveRDS(
  mapping_gebietsaenderungen,
  file = file.path(data_dir_cleaned, "mapping_gebietsaenderungen.rds")
)
