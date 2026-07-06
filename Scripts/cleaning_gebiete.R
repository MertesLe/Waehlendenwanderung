library(readxl)
library(dplyr)
library(stringr)
library(purrr)

pfad <- "Data/gebietsänderungen"

dateien <- list.files(
  pfad,
  pattern = "\\.xlsx$",
  full.names = TRUE
)

# Zeitraum der Analyse
start <- as.Date("2021-09-26") # Bundestagswahl 2021
ende  <- as.Date("2025-02-23") # Bundestagswahl 2025

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
      
      AGS_alt = as.character(AGS_alt),
      AGS_neu = as.character(AGS_neu)
      
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
