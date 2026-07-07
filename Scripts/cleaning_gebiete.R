library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

pfad <- "Data/raw/gebietsänderungen"

dateien <- list.files(
  pfad,
  pattern = "\\.xlsx$",
  full.names = TRUE
)

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



# Aggregation der zusammengehörigen Gemeindeschlüssel
collapse_keys <- function(x) {
  
  x <- x[!is.na(x) & x != ""]
  
  paste(
    sort(unique(x)),
    collapse = ", "
  )
}

gebiets_edges <- gebietsänderungen %>%
  filter(
    !is.na(AGS_alt),
    !is.na(AGS_neu),
    AGS_alt != "",
    AGS_neu != "",
    AGS_alt != AGS_neu
  ) %>%
  distinct()

listen <- Map(
  function(alt, neu) {
    sort(unique(c(alt, neu)))
  },
  gebiets_edges$AGS_alt,
  gebiets_edges$AGS_neu
)

# Nur tatsächlich überlappende Gruppen transitiv zusammenführen
repeat {
  
  neue_liste <- list()
  bereits_verwendet <- rep(FALSE, length(listen))
  wurde_zusammengefuehrt <- FALSE
  
  for (i in seq_along(listen)) {
    
    # Gruppe wurde bereits einer anderen Zusammenhangsgruppe hinzugefügt
    if (bereits_verwendet[i]) {
      next
    }
    
    aktuelle_gruppe <- listen[[i]]
    bereits_verwendet[i] <- TRUE
    
    # Solange weitere Gruppen mit der aktuellen Gruppe überlappen,
    # wird die aktuelle Gruppe erweitert.
    repeat {
      
      ueberlappende_gruppen <- which(
        !bereits_verwendet &
          vapply(
            listen,
            function(x) {
              length(intersect(aktuelle_gruppe, x)) > 0
            },
            logical(1)
          )
      )
      
      # Keine weitere Überschneidung gefunden
      if (length(ueberlappende_gruppen) == 0) {
        break
      }
      
      # Alle Schlüssel der überlappenden Gruppen aufnehmen
      aktuelle_gruppe <- sort(
        unique(
          c(
            aktuelle_gruppe,
            unlist(
              listen[ueberlappende_gruppen],
              use.names = FALSE
            )
          )
        )
      )
      
      bereits_verwendet[ueberlappende_gruppen] <- TRUE
      wurde_zusammengefuehrt <- TRUE
    }
    
    neue_liste[[length(neue_liste) + 1L]] <- aktuelle_gruppe
  }
  
  listen <- neue_liste
  
  # Sobald in einem kompletten Durchgang keine Gruppen mehr zusammengeführt wurden, stopp.
  if (!wurde_zusammengefuehrt) {
    break
  }
}

agg <- tibble(
  agg.schlüssel = vapply(
    listen,
    collapse_keys,
    character(1)
  )
) %>%
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

