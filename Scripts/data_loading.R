library(dplyr)
library(stringr)
library(tidyr)

data2025 <- read.csv("Data//btw25_wbz//btw25_wbz_ergebnisse.csv", header = TRUE, sep = ";", skip = 4)
View(data2025)
data2021 <- read.csv("Data//btw21_wbz//btw21_wbz_ergebnisse.csv", header = TRUE, sep = ";")
View(data2021)
inkartest <- read.csv("Data//inkar_2025//inkar_2025.csv", header = TRUE, sep = ";")
View(inkartest)

library(data.table)
# set cores used
setDTthreads(max(1, parallel::detectCores() - 2))
inkar_test <- as.data.table(read.csv("Data//inkar_2025//inkar_2025.csv", header = TRUE, sep = ";"))
inkar_daten <- inkar_test[Zeitbezug %in% c(2021, 2025)]
inkar_daten <- inkar_daten[Raumbezug %in% "Gemeinden"]
# save as RDS
saveRDS(inkar_daten, file = "Data//cleaned//data_inkar.rds")

## Auffälligkeiten Inkardaten
# 1. Bedeutung von ID(uniqueN = 249), Kuerzel (uniqueN = 221), Indikator (uniqueN = 234)


# Amtlichen Gemeindeschlüssel (als Key für spätere Joins) generieren:
# _ _ (Bundesland) _ (Regierungsbezirk bzw 0) _ _ (Landkreis bzw kreisfreie Stadt) _ _ _ (Gemeinde bzw. 000)
data2025 <- data2025 %>% 
  mutate(
    Gemeindeschlüssel = paste0(
      str_pad(Land, width = 2, pad = "0"),
      str_pad(Regierungsbezirk, width = 1, pad = "0"),
      str_pad(Kreis, width = 2, pad = "0"),
      str_pad(Gemeinde, width = 3, pad = "0")
    )
  )

data2025 <- data2025 %>% 
  mutate(
    Gemeindeschlüssel = paste0(
      str_pad(Land, width = 2, pad = "0"),
      str_pad(Regierungsbezirk, width = 1, pad = "0"),
      str_pad(Kreis, width = 2, pad = "0"),
      str_pad(Gemeinde, width = 3, pad = "0")
    )
  )

## gemeinsame Briefwahlbezirke unterschiedlicher Gemeinden untersuchen
## neuer Ansatz: alle künstlichen Gemeinden (mit 9 vorne) sind keine echten Gemeinden sondern zusammengelegte Briefwahlbezirke über Gemeindegrenzen hinaus
bw9 <- data2025 %>%
  filter(Bezirksart == 5,
         Gemeinde >= 900)

# Wie viele gemeinsame Briefwahlbezirke gibt es?
nrow(
  bw9 %>%
    distinct(Gemeindeschlüssel)
)

# Tabelle aller gemeinsamen Briefwahlbezirke
bw9 %>%
  distinct(
    Gemeindeschlüssel,
    Kennziffer.Briefwahlzugehörigkeit
  ) %>% 
  View()

# Künstliche Gemeinden über Wahlkreis hinaus (1 Ausnahme:) (unterschiedlicher Gemeindename)
# erledigt: in bw_groups korrekt getrennt enthalten
data2025 %>%
  filter(Gemeinde >= 900) %>%
  group_by(
    Gemeindeschlüssel,
    Kennziffer.Briefwahlzugehörigkeit
  ) %>%
  summarise(
    n_wahlkreise = n_distinct(Wahlkreis),
    .groups = "drop"
  ) %>%
  filter(n_wahlkreise > 1)

data2025 %>% 
  filter(Gemeindeschlüssel == "01053991") %>% 
  View()

# beteiligte Gemeinden: 9xx-Datensatz mit den normalen Gemeinden desselben Kreises 
# und derselben Briefwahlzugehörigkeit verbinden
# --> nicht nur ein Ergebnis pro Gemeinde! unterschiedlich in "Wahlbezirk" --> interessierend nur Gemeinden
# weil darüber aggregiert wird. Die Wahlbezirke für dieselbe Gemeinde müssen summiert werden

kgemeinden <- data2025 %>%
  filter(Gemeinde >= 900) %>%
  distinct(
    Land,
    Regierungsbezirk,
    Kreis,
    Wahlkreis,
    Kennziffer.Briefwahlzugehörigkeit,
    Gemeindeschlüssel,
    Gemeindename
  )

egemeinden <- data2025 %>%
  filter(Gemeinde < 900) %>%
  group_by(Land, Regierungsbezirk, Kreis, Wahlkreis, Kennziffer.Briefwahlzugehörigkeit) %>%
  summarise(
    n_gemeinden = n_distinct(Gemeinde),
    gemeinden = paste(sort(unique(Gemeindename)), collapse = ", "),
    gemeindecodes = paste(sort(unique(Gemeindeschlüssel)), collapse = ", "),
    .groups = "drop"
  )

bw_groups <- kgemeinden %>%
  left_join(
    egemeinden,
    by = c("Land", "Regierungsbezirk", "Kreis", "Wahlkreis", "Kennziffer.Briefwahlzugehörigkeit")
  )


# Anzahl problematischer Briefwahlbezirke
nrow(bw_groups)

# Anteil problematischer gemeinsamer Briefwahlbezirke
n_briefwahlbezirke <- data2025 %>%
  group_by(Land, Regierungsbezirk, Kreis) %>%
  summarise(
    n_briefwahlgruppen = n_distinct(Kennziffer.Briefwahlzugehörigkeit),
    .groups = "drop"
  ) %>%
  summarise(
    summe_briefwahlgruppen = sum(n_briefwahlgruppen)
  )
nrow(bw_groups)/n_briefwahlbezirke[["summe_briefwahlgruppen"]]

# Anzahl betroffener gemeinden 
sum(bw_groups$n_gemeinden)

# Anzahl echter Gemeinden (100 Gemeinden zu wenig als offiziell!!)
n_gemeinden <- data2025 %>%
  filter(Gemeinde < 900) %>%
  distinct(Gemeindeschlüssel) %>%
  nrow()

# Anteil betroffener Gemeinden
sum(bw_groups$n_gemeinden) / n_gemeinden

# wenn leerer output dann gemeinden eindeutig identifiziert
data2025 %>%
  filter(Gemeinde < 900) %>%
  group_by(Land, Regierungsbezirk, Kreis, Gemeinde) %>%
  summarise(
    n_namen = n_distinct(Gemeindename),
    .groups = "drop"
    ) %>%
  filter(n_namen > 1)

## unter Beobachtung: Wahlbezirksauszählungen durch andere Wahlbezirke
table(data2025$Kennziffer.Urnenwahlbezirke.nach...68.BWO == "")
# Anzahl Fälle
data2025 %>%
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO != "") %>%
  count(Kennziffer.Urnenwahlbezirke.nach...68.BWO)
# Anzahl davon, die tatsächlich nur nullen besitzen
data2025 %>%
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO != "") %>%
  mutate(
    leer = Gültige...Zweitstimmen == 0
  ) %>%
  count(leer)

# Untersuchung, ob jeweils shift IMMER nur innerhalb der Gemeinde (wenn überall n_gemeinden == 1, dann wahr)
par68 <- data2025 %>%
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO != "0000") %>%
  group_by(Kennziffer.Urnenwahlbezirke.nach...68.BWO) %>%
  summarise(
    n_gemeinden = n_distinct(Gemeinde),
    gemeinden = paste(unique(Gemeindename), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(innerhalb_gemeinde = n_gemeinden == 1)
# In Ordnung, zwar nicht immer TRUE, aber eigentlich (in Worten angegeben) enthalten




# Bundestagswahl 2025 aggregieren
kgemeinden <- data2025 %>%
  filter(Gemeinde >= 900) %>%
  distinct(
    Land,
    Regierungsbezirk,
    Kreis,
    Kennziffer.Briefwahlzugehörigkeit,
    Gemeinde,
    Gemeindename,
    Wahlbezirk
  )

egemeinden <- data2025 %>%
  filter(Gemeinde < 900) %>%
  group_by(
    Land,
    Regierungsbezirk,
    Kreis,
    Kennziffer.Briefwahlzugehörigkeit
  ) %>%
  summarise(
    n_gemeinden = n_distinct(Gemeinde),
    gemeinden = paste(sort(unique(Gemeindename)), collapse = ", "),
    gemeindecodes = paste(sort(unique(Gemeinde)), collapse = ", "),
    gemeindeschluessel = paste(sort(unique(Gemeindeschlüssel)), collapse = ", "),
    .groups = "drop"
  )

bw_groups_new <- kgemeinden %>%
  left_join(
    egemeinden,
    by = c("Land", "Regierungsbezirk", "Kreis", "Kennziffer.Briefwahlzugehörigkeit")
  )

###############################################################################
# 1) §68-Fälle: In Auszählung aggregierte Gemeinden erfassen
###############################################################################
# Zuordnung bestimmen
par68_groups <-
  data2025 %>%
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO != "0000")

ziel <- par68_groups %>%
  filter(Wählende..B. > 0) %>%
  distinct(
    Kennziffer.Urnenwahlbezirke.nach...68.BWO,
    zielschluessel = Gemeindeschlüssel,
    zielgemeinde = Gemeindename
  )

quelle <- par68_groups %>%
  filter(Wählende..B. == 0) %>%
  distinct(
    Kennziffer.Urnenwahlbezirke.nach...68.BWO,
    quellschluessel = Gemeindeschlüssel,
    quellgemeinde = Gemeindename
  )

par68_mapping <-
  quelle %>%
  left_join(
    ziel,
    by="Kennziffer.Urnenwahlbezirke.nach...68.BWO"
  )

data_clean <-
  data2025 %>%
  left_join(
    par68_mapping %>%
      select(quellschluessel, zielschluessel),
    by=c("Gemeindeschlüssel"="quellschluessel")
  ) %>%
  mutate(
    AnalyseGemeinde =
      if_else(
        is.na(zielschluessel),
        Gemeindeschlüssel,
        zielschluessel
      )
  ) %>%
  filter(
    !(
      Kennziffer.Urnenwahlbezirke.nach...68.BWO != "0000" &
        Wählende..B. == 0
    )
  )

###############################################################################
# 2) Mapping: jede Gemeinde -> endgültige Analyseeinheit
###############################################################################

## Mapping gemeinsamer Briefwahlgruppen
egemeinden <-
  data_clean %>%
  filter(Gemeinde < 900) %>%
  group_by(
    Land,
    Regierungsbezirk,
    Kreis,
    Kennziffer.Briefwahlzugehörigkeit
  ) %>%
  summarise(
    
    n_gemeinden =
      n_distinct(AnalyseGemeinde),
    
    gemeindeschluessel =
      paste(
        sort(unique(AnalyseGemeinde)),
        collapse=", "
      ),
    
    gemeinden =
      paste(
        sort(unique(Gemeindename)),
        collapse=", "
      ),
    
    .groups="drop"
  )

bw_groups <-
  kgemeinden %>%
  left_join(
    egemeinden,
    by=c(
      "Land",
      "Regierungsbezirk",
      "Kreis",
      "Kennziffer.Briefwahlzugehörigkeit"
    )
  )

mapping_bw <-
  bw_groups %>%
  rowwise() %>%
  mutate(
    Analysegruppe =
      paste(
        sort(strsplit(gemeindeschluessel, ",\\s*")[[1]]),
        collapse = "_"
      )
  ) %>%
  ungroup() %>%
  select(
    Analysegruppe,
    gemeindeschluessel
  ) %>%
  separate_rows(
    gemeindeschluessel,
    sep = ",\\s*"
  ) %>%
  rename(
    Analyse_ID = gemeindeschluessel
  ) %>% 
  distinct()

###############################################################################
# 3) Mapping an Wahldaten anhängen
###############################################################################

data_clean <-
  data_clean %>%
  left_join(
    mapping_bw,
    by=c(
      "AnalyseGemeinde"="Analyse_ID"
    )
  ) %>%
  mutate(
    
    Analysegruppe =
      if_else(
        is.na(Analysegruppe),
        AnalyseGemeinde,
        Analysegruppe
      )
  )


###############################################################################
# 5) Aggregation auf endgültige Analyseeinheiten
###############################################################################
num_cols <- names(data_clean)[sapply(data_clean, is.numeric)]

num_cols <- setdiff(
  num_cols,
  c(
    "Wahlkreis",
    "Land",
    "Regierungsbezirk",
    "Kreis",
    "Verbandsgemeinde",
    "Gemeinde",
    "Bezirksart"
  )
)


wahldaten_gemeinde <-
  data_clean %>%
  group_by(Analysegruppe) %>%
  summarise(
    
    Gemeinden =
      paste(
        sort(unique(Gemeindename)),
        collapse=", "
      ),
    
    Gemeindeschluessel =
      paste(
        sort(unique(Gemeindeschlüssel)),
        collapse=", "
      ),
    
    AnalyseGemeinden =
      paste(
        sort(unique(AnalyseGemeinde)),
        collapse=", "
      ),
    
    n_gemeinden =
      n_distinct(Gemeindeschlüssel),
    
    across(
      all_of(num_cols),
      ~sum(.x, na.rm = TRUE)
    ),
    
    .groups="drop"
  )

###############################################################################
# Mapping-Tabelle erzeugen
###############################################################################
mapping_final <-
  data_clean %>%
  distinct(
    Gemeindeschlüssel,
    Gemeindename,
    AnalyseGemeinde
  )

mapping_briefwahl <-
  bw_groups %>%
  mutate(
    Analysegruppe = gsub(",\\s*", "_", gemeindeschluessel)
  ) %>%
  select(
    Analysegruppe,
    gemeindeschluessel
  ) %>%
  separate_rows(
    gemeindeschluessel,
    sep = ",\\s*"
  ) %>%
  rename(
    AnalyseGemeinde = gemeindeschluessel
  )

mapping_final <-
  mapping_final %>%
  left_join(
    mapping_briefwahl,
    by = "AnalyseGemeinde"
  ) %>%
  mutate(
    Analysegruppe =
      if_else(
        is.na(Analysegruppe),
        AnalyseGemeinde,
        Analysegruppe
      )
  )

# Bundestagswahl 2025 abspeichern

# Bereinigung Bundestagswahl 2021

# Abchecken: einheitliche Gruppierungen der Wahlgebiete von 2021 zu 2025

# Abspeichern Bundestagswahl 2021