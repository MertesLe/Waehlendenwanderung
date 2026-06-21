library(dplyr)

data2025 <- read.csv("Data//btw25_wbz//btw25_wbz_ergebnisse.csv", header = TRUE, sep = ";", skip = 4)
View(data2025)

## gemeinsame Briefwahlbezirke unterschiedlicher Gemeinden untersuchen
# Wie viele verschiedene Gemeinden besitzen dieselbe EF8?
bw_problem <- data2025 %>%
  filter(Bezirksart == 5,
         Kennziffer.Briefwahlzugehörigkeit != "00") %>%
  group_by(Kennziffer.Briefwahlzugehörigkeit) %>%
  summarise(
    n_gemeinden = n_distinct(Gemeinde),
    gemeinden = paste(sort(unique(Gemeindename)), collapse = ", ")
  ) %>%
  arrange(desc(n_gemeinden))

bw_problem

# zusammengelegte Gemeinden
bw_problem %>%
  filter(n_gemeinden > 1)

# Anzahl problematischer Briefwahlbezirke
sum(bw_problem$n_gemeinden > 1)

# Anteil problematischer Briefwahlbezirke an allen Briefwahlbezirken
mean(bw_problem$n_gemeinden > 1)

# Anteil betroffener Gemeinden 
betroffene_gemeinden <- bw_problem %>%
  filter(n_gemeinden > 1) %>%
  summarise(
    betroffene_gemeinden = sum(n_gemeinden)
  ) %>%
  pull(betroffene_gemeinden)
betroffene_gemeinden/n_distinct(data2025$Gemeinde)

# betroffene Gemeinden betrachten in den Rohdaten
data2025 %>% 
  filter(Kennziffer.Briefwahlzugehörigkeit == 52) %>% 
  View()

# Welche Briefwahlgruppen bestehen aus mehreren Gemeinden?
data2025 %>%
  filter(Bezirksart == 5) %>%
  group_by(Kennziffer.Briefwahlzugehörigkeit) %>%
  summarise(
    n_gemeinden = n_distinct(Gemeinde),
    gemeinden = paste(unique(Gemeindename), collapse = ", ")
  ) %>%
  arrange(desc(n_gemeinden))

# Wie viele Gemeinden sind betroffen?
data2025 %>%
  filter(Bezirksart == 5) %>%
  group_by(Kennziffer.Briefwahlzugehörigkeit) %>%
  filter(n_distinct(Gemeinde) > 1) %>%
  summarise(
    gemeinden = n_distinct(Gemeinde)
  ) %>%
  summarise(
    anzahl_betroffene_gemeinden = sum(gemeinden)
  )


## gemeinsame Briefwahlbezirke unterschiedlicher Gemeinden untersuchen
## neuer Ansatz: alle künstlichen Gemeinden (mit 9 vorne) sind keine echten Gemeinden sondern zusammengelegte Briefwahlbezirke über Gemeindegrenzen hinaus
bw9 <- data2025 %>%
  filter(Bezirksart == 5,
         Gemeinde >= 900)

# Wie viele gemeinsame Briefwahlbezirke gibt es?
nrow(
  bw9 %>%
    distinct(Land, Regierungsbezirk, Kreis, Gemeinde)
)

# Tabelle aller gemeinsamen Briefwahlbezirke
bw9 %>%
  distinct(
    Land,
    Regierungsbezirk,
    Kreis,
    Gemeinde,
    Kennziffer.Briefwahlzugehörigkeit,
    Gemeindename
  )

# beteiligte Gemeinden: 9xx-Datensatz mit den normalen Gemeinden desselben Kreises und derselben EF8 verbinden
bw_groups <-
  data2025 %>%
  filter(Bezirksart == 5,
         Kennziffer.Briefwahlzugehörigkeit != "00") %>%
  group_by(
    Land,
    Regierungsbezirk,
    Kreis,
    Kennziffer.Briefwahlzugehörigkeit
  ) %>%
  summarise(
    
    n_gemeinden =
      n_distinct(Gemeinde[Gemeinde < 900]),
    
    gemeinden =
      paste(
        sort(unique(
          Gemeindename[Gemeinde < 900]
        )),
        collapse = ", "
      ),
    
    gemeinsame_gemeinde =
      unique(Gemeinde[Gemeinde >= 900]),
    
    .groups="drop"
  )

# Wie viele verschiedene Gemeinden besitzen dieselbe EF8?
bw_groups %>%
  select(
    gemeinsame_gemeinde,
    n_gemeinden,
    gemeinden
  )

# Zusammengelegte Gemeinden
bw_groups %>%
  filter(n_gemeinden > 1)

# Anzahl problematischer Briefwahlbezirke
sum(bw_groups$n_gemeinden > 1)

# Anteil problematischer gemeinsamer Briefwahlbezirke
mean(bw_groups$n_gemeinden > 1)

# Anzahl betroffener gemeinden 
sum(bw_groups$n_gemeinden[bw_groups$n_gemeinden > 1])

# Anteil betroffener Gemeinden
sum(
  bw_groups$n_gemeinden[
    bw_groups$n_gemeinden > 1
  ]
) /
  n_distinct(
    data2025$Gemeinde[
      data2025$Gemeinde < 900
    ]
  )

# Anzahl betroffener Gemeinden 
bw_groups %>%
  filter(n_gemeinden > 1) %>%
  summarise(
    betroffene_gemeinden =
      sum(n_gemeinden)
  )

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
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO != "") %>% 
  count(Kennziffer.Urnenwahlbezirke.nach...68.BWO) %>% 
  group_by(Kennziffer.Urnenwahlbezirke.nach...68.BWO) %>%
  summarise(
    n_gemeinden = n_distinct(Gemeinde),
    gemeinden = paste(unique(Gemeindename), collapse = ", ")
  ) %>%
  ifelse(n_gemeinden == 1, TRUE, FALSE)

# Dokumentation gegen die tatsächlichen Daten zu validieren (Einträge immer null, wenn abgebender Wahlbezirk)
# Betroffene Wahlbezirke (grundsätzlich n = 2, Ausnahmen möglich)
par68 %>%
  group_by(Kennziffer.Urnenwahlbezirke.nach...68.BWO) %>%
  summarise(
    n = n()
  )
# Innerhalb jeder Gruppe untersuchen, wie viele Null-Bezirke existieren (wichtig hier: Nicht-Null Bezirke in der Anzahl pro Gruppe immer nur = 1)
par68_check <- par68 %>%
  mutate(
    alle_null =
      Wählende..B. == 0 &
      Gültige...Erststimmen == 0 &
      Gültige...Zweitstimmen == 0
  )
par68_check %>%
  group_by(Kennziffer.Urnenwahlbezirke.nach...68.BWO) %>%
  summarise(
    anzahl_null = sum(alle_null),
    anzahl_nicht_null = sum(!alle_null),
    n = n()
  )
# Verdächtige Gruppen prüfen:
par68_check %>%
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO == "003")