library(dplyr)

data2025 <- read.csv("Data//btw25_wbz//btw25_wbz_ergebnisse.csv", header = TRUE, sep = ";", skip = 4)
View(data2025)

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
  ) %>% 
  View( )

# beteiligte Gemeinden: 9xx-Datensatz mit den normalen Gemeinden desselben Kreises und derselben Briefwahlzugehörigkeit verbinden
kgemeinden <- data2025 %>%
  filter(Gemeinde >= 900) %>%
  distinct(
    Land,
    Regierungsbezirk,
    Kreis,
    Kennziffer.Briefwahlzugehörigkeit,
    Gemeinde,
    Gemeindename
  )

egemeinden <- echt <- data2025 %>%
  filter(Gemeinde < 900) %>%
  group_by(Land, Regierungsbezirk, Kreis, Kennziffer.Briefwahlzugehörigkeit) %>%
  summarise(
    n_gemeinden = n_distinct(Gemeinde),
    gemeinden = paste(sort(unique(Gemeindename)), collapse = ", "),
    gemeindecodes = paste(sort(unique(Gemeinde)), collapse = ", "),
    .groups = "drop"
  )

bw_groups <- kgemeinden %>%
  left_join(
    egemeinden,
    by = c("Land", "Regierungsbezirk", "Kreis", "Kennziffer.Briefwahlzugehörigkeit")
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
  distinct(Land, Regierungsbezirk, Kreis, Gemeinde, Gemeindename) %>%
  group_by(Land, Regierungsbezirk, Kreis) %>%
  summarise(
    n_gemeinden = n(),
    .groups = "drop"
  ) %>%
  summarise(
    summe_gemeinden = sum(n_gemeinden)
  )

# Anteil betroffener Gemeinden
sum(
  bw_groups$n_gemeinden
) / n_gemeinden[["summe_gemeinden"]]

# Auffälligkeiten: große gruppen
data2025 %>%
  filter(
    Land == 7,
    Regierungsbezirk == 1,
    Kreis == 35,
    Kennziffer.Briefwahlzugehörigkeit == "01"
  ) %>% 
  View()

# Briefwahlergebnis für "künstliche Gemeinde" anschauen
data2025 %>% 
  filter(
    Gemeindename == "Briefwahl VG Cochem"
  ) %>% 
  View()
# --> nicht nur ein Ergebnis! unterschiedlich in "Wahlbezirk" --> bw_groups_new
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

egemeinden <- echt <- data2025 %>%
  filter(Gemeinde < 900) %>%
  group_by(Land, Regierungsbezirk, Kreis, Kennziffer.Briefwahlzugehörigkeit) %>%
  summarise(
    n_gemeinden = n_distinct(Gemeinde),
    gemeinden = paste(sort(unique(Gemeindename)), collapse = ", "),
    gemeindecodes = paste(sort(unique(Gemeinde)), collapse = ", "),
    .groups = "drop"
  )

bw_groups_new <- kgemeinden %>%
  left_join(
    egemeinden,
    by = c("Land", "Regierungsbezirk", "Kreis", "Kennziffer.Briefwahlzugehörigkeit")
  )

# Künstliche Gemeinden über Wahlkreis hinaus (1 Ausnahme:)
data2025 %>%
  filter(Gemeinde >= 900) %>%
  group_by(
    Land,
    Regierungsbezirk,
    Kreis,
    Kennziffer.Briefwahlzugehörigkeit,
    Gemeinde
  ) %>%
  summarise(
    n_wahlkreise = n_distinct(Wahlkreis),
    .groups = "drop"
  ) %>%
  filter(n_wahlkreise > 1)

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
