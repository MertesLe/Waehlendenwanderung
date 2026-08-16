###########################################################################
# zu mapping_wahldaten (Checks)



# zusätzlich deskriptives:
bw9 <- data2025 %>%
  filter(
    Bezirksart == 5,
    Gemeinde >= 900
  )

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

## unter Beobachtung: Wahlbezirksauszählungen durch andere Wahlbezirke
table(data2025$Kennziffer.Urnenwahlbezirke.nach...68.BWO == "0000")
# Anzahl Fälle
data2025 %>%
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO != "0000") %>%
  count(Kennziffer.Urnenwahlbezirke.nach...68.BWO)
# Anzahl davon, die tatsächlich nur nullen besitzen
data2025 %>%
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO != "0000") %>%
  mutate(
    leer = Gültige...Zweitstimmen == 0
  ) %>%
  count(leer)



# 2021 Künstliche Gemeinden über Wahlkreis hinaus (3 Ausnahmen) (unterschiedlicher Gemeindename, Wahlkreis)
# erledigt: in bw_groups korrekt getrennt enthalten
data2021 %>%
  filter(Gemeinde >= 900) %>%
  group_by(
    Gemeindeschlüssel,
    Kennziffer.Briefwahlzugehörigkeit
  ) %>%
  summarise(
    n_wahlkreise = n_distinct(Wahlkreis),
    .groups = "drop"
  ) %>%
  filter(n_wahlkreise > 1) %>%
  View()

# Anzahl problematischer Briefwahlbezirke
nrow(bw_groups21)

# Anteil problematischer gemeinsamer Briefwahlbezirke
n_briefwahlbezirke21 <- data2021 %>%
  group_by(Land, Regierungsbezirk, Kreis) %>%
  summarise(
    n_briefwahlgruppen = n_distinct(Kennziffer.Briefwahlzugehörigkeit),
    .groups = "drop"
  ) %>%
  summarise(
    summe_briefwahlgruppen = sum(n_briefwahlgruppen)
  )
nrow(bw_groups21)/n_briefwahlbezirke21[["summe_briefwahlgruppen"]]

# Anzahl betroffener gemeinden
sum(bw_groups21$n_gemeinden)

# Anzahl echter Gemeinden (100 Gemeinden zu wenig als offiziell!!)
n_gemeinden21 <- data2021 %>%
  filter(Gemeinde < 900) %>%
  distinct(Gemeindeschlüssel) %>%
  nrow()

# Anteil betroffener Gemeinden
sum(bw_groups21$n_gemeinden) / n_gemeinden21


## unter Beobachtung: Wahlbezirksauszählungen durch andere Wahlbezirke
table(data2021$Kennziffer.Urnenwahlbezirke.nach...68.BWO == "0000")
# Anzahl Fälle
data2021 %>%
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO != "0000") %>%
  count(Kennziffer.Urnenwahlbezirke.nach...68.BWO)
# Anzahl davon, die tatsächlich nur nullen besitzen
data2021 %>%
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO != "0000") %>%
  mutate(
    leer = Z_Gültige == 0
  ) %>%
  count(leer)






# Abchecken: einheitliche Gruppierungen der Wahlgebiete von 2021 zu 2025

mapping21_echt <-
  mapping21 %>%
  filter(substr(Gemeindeschlüssel,
                nchar(Gemeindeschlüssel)-2,
                nchar(Gemeindeschlüssel)-2) != "9")

mapping25_echt <-
  mapping25 %>%
  filter(substr(Gemeindeschlüssel,
                nchar(Gemeindeschlüssel)-2,
                nchar(Gemeindeschlüssel)-2) != "9")

anti_join( # Fehlende Gemeinden: 57 fehlend im Vergleich zu 21 (nrow(mapping25_echt) = 10721)
  mapping21_echt,
  mapping25_echt,
  by="Gemeindeschlüssel"
) %>%
  nrow()

anti_join( # Zusätzliche Gemeinden: 91 zusätzlich im Vergleich zu 21 (nrow(mapping25_echt) = 10721)
  mapping25_echt,
  mapping21_echt,
  by="Gemeindeschlüssel"
) %>%
  nrow()


# Aggregationen prüfen
vergleich <-
  mapping21 %>%
  select(
    Gemeindeschlüssel,
    agg21 = agg.schlüssel,
    Wahlkreis
  ) %>%
  inner_join(
    mapping25 %>%
      select(
        Gemeindeschlüssel,
        agg25 = agg.schlüssel,
        Wahlkreis
      ),
    by= c("Gemeindeschlüssel", "Wahlkreis")
  )

vergleich %>%
  filter(agg21 != agg25) %>% # klappt da Gemeindeschlüssel in string sortiert wurden
  View()



## checks
# Sind die einzigartigen Aggregationen identisch?
agg21 <- unique(mapping21_new$agg.schlüssel)
agg25 <- unique(mapping25_new$agg.schlüssel)
setequal(agg21, agg25)
setdiff(agg21, agg25) %>%
  View()
setdiff(agg25, agg21)

# Differenzierende betrachten
diff21 <- setdiff(
  unique(mapping21_new$agg.schlüssel),
  unique(mapping25_new$agg.schlüssel)
)
diff25 <- setdiff(
  unique(mapping25_new$agg.schlüssel),
  unique(mapping21_new$agg.schlüssel)
)
mapping21_new %>%
  filter(agg.schlüssel %in% diff21) %>%
  arrange(agg.schlüssel) %>%
  View()
mapping25_new %>%
  filter(agg.schlüssel %in% diff25) %>%
  arrange(agg.schlüssel) %>%
  View()

# Test auf unterschiedliche Reihenfolge in character-IDs
normalize <- function(x) {
  paste(
    sort(unique(strsplit(x, ",\\s*")[[1]])),
    collapse = ", "
  )
}
mapping21_test <- mapping21_new %>%
  mutate(agg_norm = sapply(agg.schlüssel, normalize))
mapping25_test <- mapping25_new %>%
  mutate(agg_norm = sapply(agg.schlüssel, normalize))
setequal(
  unique(mapping21_test$agg_norm),
  unique(mapping25_test$agg_norm)
)

# Konsistenzcheck (eindeutiger künstlicher gemeindecode check) (passt da 0 zeilen)
mapping21_new %>%
  group_by(Wahlkreis, Gemeindeschlüssel) %>%
  summarise(
    n = n_distinct(agg.schlüssel),
    .groups = "drop"
  ) %>%
  filter(n > 1)
mapping25_new %>%
  group_by(Wahlkreis, Gemeindeschlüssel) %>%
  summarise(
    n = n_distinct(agg.schlüssel),
    .groups = "drop"
  ) %>%
  filter(n > 1)

# Hat jede echte Gemeinde dieselbe Aggregation? Antwort: Ja
vergleich <-
  mapping21_new %>%
  filter(Gemeinde < 900) %>%
  select(
    Gemeindeschlüssel,
    Wahlkreis,
    agg21 = agg.schlüssel
  ) %>%
  inner_join(
    mapping25_new %>%
      filter(Gemeinde < 900) %>%
      select(
        Gemeindeschlüssel,
        Wahlkreis,
        agg25 = agg.schlüssel
      ),
    by = c("Gemeindeschlüssel", "Wahlkreis")
  )
vergleich %>%
  filter(agg21 != agg25)

# Kommt ein Gemeindeschlüssel in mehreren Aggregationen vor? Antwort: Nein
lookup <-
  mapping21_new %>%
  distinct(Wahlkreis, agg.schlüssel) %>%
  tidyr::separate_rows(
    agg.schlüssel,
    sep = ",\\s*"
  ) %>%
  rename(
    Gemeindeschlüssel = agg.schlüssel
  )
lookup %>%
  count(
    Wahlkreis,
    Gemeindeschlüssel
  ) %>%
  filter(n > 1)

################################################################################