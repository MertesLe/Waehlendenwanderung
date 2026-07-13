library(dplyr)
library(stringr)
library(tidyr)

source("functions.R", encoding = "UTF-8")
source("paths.R", encoding = "UTF-8")

ensure_data_dirs()

data2025 <- read.csv("Data//raw//btw25_wbz//btw25_wbz_ergebnisse.csv", header = TRUE, sep = ";", skip = 4)
data2021 <- read.csv("Data//raw//btw21_wbz//btw21_wbz_ergebnisse.csv", header = TRUE, sep = ";")

data2025 <- drop_empty_rows(data2025)
data2021 <- drop_empty_rows(data2021)


# Amtlichen Gemeindeschlüssel (als Key für spätere Joins) generieren:
# _ _ (Bundesland) _ (Regierungsbezirk bzw 0) _ _ (Landkreis bzw kreisfreie Stadt) _ _ _ (Gemeinde bzw. 000)
data2025 <- make_ags(data2025)
data2021 <- make_ags(data2021)


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
    agg.schlüssel = paste(sort(unique(Gemeindeschlüssel)), collapse = ", "),
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

# Shifts innerhalb und außerhalb der Gemeinde. Wichtig ist außerhalb der Gemeinde
par68 <- data2025 %>%
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO != "0000") %>%
  group_by(Wahlkreis, Kennziffer.Urnenwahlbezirke.nach...68.BWO) %>%
  summarise(
    gemeinden = paste(unique(Gemeindename), collapse = ", "),
    agg.schlüssel = collapse_keys(Gemeindeschlüssel),
    .groups = "drop"
  ) %>%
  filter(grepl(",", agg.schlüssel))
# In Ordnung, zwar nicht immer TRUE, aber eigentlich (in Worten angegeben) enthalten


# Zusammengeführte Gemeinden je Wahlkreis
joinedG <- bind_rows(
  bw_groups %>%
    transmute(Wahlkreis, agg.schlüssel),
  par68 %>%
    transmute(Wahlkreis, agg.schlüssel)
) %>%
  filter(!is.na(agg.schlüssel), agg.schlüssel != "") %>%
  group_by(Wahlkreis) %>%
  group_modify(~ connected_components(.x$agg.schlüssel)) %>%
  ungroup()

# Ergebnis gleich zur bw_groups Aggregation, siehe:
identisch <- mapply(
  function(x, y) {
    setequal(
      split_keys(x),
      split_keys(y)
    )
  },
  joinedG$agg.schlüssel,
  bw_groups$agg.schlüssel
)

all(identisch)

###############################################################################
#Mapping-Datensatz erstellen
###############################################################################
mapping25 <- data2025 %>%
  select(Wahlkreis, Gemeinde, Gemeindeschlüssel) %>%
  distinct()

mapping25 <- mapping25 %>%
  left_join(
    bw_groups %>%
      select(
        Wahlkreis,
        Gemeindeschlüssel,
        agg.schlüssel
      ),
    by = c(
      "Wahlkreis",
      "Gemeindeschlüssel"
    )
  )

bw_lookup <-
  joinedG %>%
  distinct(Wahlkreis, agg.schlüssel) %>%
  mutate(
    Gemeindeschlüssel =
      strsplit(agg.schlüssel, ",\\s*")
  ) %>%
  tidyr::unnest(Gemeindeschlüssel)

mapping25 <-
  mapping25 %>%
  left_join(
    bw_lookup,
    by = c("Wahlkreis", "Gemeindeschlüssel"),
    suffix = c("", ".bw")
  ) %>%
  mutate(

    agg.schlüssel =
      case_when(

        # künstliche Gemeinde:
        Gemeinde >= 900 ~ agg.schlüssel,

        # echte Gemeinde mit Briefwahlaggregation:
        !is.na(agg.schlüssel.bw) ~ agg.schlüssel.bw,

        # normale Gemeinde:
        TRUE ~ Gemeindeschlüssel

      )

  ) %>%
  select(-agg.schlüssel.bw)






















# Bereinigung Bundestagswahl 2021
# Der Gemeindeschlüssel wurde bereits oben mit make_ags() erzeugt.


# Künstliche Gemeinden über Wahlkreis hinaus (3 Ausnahmen) (unterschiedlicher Gemeindename, Wahlkreis)
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

# beteiligte Gemeinden: 9xx-Datensatz mit den normalen Gemeinden desselben Kreises
# und derselben Briefwahlzugehörigkeit verbinden
# --> nicht nur ein Ergebnis pro Gemeinde! unterschiedlich in "Wahlbezirk" --> interessierend nur Gemeinden
# weil darüber aggregiert wird. Die Wahlbezirke für dieselbe Gemeinde müssen summiert werden

kgemeinden21 <- data2021 %>%
  filter(Gemeinde >= 900) %>%
  distinct(
    Land,
    Regierungsbezirk,
    Kreis,
    Wahlkreis,
    Kennziffer.Briefwahlzugehörigkeit,
    Gemeindeschlüssel,
    Gemeinde.Name
  )

egemeinden21 <- data2021 %>%
  filter(Gemeinde < 900) %>%
  group_by(Land, Regierungsbezirk, Kreis, Wahlkreis, Kennziffer.Briefwahlzugehörigkeit) %>%
  summarise(
    n_gemeinden = n_distinct(Gemeinde),
    gemeinden = paste(sort(unique(Gemeinde.Name)), collapse = ", "),
    agg.schlüssel = paste(sort(unique(Gemeindeschlüssel)), collapse = ", "),
    .groups = "drop"
  )

bw_groups21 <- kgemeinden21 %>%
  left_join(
    egemeinden21,
    by = c("Land", "Regierungsbezirk", "Kreis", "Wahlkreis", "Kennziffer.Briefwahlzugehörigkeit")
  )


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
    leer = E_Gültige == 0
  ) %>%
  count(leer)

# Shifts innerhalb und außerhalb der Gemeinde. Wichtig ist außerhalb der Gemeinde
par6821 <- data2021 %>%
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO != "0000") %>%
  group_by(Wahlkreis, Kennziffer.Urnenwahlbezirke.nach...68.BWO) %>%
  summarise(
    gemeinden = paste(unique(Gemeinde.Name), collapse = ", "),
    agg.schlüssel = collapse_keys(Gemeindeschlüssel),
    .groups = "drop"
  ) %>%
  filter(grepl(",", agg.schlüssel))
# In Ordnung, zwar nicht immer TRUE, aber eigentlich (in Worten angegeben) enthalten


# Zusammengeführte Gemeinden je Wahlkreis
joinedG21 <- bind_rows(
  bw_groups21 %>%
    transmute(Wahlkreis, agg.schlüssel),
  par6821 %>%
    transmute(Wahlkreis, agg.schlüssel)
) %>%
  filter(!is.na(agg.schlüssel), agg.schlüssel != "") %>%
  group_by(Wahlkreis) %>%
  group_modify(~ connected_components(.x$agg.schlüssel)) %>%
  ungroup()

# Ergebnis gleich zur bw_groups Aggregation, siehe:
identisch21 <- mapply(
  function(x, y) {
    setequal(
      split_keys(x),
      split_keys(y)
    )
  },
  joinedG21$agg.schlüssel,
  bw_groups21$agg.schlüssel
)

all(identisch21) # muss TRUE sein für nachfolgenden Code

###############################################################################
#Mapping-Datensatz erstellen
###############################################################################
mapping21 <- data2021 %>%
  select(Wahlkreis, Gemeinde, Gemeindeschlüssel) %>%
  distinct()

mapping21 <- mapping21 %>%
  left_join(
    bw_groups21 %>%
      select(
        Wahlkreis,
        Gemeindeschlüssel,
        agg.schlüssel
      ),
    by = c(
      "Wahlkreis",
      "Gemeindeschlüssel"
    )
  )

bw_lookup21 <-
  joinedG21 %>%
  distinct(Wahlkreis, agg.schlüssel) %>%
  mutate(
    Gemeindeschlüssel =
      strsplit(agg.schlüssel, ",\\s*")
  ) %>%
  tidyr::unnest(Gemeindeschlüssel)

mapping21 <-
  mapping21 %>%
  left_join(
    bw_lookup21,
    by = c("Wahlkreis", "Gemeindeschlüssel"),
    suffix = c("", ".bw")
  ) %>%
  mutate(

    agg.schlüssel =
      case_when(

        # künstliche Gemeinde:
        Gemeinde >= 900 ~ agg.schlüssel,

        # echte Gemeinde mit Briefwahlaggregation:
        !is.na(agg.schlüssel.bw) ~ agg.schlüssel.bw,

        # normale Gemeinde:
        TRUE ~ Gemeindeschlüssel

      )

  ) %>%
  select(-agg.schlüssel.bw)










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



# Keine Beschränkung auf die Schnittmenge der Gemeinden:
# Gebietsänderungen sollen über Aggregationen harmonisiert werden,
# nicht durch Löschen von Gemeinden oder Wahlbezirkszeilen.
mapping21_clean <- mapping21
mapping25_clean <- mapping25
data21_clean <- data2021
data25_clean <- data2025






## Aggregation anpassen
mapping_gebietsaenderungen_pfad <- file.path(data_dir_cleaned, "mapping_gebietsaenderungen.rds")

if (!file.exists(mapping_gebietsaenderungen_pfad)) {
  source("Scripts/mapping_gebiete.R", encoding = "UTF-8")
}

mapping_gebietsaenderungen <- readRDS(mapping_gebietsaenderungen_pfad)

clean_gemeindename <- function(x) {
  x %>%
    str_replace("\\s*\\([^\\)]*\\)\\s*$", "") %>%
    str_squish()
}

extract_einschlussnamen <- function(x) {
  einschluss <- str_match(
    x,
    regex("\\(\\s*einschl\\.\\s*([^\\)]*)\\)", ignore_case = TRUE)
  )[, 2]

  if (is.na(einschluss)) {
    return(character())
  }

  einschluss %>%
    strsplit("\\s*,\\s*|\\s+und\\s+|\\s*;\\s*") %>%
    unlist(use.names = FALSE) %>%
    str_replace("\\s+in\\s+.+\\s+enthalten$", "") %>%
    str_squish() %>%
    .[. != ""]
}

gemeinde_namen_lookup <- bind_rows(
  data2021 %>%
    transmute(
      Land,
      Regierungsbezirk,
      Kreis,
      Gemeinde,
      Gemeindeschlüssel,
      gemeindename = Gemeinde.Name
    ),
  data2025 %>%
    transmute(
      Land,
      Regierungsbezirk,
      Kreis,
      Gemeinde,
      Gemeindeschlüssel,
      gemeindename = Gemeindename
    )
) %>%
  filter(
    !is.na(Gemeinde),
    Gemeinde < 900,
    !is.na(Gemeindeschlüssel),
    !is.na(gemeindename)
  ) %>%
  mutate(
    gemeindename_clean = clean_gemeindename(gemeindename)
  ) %>%
  distinct(
    Land,
    Regierungsbezirk,
    Kreis,
    Gemeindeschlüssel,
    gemeindename_clean
  )

einschluss_ausweisungen <- bind_rows(
  data2021 %>%
    transmute(
      Jahr = 2021,
      Wahlkreis,
      Land,
      Regierungsbezirk,
      Kreis,
      Gemeinde,
      Gemeindeschlüssel,
      gemeindename = Gemeinde.Name,
      par68 = Kennziffer.Urnenwahlbezirke.nach...68.BWO != "0000",
      Bezirksart
    ),
  data2025 %>%
    transmute(
      Jahr = 2025,
      Wahlkreis,
      Land,
      Regierungsbezirk,
      Kreis,
      Gemeinde,
      Gemeindeschlüssel,
      gemeindename = Gemeindename,
      par68 = Kennziffer.Urnenwahlbezirke.nach...68.BWO != "0000",
      Bezirksart
    )
) %>%
  filter(
    !is.na(Gemeindeschlüssel),
    !is.na(gemeindename),
    str_detect(gemeindename, regex("\\(\\s*einschl\\.", ignore_case = TRUE))
  ) %>%
  distinct(
    Jahr,
    Wahlkreis,
    Land,
    Regierungsbezirk,
    Kreis,
    Gemeinde,
    Gemeindeschlüssel,
    gemeindename,
    par68,
    Bezirksart
  ) %>%
  mutate(
    einschluss_name = lapply(gemeindename, extract_einschlussnamen)
  ) %>%
  unnest(einschluss_name) %>%
  mutate(
    einschluss_name_clean = clean_gemeindename(einschluss_name)
  )

vorlaeufiges_mapping <- bind_rows(
  mapping21_clean %>%
    mutate(Jahr = 2021),
  mapping25_clean %>%
    mutate(Jahr = 2025)
) %>%
  select(
    Jahr,
    Wahlkreis,
    Gemeinde,
    Gemeindeschlüssel,
    agg.vorlaeufig = agg.schlüssel
  )

textausweisungen_namensdiagnose <- einschluss_ausweisungen %>%
  left_join(
    gemeinde_namen_lookup %>%
      group_by(
        Land,
        Regierungsbezirk,
        Kreis,
        gemeindename_clean
      ) %>%
      mutate(
        n_gemeindeschluessel_match = n_distinct(Gemeindeschlüssel)
      ) %>%
      ungroup() %>%
      rename(
        einschluss_gemeindeschlüssel = Gemeindeschlüssel
      ),
    by = c(
      "Land",
      "Regierungsbezirk",
      "Kreis",
      "einschluss_name_clean" = "gemeindename_clean"
    ),
    relationship = "many-to-many"
  ) %>%
  left_join(
    vorlaeufiges_mapping %>%
      select(
        Jahr,
        Wahlkreis,
        Gemeindeschlüssel,
        agg.ausweisende_gemeinde = agg.vorlaeufig
      ),
    by = c(
      "Jahr",
      "Wahlkreis",
      "Gemeindeschlüssel"
    )
  ) %>%
  left_join(
    vorlaeufiges_mapping %>%
      select(
        Jahr,
        Wahlkreis,
        einschluss_gemeindeschlüssel = Gemeindeschlüssel,
        agg.einschluss_gemeinde = agg.vorlaeufig
      ),
    by = c(
      "Jahr",
      "Wahlkreis",
      "einschluss_gemeindeschlüssel"
    )
  ) %>%
  mutate(
    namensmatch_status = case_when(
      is.na(einschluss_gemeindeschlüssel) ~ "kein Namensmatch im selben Kreis",
      n_gemeindeschluessel_match > 1 ~ "mehrdeutiger Namensmatch im selben Kreis",
      einschluss_gemeindeschlüssel == Gemeindeschlüssel ~ "Selbstmatch",
      TRUE ~ "eindeutiger Namensmatch im selben Kreis"
    ),
    einschluss_in_gleichem_jahr_wahlkreis = !is.na(agg.einschluss_gemeinde),
    bereits_vorlaeufig_abgedeckt =
      !is.na(agg.ausweisende_gemeinde) &
      !is.na(agg.einschluss_gemeinde) &
      agg.ausweisende_gemeinde == agg.einschluss_gemeinde,
    ausweisende_agg_enthaelt_einschluss = mapply(
      function(agg, key) {
        !is.na(agg) &&
          !is.na(key) &&
          key %in% split_keys(agg)
      },
      agg.ausweisende_gemeinde,
      einschluss_gemeindeschlüssel,
      USE.NAMES = FALSE
    )
  )

textausweisungen_inkonsistenzen <- textausweisungen_namensdiagnose %>%
  filter(
    namensmatch_status != "eindeutiger Namensmatch im selben Kreis" |
      !bereits_vorlaeufig_abgedeckt
  )

build_final_lookup <- function(components) {
  components %>%
    mutate(
      Gemeindeschlüssel = strsplit(agg.schlüssel, ",\\s*")
    ) %>%
    tidyr::unnest(Gemeindeschlüssel) %>%
    rename(
      agg.final = agg.schlüssel
    ) %>%
    distinct(Gemeindeschlüssel, agg.final)
}

build_prelim_to_final <- function(final_lookup) {
  bind_rows(
    mapping21_clean %>%
      distinct(agg.schlüssel),
    mapping25_clean %>%
      distinct(agg.schlüssel)
  ) %>%
    distinct() %>%
    mutate(
      Gemeindeschlüssel = strsplit(agg.schlüssel, ",\\s*")
    ) %>%
    tidyr::unnest(Gemeindeschlüssel) %>%
    left_join(
      final_lookup,
      by = "Gemeindeschlüssel"
    ) %>%
    group_by(agg.schlüssel) %>%
    summarise(
      n_missing_final = sum(is.na(agg.final)),
      agg.final = collapse_keys(
        unlist(
          lapply(agg.final, split_keys),
          use.names = FALSE
        )
      ),
      .groups = "drop"
    )
}

apply_final_mapping <- function(mapping_clean, prelim_to_final) {
  mapping_clean %>%
    left_join(
      prelim_to_final %>%
        select(agg.schlüssel, agg.final),
      by = "agg.schlüssel"
    ) %>%
    mutate(
      agg.schlüssel = agg.final
    ) %>%
    select(-agg.final)
}

final_components_basis <- bind_rows(
  mapping21_clean %>%
    distinct(agg.schlüssel),
  mapping25_clean %>%
    distinct(agg.schlüssel),
  mapping_gebietsaenderungen %>%
    distinct(agg.schlüssel)
) %>%
  filter(!is.na(agg.schlüssel), agg.schlüssel != "") %>%
  pull(agg.schlüssel) %>%
  connected_components()

final_lookup_basis <- build_final_lookup(final_components_basis)

final_lookup_basis_check <- final_lookup_basis %>%
  count(Gemeindeschlüssel) %>%
  filter(n > 1)

stopifnot(nrow(final_lookup_basis_check) == 0)

prelim_to_final_basis <- build_prelim_to_final(final_lookup_basis)

stopifnot(all(prelim_to_final_basis$n_missing_final == 0))

mapping21_basis <- apply_final_mapping(mapping21_clean, prelim_to_final_basis)
mapping25_basis <- apply_final_mapping(mapping25_clean, prelim_to_final_basis)

diff21_basis <- setdiff(
  unique(mapping21_basis$agg.schlüssel),
  unique(mapping25_basis$agg.schlüssel)
)

diff25_basis <- setdiff(
  unique(mapping25_basis$agg.schlüssel),
  unique(mapping21_basis$agg.schlüssel)
)

restfaelle_basis <- bind_rows(
  mapping21_basis %>%
    filter(agg.schlüssel %in% diff21_basis) %>%
    mutate(Jahr = 2021),
  mapping25_basis %>%
    filter(agg.schlüssel %in% diff25_basis) %>%
    mutate(Jahr = 2025)
) %>%
  distinct(
    Jahr,
    Wahlkreis,
    Gemeinde,
    Gemeindeschlüssel,
    agg.schlüssel
  ) %>%
  arrange(Jahr, Wahlkreis, Gemeindeschlüssel)

mapping_manuelle_textkorrekturen <- tibble::tribble(
  ~agg.basis, ~agg.schlüssel, ~grund,
  "07138010", "07138010, 07138047", "Datzeroth wird 2025 separat ausgewiesen, 2021 aber bei Niederbreitbach (einschl. Datzeroth)."
)

unabgedeckte_basis_differenzen <- setdiff(
  c(diff21_basis, diff25_basis),
  mapping_manuelle_textkorrekturen$agg.basis
)

stopifnot(length(unabgedeckte_basis_differenzen) == 0)

final_components <- bind_rows(
  mapping21_clean %>%
    distinct(agg.schlüssel),
  mapping25_clean %>%
    distinct(agg.schlüssel),
  mapping_gebietsaenderungen %>%
    distinct(agg.schlüssel),
  mapping_manuelle_textkorrekturen %>%
    distinct(agg.schlüssel)
) %>%
  filter(!is.na(agg.schlüssel), agg.schlüssel != "") %>%
  pull(agg.schlüssel) %>%
  connected_components()

final_lookup <- final_components %>%
  mutate(
    Gemeindeschlüssel = strsplit(agg.schlüssel, ",\\s*")
  ) %>%
  tidyr::unnest(Gemeindeschlüssel) %>%
  rename(
    agg.final = agg.schlüssel
  ) %>%
  distinct(Gemeindeschlüssel, agg.final)

final_lookup_check <- final_lookup %>%
  count(Gemeindeschlüssel) %>%
  filter(n > 1)

stopifnot(nrow(final_lookup_check) == 0)

prelim_to_final <- bind_rows(
  mapping21_clean %>%
    distinct(agg.schlüssel),
  mapping25_clean %>%
    distinct(agg.schlüssel)
) %>%
  distinct() %>%
  mutate(
    Gemeindeschlüssel = strsplit(agg.schlüssel, ",\\s*")
  ) %>%
  tidyr::unnest(Gemeindeschlüssel) %>%
  left_join(
    final_lookup,
    by = "Gemeindeschlüssel"
  ) %>%
  group_by(agg.schlüssel) %>%
  summarise(
    n_missing_final = sum(is.na(agg.final)),
    agg.final = collapse_keys(
      unlist(
        lapply(agg.final, split_keys),
        use.names = FALSE
      )
    ),
    .groups = "drop"
  )

stopifnot(all(prelim_to_final$n_missing_final == 0))

mapping21_new <- mapping21_clean %>%
  left_join(
    prelim_to_final %>%
      select(agg.schlüssel, agg.final),
    by = "agg.schlüssel"
  ) %>%
  mutate(
    agg.schlüssel = agg.final
  ) %>%
  select(-agg.final)

mapping25_new <- mapping25_clean %>%
  left_join(
    prelim_to_final %>%
      select(agg.schlüssel, agg.final),
    by = "agg.schlüssel"
  ) %>%
  mutate(
    agg.schlüssel = agg.final
  ) %>%
  select(-agg.final)

mapping_wahldaten_final <- bind_rows(
  mapping21_new %>%
    mutate(Jahr = 2021),
  mapping25_new %>%
    mutate(Jahr = 2025)
) %>%
  distinct(
    Jahr,
    Wahlkreis,
    Gemeinde,
    Gemeindeschlüssel,
    agg.schlüssel
  ) %>%
  arrange(Jahr, Wahlkreis, Gemeindeschlüssel)

mapping_gemeinden_final <- mapping_wahldaten_final %>%
  filter(Gemeinde < 900) %>%
  distinct(
    Gemeindeschlüssel,
    agg.schlüssel
  ) %>%
  arrange(Gemeindeschlüssel)

mapping_gemeinden_final_check <- mapping_gemeinden_final %>%
  count(Gemeindeschlüssel) %>%
  filter(n > 1)

stopifnot(nrow(mapping_gemeinden_final_check) == 0)

saveRDS(
  mapping_wahldaten_final,
  file = file.path(data_dir_cleaned, "mapping_wahldaten_final_manuell_validiert.rds")
)
saveRDS(
  mapping_gemeinden_final,
  file = file.path(data_dir_cleaned, "mapping_gemeinden_final_manuell_validiert.rds")
)
saveRDS(
  textausweisungen_namensdiagnose,
  file = file.path(data_dir_validation, "textausweisungen_namensdiagnose.rds")
)
saveRDS(
  textausweisungen_inkonsistenzen,
  file = file.path(data_dir_validation, "textausweisungen_inkonsistenzen.rds")
)


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






# Aggregation der Wahldaten
id_vars <- c(
  "Wahlkreis",
  "Land",
  "Regierungsbezirk",
  "Kreis",
  "Verbandsgemeinde",
  "Gemeinde",
  "Kennziffer.Urnenwahlbezirke.nach...68.BWO",
  "Kennziffer.Briefwahlzugehörigkeit",
  "Gemeindename",
  "Gemeinde.Name",
  "Wahlbezirk",
  "Bezirksart",
  "Ungekürzte.Wahlbezirksbezeichnung",
  "Bezeichnung.des.Wahlbezirkes.gemäß.Anlage.30.zur.BWO",
  "Gemeindeschlüssel"
)

num_vars25 <- setdiff(
  names(data2025)[sapply(data2025, is.numeric)],
  id_vars
)

num_vars21 <- setdiff(
  names(data2021)[sapply(data2021, is.numeric)],
  id_vars
)

data25_agg <- data25_clean %>%
  left_join(
    mapping25_new %>%
      select(
        Wahlkreis,
        Gemeindeschlüssel,
        agg.schlüssel
      ),
    by = c("Wahlkreis", "Gemeindeschlüssel")
  )

data21_agg <- data21_clean %>%
  left_join(
    mapping21_new %>%
      select(
        Wahlkreis,
        Gemeindeschlüssel,
        agg.schlüssel
      ),
    by = c("Wahlkreis", "Gemeindeschlüssel")
  )

wahldaten2025 <- data25_agg %>%
  group_by(agg.schlüssel) %>%
  summarise(

    Wahlkreis = first(Wahlkreis),

    Gemeinden =
      paste(sort(unique(Gemeindename)), collapse = ", "),

    Gemeindeschlüssel =
      paste(sort(unique(Gemeindeschlüssel)), collapse = ", "),

    across(
      all_of(num_vars25),
      ~sum(.x, na.rm = TRUE)
    ),

    .groups = "drop"
  )

wahldaten2021 <- data21_agg %>%
  group_by(agg.schlüssel) %>%
  summarise(

    Wahlkreis = first(Wahlkreis),

    Gemeinden =
      paste(sort(unique(Gemeinde.Name)), collapse = ", "),

    Gemeindeschlüssel =
      paste(sort(unique(Gemeindeschlüssel)), collapse = ", "),

    across(
      all_of(num_vars21),
      ~sum(.x, na.rm = TRUE)
    ),

    .groups = "drop"
  )






# Mapping Größe untersuchen
wahldaten2025 %>%
  summarise(
    n        = n(),
    Mittel   = mean(Gültige...Erststimmen),
    Median   = median(Gültige...Erststimmen),
    SD       = sd(Gültige...Erststimmen),
    Varianz  = var(Gültige...Erststimmen),
    Minimum  = min(Gültige...Erststimmen),
    Q1       = quantile(Gültige...Erststimmen, 0.25),
    Q3       = quantile(Gültige...Erststimmen, 0.75),
    Maximum  = max(Gültige...Erststimmen),
    IQR      = IQR(Gültige...Erststimmen),
    CV       = sd(Gültige...Erststimmen) / mean(Gültige...Erststimmen)
  )

options(scipen = 999)
hist(
  wahldaten2025$Gültige...Erststimmen,
  breaks = 100,
  main = "Histogramm der gültigen Erststimmen",
  xlab = "Gültige Erststimmen"
)




# Bundestagswahl 2025 abspeichern
saveRDS(
  wahldaten2025,
  file = file.path(data_dir_cleaned, "wahldaten2025_gemappt.rds")
)

# Mapping speichern
saveRDS(
  mapping_wahldaten_final,
  file = file.path(data_dir_cleaned, "mapping_wahldaten_final_manuell_validiert.rds")
)
saveRDS(
  mapping_gemeinden_final,
  file = file.path(data_dir_cleaned, "mapping_gemeinden_final_manuell_validiert.rds")
)

# Abspeichern Bundestagswahl 2021
saveRDS(
  wahldaten2021,
  file = file.path(data_dir_cleaned, "wahldaten2021_gemappt.rds")
)
