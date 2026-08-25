# INKAR-Werte von 2023 auf die finalen Wahl-Aggregationseinheiten zusammenfassen.

library(data.table)
library(dplyr)
library(sf)
library(tidyr)

source("Functions/general_functions.R", encoding = "UTF-8")
source("paths.R", encoding = "UTF-8")

ensure_data_dirs()

# Kleine, bereits auf 2023 und relevante Indikatoren gefilterte INKAR-Basis laden.
inkar_basis_rds <- file.path(data_dir_intermediate, "vorlaeufig_inkar_basis_gemeinden_kreise.rds")
inkar_metadata_rds <- file.path(data_dir_intermediate, "vorlaeufig_inkar_basis_metadata.rds")

if (!file.exists(inkar_basis_rds) || !file.exists(inkar_metadata_rds)) {
  source("Scripts/cleaning_strukturdaten.R", encoding = "UTF-8")
}

inkar <- as_tibble(readRDS(inkar_basis_rds))
metadata <- readRDS(inkar_metadata_rds)
struktur_jahr <- if ("struktur_jahr" %in% names(metadata)) {
  metadata$struktur_jahr[[1]]
} else {
  2023L
}
# Indikatorkonfiguration ausschliesslich aus dem zuvor erzeugten Cache uebernehmen.
if (!"inkar_indikatoren" %in% names(metadata)) {
  stop(
    "Die INKAR-Metadaten enthalten keine Indikatorkonfiguration. ",
    "Fuehre zuerst Scripts/cleaning_strukturdaten.R aus."
  )
}

indikator_config <- as_tibble(metadata$inkar_indikatoren)
gemeinde_config <- indikator_config %>%
  filter(Raumbezug == "Gemeinden")
kreis_config <- indikator_config %>%
  filter(Raumbezug == "Kreise")
struktur_variablen <- setdiff(unique(indikator_config$variable), "bevoelkerung")
speziell_aggregierte_variablen <- c(
  "anteilAuslaendischeArbeitslose",
  "haushaltsgroesseMean",
  "einwohnerdichte",
  "einwohnerArbeitsplatzDichte"
)
bevoelkerungsgewichtete_variablen <- setdiff(
  struktur_variablen,
  speziell_aggregierte_variablen
)

if (!"bevoelkerung" %in% indikator_config$variable) {
  stop(
    "Die INKAR-Indikator-Konfiguration muss eine Variable 'bevoelkerung' ",
    "enthalten, weil sie als Aggregationsgewicht verwendet wird."
  )
}

# Finales Gemeindemapping und die vollstaendige Menge der Analyse-Aggregationen laden.
mapping_gemeinden <- readRDS(file.path(data_dir_cleaned, "mapping_gemeinden_final_manuell_validiert.rds"))
all_aggs <- readRDS(file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_2021.rds")) %>%
  select(agg_schluessel) %>%
  distinct() %>%
  arrange(agg_schluessel)

geometry_path <- getOption(
  "waehlendenwanderung.struktur_geometry_path",
  file.path(
    "Data",
    "raw",
    "gebiete_visualisierung",
    "vg250_01-01.utm32s.gpkg.ebenen",
    "vg250_ebenen_0101",
    "DE_VG250.gpkg"
  )
)

gerichtetes_mapping_path <- file.path(
  data_dir_cleaned,
  "mapping_gebietsaenderungen_gerichtet.rds"
)

if (!file.exists(gerichtetes_mapping_path)) {
  source("Scripts/mapping_gebiete.R", encoding = "UTF-8")
}

if (!file.exists(geometry_path)) {
  stop("Die VG250-Geometriedatei wurde nicht gefunden: ", geometry_path)
}

gebietsaenderungen_gerichtet <- readRDS(gerichtetes_mapping_path)

agg_col <- get_agg_col(mapping_gemeinden)
ags_col <- grep("^Gemeindesch", names(mapping_gemeinden), value = TRUE)[1]

mapping_gemeinden <- mapping_gemeinden %>%
  transmute(
    gemeindeschluessel = .data[[ags_col]],
    agg_schluessel = .data[[agg_col]]
  ) %>%
  distinct()

# Gemeindeindikatoren direkt ueber den Gemeindeschluessel aufbereiten.
gemeinde_indikatoren <- inkar %>%
  inner_join(
    gemeinde_config,
    by = c("Kuerzel", "Raumbezug")
  ) %>%
  transmute(
    gemeindeschluessel = Kennziffer,
    jahr = Zeitbezug,
    variable,
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

gemeinde_struktur <- gemeinde_indikatoren %>%
  mutate(
    kreis_schluessel = paste0(substr(gemeindeschluessel, 1, 5), "000")
  )

# Nur falls noetig Kreisindikatoren als Ersatz fuer nicht gemeindescharfe Variablen anspielen.
if (nrow(kreis_config) > 0) {
  kreis_indikatoren <- inkar %>%
    inner_join(
      kreis_config,
      by = c("Kuerzel", "Raumbezug")
    ) %>%
    transmute(
      kreis_schluessel = Kennziffer,
      jahr = Zeitbezug,
      variable,
      wert = Wert
    ) %>%
    group_by(
      kreis_schluessel,
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

  gemeinde_struktur <- gemeinde_struktur %>%
    left_join(
      kreis_indikatoren,
      by = c("kreis_schluessel", "jahr")
    )
}

inkar_gemeinden <- unique(gemeinde_struktur$gemeindeschluessel)

# Fuer jeden historischen Gemeindeschluessel den eindeutigen aktuellen
# Nachfolger suchen, dessen VG250-Geometrie fuer den Mittelpunkt genutzt wird.
find_current_successor <- function(
    gemeindeschluessel,
    current_keys,
    changes) {
  frontier <- gemeindeschluessel
  visited <- character()
  targets <- character()

  while (length(frontier) > 0) {
    frontier <- setdiff(frontier, visited)

    if (length(frontier) == 0) {
      break
    }

    visited <- union(visited, frontier)
    targets <- union(targets, intersect(frontier, current_keys))

    frontier <- changes %>%
      filter(.data$AGS_alt %in% setdiff(frontier, current_keys)) %>%
      pull(.data$AGS_neu) %>%
      unique()
  }

  targets
}

# Bevoelkerungsanteile eines historischen Schluessels auf aktuelle Nachfolger
# verteilen. Bei Aufteilungen werden die amtlich betroffenen Einwohner genutzt.
make_successor_weights <- function(
    gemeindeschluessel,
    current_keys,
    changes) {
  targets <- find_current_successor(
    gemeindeschluessel,
    current_keys,
    changes
  )

  if (length(targets) == 0) {
    return(tibble(
      gemeindeschluessel = character(),
      gemeindeschluessel_2025 = character(),
      successor_weight = numeric()
    ))
  }

  if (length(targets) == 1) {
    return(tibble(
      gemeindeschluessel = gemeindeschluessel,
      gemeindeschluessel_2025 = targets,
      successor_weight = 1
    ))
  }

  allocation <- changes %>%
    filter(
      .data$AGS_alt == gemeindeschluessel,
      .data$AGS_neu %in% targets
    ) %>%
    group_by(.data$AGS_neu) %>%
    summarise(
      Einwohner = sum(.data$Einwohner, na.rm = TRUE),
      .groups = "drop"
    )

  if (
    !setequal(allocation$AGS_neu, targets) ||
      any(allocation$Einwohner <= 0) ||
      sum(allocation$Einwohner) <= 0
  ) {
    stop(
      "Die Bevoelkerung des aufgeteilten Gemeindeschluessels ",
      gemeindeschluessel,
      " kann nicht eindeutig auf aktuelle Nachfolger verteilt werden."
    )
  }

  allocation %>%
    transmute(
      gemeindeschluessel = .env$gemeindeschluessel,
      gemeindeschluessel_2025 = .data$AGS_neu,
      successor_weight = .data$Einwohner / sum(.data$Einwohner)
    )
}

# Nur fuer die Hauptstichprobe Ostdeutschland ohne Berlin raeumliche
# Aggregationspunkte und die Entfernung zur auslaendischen Landgrenze bilden.
ost_aggs <- all_aggs %>%
  filter(is_ostdeutschland_ohne_berlin(.data$agg_schluessel))

ost_agg_keys <- ost_aggs %>%
  mutate(
    gemeindeschluessel = lapply(.data$agg_schluessel, split_keys)
  ) %>%
  unnest(gemeindeschluessel)

gemeinde_geometrien <- st_read(
  geometry_path,
  layer = "vg250_gem",
  quiet = TRUE
) %>%
  filter(
    .data$GF == 4,
    .data$BSG == 1,
    substr(.data$AGS, 1, 2) %in% c("12", "13", "14", "15", "16")
  ) %>%
  transmute(
    gemeindeschluessel_2025 = as.character(.data$AGS)
  )

current_geometry_keys <- unique(gemeinde_geometrien$gemeindeschluessel_2025)

bevoelkerung_2023 <- gemeinde_indikatoren %>%
  filter(.data$jahr == .env$struktur_jahr) %>%
  select(
    gemeindeschluessel,
    bevoelkerung_2023 = bevoelkerung
  )

# Bei Gebietsveraenderungen wird die Bevoelkerung der alten Gemeinde dem
# eindeutigen aktuellen Nachfolger zugerechnet. Dies ist die vereinbarte
# Naeherung fuer 17 ostdeutsche Aggregationen ohne historische Geometrien.
population_sources <- ost_agg_keys %>%
  inner_join(
    bevoelkerung_2023,
    by = "gemeindeschluessel"
  ) %>%
  filter(
    !is.na(.data$bevoelkerung_2023),
    .data$bevoelkerung_2023 > 0
  )

successor_weights <- bind_rows(lapply(
  unique(population_sources$gemeindeschluessel),
  make_successor_weights,
  current_keys = current_geometry_keys,
  changes = gebietsaenderungen_gerichtet
))

missing_successor_keys <- setdiff(
  unique(population_sources$gemeindeschluessel),
  unique(successor_weights$gemeindeschluessel)
)

if (length(missing_successor_keys) > 0) {
  stop(
    "Fuer folgende ostdeutsche Gemeindeschluessel wurde keine aktuelle ",
    "VG250-Geometrie gefunden: ",
    paste(missing_successor_keys, collapse = ", ")
  )
}

successor_weight_check <- successor_weights %>%
  group_by(.data$gemeindeschluessel) %>%
  summarise(
    weight_sum = sum(.data$successor_weight),
    .groups = "drop"
  )

if (any(abs(successor_weight_check$weight_sum - 1) > 1e-10)) {
  stop("Die Nachfolgergewichte summieren sich nicht fuer jede Gemeinde zu 1.")
}

population_by_geometry <- population_sources %>%
  left_join(
    successor_weights,
    by = "gemeindeschluessel"
  ) %>%
  mutate(
    bevoelkerung_2023 = .data$bevoelkerung_2023 * .data$successor_weight
  ) %>%
  rowwise() %>%
  mutate(
    successor_in_same_agg = .data$gemeindeschluessel_2025 %in%
      split_keys(.data$agg_schluessel)
  ) %>%
  ungroup()

if (any(!population_by_geometry$successor_in_same_agg)) {
  stop("Mindestens ein aktueller Gemeindenachfolger liegt ausserhalb seines finalen agg_schluessels.")
}

population_by_geometry <- population_by_geometry %>%
  group_by(
    .data$agg_schluessel,
    .data$gemeindeschluessel_2025
  ) %>%
  summarise(
    bevoelkerung_2023 = sum(.data$bevoelkerung_2023),
    .groups = "drop"
  )

missing_ost_aggs <- setdiff(
  ost_aggs$agg_schluessel,
  unique(population_by_geometry$agg_schluessel)
)

if (length(missing_ost_aggs) > 0) {
  stop(
    "Fuer folgende ostdeutsche Aggregationen konnte kein Bevoelkerungsgewicht ",
    "gebildet werden: ",
    paste(missing_ost_aggs, collapse = ", ")
  )
}

# Geometrische Gemeindemittelpunkte mit der auf die aktuellen Geometrien
# uebertragenen Bevoelkerung 2023 zum Aggregationsmittelpunkt gewichten.
gemeinde_mittelpunkte <- suppressWarnings(
  st_centroid(gemeinde_geometrien)
)
mittelpunkt_koordinaten <- st_coordinates(gemeinde_mittelpunkte)

gemeinde_mittelpunkte <- gemeinde_mittelpunkte %>%
  st_drop_geometry() %>%
  mutate(
    x = mittelpunkt_koordinaten[, 1],
    y = mittelpunkt_koordinaten[, 2]
  )

agg_mittelpunkte <- population_by_geometry %>%
  left_join(
    gemeinde_mittelpunkte,
    by = "gemeindeschluessel_2025"
  ) %>%
  group_by(.data$agg_schluessel) %>%
  summarise(
    x = weighted.mean(.data$x, .data$bevoelkerung_2023),
    y = weighted.mean(.data$y, .data$bevoelkerung_2023),
    bevoelkerung_2023 = sum(.data$bevoelkerung_2023),
    .groups = "drop"
  )

if (any(!is.finite(agg_mittelpunkte$x)) || any(!is.finite(agg_mittelpunkte$y))) {
  stop("Mindestens ein bevoelkerungsgewichteter Aggregationsmittelpunkt ist nicht endlich.")
}

agg_mittelpunkte_sf <- st_as_sf(
  agg_mittelpunkte,
  coords = c("x", "y"),
  crs = st_crs(gemeinde_geometrien),
  remove = FALSE
)

# AGZ 1 bezeichnet die Staatsgrenze; GMK 0 schliesst Kuesten- und
# Meeresgrenzen aus. Die UTM-Geometrie liefert euklidische Meterdistanzen.
staatsgrenze_land <- st_read(
  geometry_path,
  layer = "vg250_li",
  quiet = TRUE
) %>%
  filter(
    .data$AGZ == 1,
    .data$GMK == 0
  ) %>%
  st_geometry() %>%
  st_union()

agg_distanz <- agg_mittelpunkte_sf %>%
  mutate(
    distanz_staatsgrenze_km = as.numeric(
      st_distance(geometry, staatsgrenze_land)
    ) / 1000
  ) %>%
  st_drop_geometry() %>%
  select(
    agg_schluessel,
    distanz_staatsgrenze_km
  )

if (
  nrow(agg_distanz) != nrow(ost_aggs) ||
    any(!is.finite(agg_distanz$distanz_staatsgrenze_km)) ||
    any(agg_distanz$distanz_staatsgrenze_km < 0)
) {
  stop("Die Staatsgrenzendistanz ist fuer die Oststichprobe nicht vollstaendig oder unplausibel.")
}

# Gemeinden ohne eigenen INKAR-Eintrag innerhalb ihrer finalen Aggregation kennzeichnen.
mapping_gemeinden <- mapping_gemeinden %>%
  mutate(
    gemeindeschluessel_inkar = case_when(
      gemeindeschluessel %in% inkar_gemeinden ~ gemeindeschluessel,
      substr(gemeindeschluessel, 1, 2) == "02" ~ "02000000",
      substr(gemeindeschluessel, 1, 2) == "11" ~ "11000000",
      TRUE ~ gemeindeschluessel
    )
  )

# Werte innerhalb jedes agg_schluessels passend zur inhaltlichen Bezugsgroesse zusammenfassen.
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
    across(
      all_of(bevoelkerungsgewichtete_variablen),
      ~ weighted_mean_safe(.x, bevoelkerung)
    ),
    # Anteil auslaendischer Arbeitsloser an allen Arbeitslosen auf die
    # passende Grundgesamtheit beziehen: alle Arbeitslosen, angenaehert ueber
    # Bevoelkerung * Arbeitslosenquote. Eine Gewichtung nach Auslaenderzahl
    # waere fuer diesen INKAR-Indikator fachlich falsch.
    anteilAuslaendischeArbeitslose = {
      arbeitslose_gewicht <- bevoelkerung * arbeitslosigkeit
      weighted_mean_safe(anteilAuslaendischeArbeitslose, arbeitslose_gewicht)
    },
    # Durchschnittliche Haushaltsgroesse ueber die aus Bevoelkerung und
    # Haushaltsgroesse angenaeherte Zahl der Haushalte aggregieren.
    haushaltsgroesseMean = {
      gueltig <- !is.na(bevoelkerung) & bevoelkerung > 0 &
        !is.na(haushaltsgroesseMean) & haushaltsgroesseMean > 0

      if (any(gueltig)) {
        sum(bevoelkerung[gueltig]) /
          sum(bevoelkerung[gueltig] / haushaltsgroesseMean[gueltig])
      } else {
        NA_real_
      }
    },
    # Einwohner-Arbeitsplatz-Dichte mit der aus Bevoelkerung und Einwohnerdichte
    # angenaeherten Gemeindeflaeche gewichten. Dies steht vor der Aggregation der
    # Einwohnerdichte, damit hier noch deren gemeindescharfe Werte verwendet werden.
    einwohnerArbeitsplatzDichte = {
      gueltig <- !is.na(bevoelkerung) & bevoelkerung > 0 &
        !is.na(einwohnerdichte) & einwohnerdichte > 0 &
        !is.na(einwohnerArbeitsplatzDichte)

      if (any(gueltig)) {
        flaeche <- bevoelkerung[gueltig] / einwohnerdichte[gueltig]
        sum(einwohnerArbeitsplatzDichte[gueltig] * flaeche) / sum(flaeche)
      } else {
        NA_real_
      }
    },
    # Einwohnerdichte als Gesamtbevoelkerung geteilt durch die aus den
    # Gemeindedichten angenaeherte Gesamtflaeche berechnen.
    einwohnerdichte = {
      gueltig <- !is.na(bevoelkerung) & bevoelkerung > 0 &
        !is.na(einwohnerdichte) & einwohnerdichte > 0

      if (any(gueltig)) {
        sum(bevoelkerung[gueltig]) /
          sum(bevoelkerung[gueltig] / einwohnerdichte[gueltig])
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  )

# Die Grenzdistanz ist nur fuer die Hauptstichprobe Ostdeutschland ohne Berlin
# definiert und wird ueber den unveraenderten agg_schluessel angefuegt.
agg_struktur_long <- agg_struktur_long %>%
  left_join(
    agg_distanz,
    by = "agg_schluessel"
  )

struktur_variablen <- c(
  struktur_variablen,
  "distanz_staatsgrenze_km"
)
struktur_spalten <- paste0(struktur_variablen, "_", struktur_jahr)

agg_struktur_wide_inner <- agg_struktur_long %>%
  filter(jahr == struktur_jahr) %>%
  mutate(
    struktur_jahr = .env$struktur_jahr
  ) %>%
  rename_with(
    ~ paste0(.x, "_", struktur_jahr),
    all_of(struktur_variablen)
  ) %>%
  rename(
    !!paste0("gewicht_summe_", struktur_jahr) := gewicht_summe
  ) %>%
  select(
    agg_schluessel,
    struktur_jahr,
    all_of(struktur_spalten),
    all_of(paste0("gewicht_summe_", struktur_jahr)),
    n_gemeinden_mapping,
    n_gemeinden_mit_inkar
  ) %>%
  arrange(agg_schluessel)

# Eine Zeile je Wahlanalyseeinheit und eine Spalte je 2023-Kovariate erzeugen.
agg_struktur_wide <- all_aggs %>%
  left_join(
    agg_struktur_wide_inner,
    by = "agg_schluessel"
  )

# Die Vollstaendigkeitschecks beziehen sich auf die Ost-Hauptstichprobe, weil
# die Grenzdistanz fuer westdeutsche Einheiten bewusst nicht berechnet wird.
struktur_checks <- agg_struktur_wide %>%
  filter(is_ostdeutschland_ohne_berlin(.data$agg_schluessel)) %>%
  summarise(
    n_agg = n(),
    across(
      all_of(struktur_spalten),
      ~ sum(is.na(.x)),
      .names = "n_missing_{.col}"
    ),
    struktur_jahr = .env$struktur_jahr
  )

struktur_missing <- agg_struktur_wide %>%
  filter(is_ostdeutschland_ohne_berlin(.data$agg_schluessel)) %>%
  filter(
    if_any(
      all_of(struktur_spalten),
      is.na
    )
  )

# Interne Plausibilitaetspruefungen, nicht als eigene Datensaetze gespeichert:
# - struktur_checks: Zaehlt fehlende 2023-Kovariaten in der Oststichprobe.
# - struktur_missing: Enthaelt Ost-Einheiten mit mindestens einem fehlenden Wert.
#   Diese Einheiten bleiben im Datensatz und werden in Regressionen fallweise ausgeschlossen.
if (nrow(struktur_missing) > 0) {
  warning(
    "INKAR-Aggregation enthaelt ",
    nrow(struktur_missing),
    " agg_schluessel ohne vollstaendige 2023-Kovariaten. ",
    "Die Regressionsskripte verwenden fuer die betroffenen Modelle vollstaendige Faelle."
  )
}

saveRDS(agg_struktur_long, file.path(data_dir_cleaned, "vorlaeufig_inkar_agg_long.rds"))
saveRDS(agg_struktur_wide, file.path(data_dir_cleaned, "vorlaeufig_inkar_kovariaten_2023.rds"))
