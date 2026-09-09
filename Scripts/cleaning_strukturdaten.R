# Benötigte INKAR-Indikatoren fuer 2023 einlesen und als kleine wiederverwendbare Basis cachen.

library(data.table)

source("paths.R", encoding = "UTF-8")

ensure_data_dirs()

setDTthreads(max(1, parallel::detectCores() - 2))

inkar_pfad <- "Data/raw/inkar_2025/inkar_2025.csv"
inkar_basis_rds <- file.path(data_dir_intermediate, "vorlaeufig_inkar_basis_gemeinden_kreise.rds")
inkar_metadata_rds <- file.path(data_dir_intermediate, "vorlaeufig_inkar_basis_metadata.rds")

legacy_basis_rds <- file.path(data_dir_cleaned, "vorlaeufig_inkar_workflow_rohdaten.rds")
legacy_metadata_rds <- file.path(data_dir_cleaned, "vorlaeufig_inkar_workflow_metadata.rds")
force_rebuild <- isTRUE(getOption("waehlendenwanderung.inkar_basis_rebuild", FALSE))
struktur_jahr <- as.integer(getOption("waehlendenwanderung.inkar_struktur_jahr", 2023L))

inkar_indikatoren <- data.table(
  Kuerzel = c(
    "xbev", # Bevoelkerung als Aggregationsgewicht
    "q_bev_fl", # Einwohner je Quadratkilometer
    "m_G02_SUP_DIST", # Einwohnergewichtete Entfernung zum naechsten Supermarkt
    "m_bev_alter", # Durchschnittsalter der Bevoelkerung
    "i_wans", # Wanderungssaldo je 1.000 Einwohner
    "svw", # Beschaeftigte am Wohnort als Gewicht fuer a_pend50
    "a_pend50", # Anteil Beschaeftigte mit mindestens 50 km Arbeitsweg
    "d_steuereinnahme", # Steuereinnahmen je Einwohner auf Kreisebene
    "q_HH", # Durchschnittliche Haushaltsgroesse
    "a_hheink_niedrig", # Anteil Haushalte mit niedrigem Einkommen
    "alo", # Arbeitslose als Zaehler fuer den kommunalen Arbeitslosenanteil
    "ewf_1565_ges" # Bevoelkerung von 15 bis unter 65 als Nenner
  ),
  variable = c(
    "bevoelkerung",
    "einwohnerdichte",
    "supermarktEntfernung",
    "alterMean",
    "wanderungssaldo",
    "beschaeftigteWohnort",
    "pendler50",
    "steuereinnahmen",
    "haushaltsgroesseMean",
    "anteilHaushalteNiedrigesEinkommen",
    "arbeitslose",
    "erwerbsfaehigeBevoelkerung"
  ),
  Raumbezug = c(
    "Gemeinden",
    "Gemeinden",
    "Gemeinden",
    "Gemeinden",
    "Gemeinden",
    "Gemeinden",
    "Gemeinden",
    "Kreise",
    "Gemeinden",
    "Gemeinden",
    "Gemeinden",
    "Gemeinden"
  )
)

workflow_kuerzel <- unique(inkar_indikatoren$Kuerzel)

spalten <- c(
  "Bereich",
  "ID",
  "Kuerzel",
  "Indikator",
  "Raumbezug",
  "Kennziffer",
  "Name",
  "Zeitbezug",
  "Wert"
)

# Nur benoetigte Spalten und Indikatoren aus einer grossen INKAR-Datei einlesen.
read_selected_inkar <- function(path, kuerzel) {
  if (.Platform$OS.type == "windows") {
    # findstr verarbeitet nicht-ASCII-Zeichen in Suchmustern unzuverlaessig.
    # Solche Kuerzel werden ueber ihren ASCII-Praefix gesucht und danach in R exakt gefiltert.
    findstr_kuerzel <- sub("[^\\x01-\\x7F].*$", "", kuerzel, perl = TRUE)
    patterns <- ifelse(
      findstr_kuerzel == kuerzel,
      sprintf('/C:";%s;"', findstr_kuerzel),
      sprintf('/C:";%s"', findstr_kuerzel)
    )
    cmd <- paste(
      "findstr",
      paste(patterns, collapse = " "),
      shQuote(normalizePath(path, winslash = "\\"))
    )

    selected <- fread(
      cmd = cmd,
      sep = ";",
      header = FALSE,
      col.names = spalten,
      dec = ",",
      encoding = "UTF-8",
      showProgress = FALSE
    )

    return(selected[Kuerzel %chin% kuerzel])
  }

  fread(
    path,
    sep = ";",
    select = spalten,
    dec = ",",
    encoding = "UTF-8",
    showProgress = FALSE
  )[Kuerzel %in% kuerzel]
}

# Metadaten des aktuellen INKAR-Caches aus Dateien und Indikatorauswahl erzeugen.
make_metadata <- function() {
  list(
    struktur_jahr = struktur_jahr,
    workflow_kuerzel = workflow_kuerzel,
    inkar_indikatoren = as.data.frame(inkar_indikatoren),
    hinweis = paste(
      "Fuer den Regressionsworkflow werden INKAR-Strukturmerkmale als",
      "Niveauwerte des Jahres", struktur_jahr,
      "verwendet; es werden keine Veraenderungen zwischen Wahljahren gebildet."
    )
  )
}

# Indikatorkonfiguration sortieren und fuer einen stabilen Cachevergleich vereinheitlichen.
normalise_indicator_config <- function(config) {
  config <- as.data.table(config)
  config <- config[
    ,
    .(
      Kuerzel = as.character(Kuerzel),
      variable = as.character(variable),
      Raumbezug = as.character(Raumbezug)
    )
  ]
  config[order(Raumbezug, Kuerzel, variable)]
}

# Pruefen, ob die im Cache gespeicherten Indikatoren der aktuellen Auswahl entsprechen.
indicator_config_matches <- function(metadata) {
  if (!all(c("workflow_kuerzel", "inkar_indikatoren") %in% names(metadata))) {
    return(FALSE)
  }

  identical(
    sort(as.character(metadata$workflow_kuerzel)),
    sort(as.character(workflow_kuerzel))
  ) &&
    identical(
      normalise_indicator_config(metadata$inkar_indikatoren),
      normalise_indicator_config(inkar_indikatoren)
    )
}

# INKAR-Daten auf das Strukturjahr und die benoetigten Raumebenen begrenzen.
filter_workflow_rows <- function(inkar_data) {
  inkar_data <- as.data.table(inkar_data)

  merge(
    inkar_data,
    unique(inkar_indikatoren[, .(Kuerzel, Raumbezug)]),
    by = c("Kuerzel", "Raumbezug"),
    all = FALSE,
    sort = FALSE
  )
}

# Kontrollieren, dass alle angeforderten Indikatoren in den eingelesenen Daten vorhanden sind.
required_indicators_present <- function(inkar_data) {
  present <- unique(
    as.data.table(inkar_data)[
      Zeitbezug == struktur_jahr,
      .(Kuerzel, Raumbezug)
    ]
  )

  missing <- fsetdiff(
    unique(inkar_indikatoren[, .(Kuerzel, Raumbezug)]),
    present
  )

  nrow(missing) == 0
}

# Gefilterte INKAR-Basis und ihre Metadaten als RDS-Cache speichern.
write_inkar_basis <- function(inkar_workflow, metadata) {
  saveRDS(inkar_workflow, inkar_basis_rds)
  saveRDS(metadata, inkar_metadata_rds)
}

# Entscheiden, ob vorhandene Cachedateien noch zu Rohdaten und Konfiguration passen.
cache_is_current <- function() {
  if (!file.exists(inkar_basis_rds) || !file.exists(inkar_metadata_rds)) {
    return(FALSE)
  }

  metadata <- readRDS(inkar_metadata_rds)

  if (!"struktur_jahr" %in% names(metadata)) {
    return(FALSE)
  }

  if (!indicator_config_matches(metadata)) {
    return(FALSE)
  }

  inkar_workflow <- readRDS(inkar_basis_rds)

  isTRUE(metadata$struktur_jahr[[1]] == struktur_jahr) &&
    identical(sort(unique(inkar_workflow[["Zeitbezug"]])), struktur_jahr) &&
    required_indicators_present(inkar_workflow)
}

# Vorhandene INKAR-Basis laden oder bei unpassendem Cache neu erzeugen.
load_existing_basis <- function() {
  if (file.exists(inkar_basis_rds)) {
    return(readRDS(inkar_basis_rds))
  }

  if (file.exists(legacy_basis_rds)) {
    return(readRDS(legacy_basis_rds))
  }

  NULL
}

existing_basis <- if (!force_rebuild) {
  load_existing_basis()
} else {
  NULL
}

if (!force_rebuild && cache_is_current()) {
  message("INKAR-Basisdaten sind bereits vorhanden: ", inkar_basis_rds)
} else if (
  !is.null(existing_basis) &&
    required_indicators_present(existing_basis)
) {
  message("Aktualisiere vorhandene INKAR-Basisdaten auf Strukturjahr ", struktur_jahr, ".")

  inkar_workflow <- existing_basis
  inkar_workflow <- filter_workflow_rows(inkar_workflow)
  inkar_workflow <- inkar_workflow[Zeitbezug == struktur_jahr]

  if (nrow(inkar_workflow) == 0) {
    stop(
      "Der vorhandene INKAR-Zwischenstand enthaelt keine Werte fuer ",
      struktur_jahr,
      ". Setze options(waehlendenwanderung.inkar_basis_rebuild = TRUE), ",
      "um die Rohdaten neu einzulesen."
    )
  }

  metadata <- make_metadata()
  write_inkar_basis(inkar_workflow, metadata)
} else {
  message("Lese INKAR-Rohdaten einmalig ein und speichere die kleine Basisdatei.")

  inkar_selected <- read_selected_inkar(
    inkar_pfad,
    workflow_kuerzel
  )

  inkar_selected[, Zeitbezug := as.integer(Zeitbezug)]

  inkar_workflow <- filter_workflow_rows(inkar_selected)
  inkar_workflow[, Kennziffer := sprintf("%08d", as.integer(Kennziffer))]

  inkar_workflow <- inkar_workflow[
    Zeitbezug == struktur_jahr
  ]

  if (nrow(inkar_workflow) == 0) {
    stop(
      "Die INKAR-Rohdaten enthalten fuer die ausgewaehlten Workflow-Indikatoren ",
      "keine Werte fuer ",
      struktur_jahr,
      "."
    )
  }

  metadata <- make_metadata()

  write_inkar_basis(inkar_workflow, metadata)
}

if (!exists("inkar_workflow")) {
  inkar_workflow <- readRDS(inkar_basis_rds)
}
if (!exists("metadata")) {
  metadata <- readRDS(inkar_metadata_rds)
}
