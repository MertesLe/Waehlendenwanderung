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

workflow_kuerzel <- c(
  "xbev",
  "q_arbeitslosigkeit",
  "q_kaufkraft",
  "a_ausl_bev"
)

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

read_selected_inkar <- function(path, kuerzel) {
  if (.Platform$OS.type == "windows") {
    patterns <- paste(sprintf('/C:";%s;"', kuerzel), collapse = " ")
    cmd <- paste(
      "findstr",
      patterns,
      shQuote(normalizePath(path, winslash = "\\"))
    )

    return(
      fread(
        cmd = cmd,
        sep = ";",
        header = FALSE,
        col.names = spalten,
        dec = ",",
        encoding = "UTF-8",
        showProgress = FALSE
      )
    )
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

make_metadata <- function() {
  data.table(
    struktur_jahr = struktur_jahr,
    hinweis = paste(
      "Fuer den Regressionsworkflow werden INKAR-Strukturmerkmale als",
      "Niveauwerte des Jahres", struktur_jahr,
      "verwendet; es werden keine Veraenderungen zwischen Wahljahren gebildet."
    )
  )
}

write_inkar_basis <- function(inkar_workflow, metadata) {
  saveRDS(inkar_workflow, inkar_basis_rds)
  saveRDS(metadata, inkar_metadata_rds)
}

cache_is_current <- function() {
  if (!file.exists(inkar_basis_rds) || !file.exists(inkar_metadata_rds)) {
    return(FALSE)
  }

  metadata <- readRDS(inkar_metadata_rds)

  if (!"struktur_jahr" %in% names(metadata)) {
    return(FALSE)
  }

  inkar_workflow <- readRDS(inkar_basis_rds)

  isTRUE(metadata$struktur_jahr[[1]] == struktur_jahr) &&
    identical(sort(unique(inkar_workflow[["Zeitbezug"]])), struktur_jahr)
}

load_existing_basis <- function() {
  if (file.exists(inkar_basis_rds)) {
    return(readRDS(inkar_basis_rds))
  }

  if (file.exists(legacy_basis_rds)) {
    return(readRDS(legacy_basis_rds))
  }

  NULL
}

if (!force_rebuild && cache_is_current()) {
  message("INKAR-Basisdaten sind bereits vorhanden: ", inkar_basis_rds)
} else if (!force_rebuild && !is.null(load_existing_basis())) {
  message("Aktualisiere vorhandene INKAR-Basisdaten auf Strukturjahr ", struktur_jahr, ".")

  inkar_workflow <- load_existing_basis()
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
  inkar_selected[, Kennziffer := sprintf("%08d", as.integer(Kennziffer))]

  inkar_workflow <- inkar_selected[
    (
      Raumbezug == "Gemeinden" &
        Kuerzel %in% c("xbev", "q_arbeitslosigkeit", "q_kaufkraft")
    ) |
      (
        Raumbezug == "Kreise" &
          Kuerzel == "a_ausl_bev"
      )
  ]

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
