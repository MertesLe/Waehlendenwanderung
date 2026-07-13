library(data.table)

source("paths.R", encoding = "UTF-8")

ensure_data_dirs()

setDTthreads(max(1, parallel::detectCores() - 2))

inkar_pfad <- "Data/raw/inkar_2025/inkar_2025.csv"
inkar_basis_rds <- file.path(data_dir_intermediate, "vorlaeufig_inkar_basis_gemeinden_kreise.rds")
inkar_basis_csv <- file.path(data_dir_intermediate, "vorlaeufig_inkar_basis_gemeinden_kreise.csv")
inkar_metadata_rds <- file.path(data_dir_intermediate, "vorlaeufig_inkar_basis_metadata.rds")
inkar_metadata_csv <- file.path(data_dir_intermediate, "vorlaeufig_inkar_basis_metadata.csv")

legacy_basis_rds <- file.path(data_dir_cleaned, "vorlaeufig_inkar_workflow_rohdaten.rds")
legacy_metadata_rds <- file.path(data_dir_cleaned, "vorlaeufig_inkar_workflow_metadata.rds")
force_rebuild <- isTRUE(getOption("waehlendenwanderung.inkar_basis_rebuild", FALSE))

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

if (!force_rebuild && file.exists(inkar_basis_rds) && file.exists(inkar_metadata_rds)) {
  message("INKAR-Basisdaten sind bereits vorhanden: ", inkar_basis_rds)
} else if (!force_rebuild && file.exists(legacy_basis_rds) && file.exists(legacy_metadata_rds)) {
  message("Migriere vorhandene INKAR-Basisdaten nach ", data_dir_intermediate, ".")

  inkar_workflow <- readRDS(legacy_basis_rds)
  metadata <- readRDS(legacy_metadata_rds)

  saveRDS(inkar_workflow, inkar_basis_rds)
  saveRDS(metadata, inkar_metadata_rds)

  fwrite(
    inkar_workflow,
    inkar_basis_csv
  )
  fwrite(
    metadata,
    inkar_metadata_csv
  )
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

  jahr_vorwahl <- 2021L
  jahr_nachwahl_proxy <- max(
    inkar_workflow[Zeitbezug <= 2025, Zeitbezug],
    na.rm = TRUE
  )

  inkar_workflow <- inkar_workflow[
    Zeitbezug %in% c(jahr_vorwahl, jahr_nachwahl_proxy)
  ]

  metadata <- data.table(
    jahr_vorwahl = jahr_vorwahl,
    jahr_nachwahl_proxy = jahr_nachwahl_proxy,
    hinweis = paste(
      "INKAR 2025 enthaelt fuer diese Workflow-Indikatoren keine Werte fuer 2025;",
      "als Naeherung fuer die Struktur zum Zeitpunkt der Wahl 2025 wird das",
      "neueste verfuegbare Jahr bis 2025 verwendet."
    )
  )

  saveRDS(inkar_workflow, inkar_basis_rds)
  saveRDS(metadata, inkar_metadata_rds)

  fwrite(
    inkar_workflow,
    inkar_basis_csv
  )
  fwrite(
    metadata,
    inkar_metadata_csv
  )
}

if (!exists("inkar_workflow")) {
  inkar_workflow <- readRDS(inkar_basis_rds)
}
if (!exists("metadata")) {
  metadata <- readRDS(inkar_metadata_rds)
}
