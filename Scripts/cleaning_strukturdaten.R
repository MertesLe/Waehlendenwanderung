library(data.table)

dir.create("Data/cleaned", recursive = TRUE, showWarnings = FALSE)

setDTthreads(max(1, parallel::detectCores() - 2))

inkar_pfad <- "Data/raw/inkar_2025/inkar_2025.csv"

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

saveRDS(inkar_workflow, "Data/cleaned/vorlaeufig_inkar_workflow_rohdaten.rds")
saveRDS(metadata, "Data/cleaned/vorlaeufig_inkar_workflow_metadata.rds")

fwrite(
  inkar_workflow,
  "Data/cleaned/vorlaeufig_inkar_workflow_rohdaten.csv"
)
fwrite(
  metadata,
  "Data/cleaned/vorlaeufig_inkar_workflow_metadata.csv"
)
