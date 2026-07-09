library(data.table)

# set cores used
setDTthreads(max(1, parallel::detectCores() - 2))

inkar_pfad <- "Data//raw//inkar_2025//inkar_2025.csv"
inkar_test <- fread(
  inkar_pfad,
  sep = ";",
  header = TRUE,
  showProgress = FALSE
)

inkar_daten <- inkar_test[Zeitbezug %in% c(2021, 2025)]
inkar_daten <- inkar_daten[Raumbezug %in% "Gemeinden"]

# save as RDS
dir.create("Data//cleaned", recursive = TRUE, showWarnings = FALSE)
saveRDS(inkar_daten, file = "Data//cleaned//data_inkar.rds")

## Auffälligkeiten Inkardaten
# 1. Bedeutung von ID(uniqueN = 249), Kuerzel (uniqueN = 221), Indikator (uniqueN = 234)
