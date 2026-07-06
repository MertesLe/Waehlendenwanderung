inkartest <- read.csv("Data//inkar_2025//inkar_2025.csv", header = TRUE, sep = ";")
View(inkartest)

library(data.table)
# set cores used
setDTthreads(max(1, parallel::detectCores() - 2))
inkar_test <- as.data.table(read.csv("Data//inkar_2025//inkar_2025.csv", header = TRUE, sep = ";"))
inkar_daten <- inkar_test[Zeitbezug %in% c(2021, 2025)]
inkar_daten <- inkar_daten[Raumbezug %in% "Gemeinden"]
# save as RDS
saveRDS(inkar_daten, file = "Data//cleaned//data_inkar.rds")

## Auffälligkeiten Inkardaten
# 1. Bedeutung von ID(uniqueN = 249), Kuerzel (uniqueN = 221), Indikator (uniqueN = 234)
