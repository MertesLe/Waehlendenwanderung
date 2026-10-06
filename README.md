# Waehlendenwanderung

R-Projekt zur Bachelorarbeit "Schätzung struktureller Einflüsse auf die
Wählerwanderung zwischen den
Bundestagswahlen 2021 und 2025 mittels
ökologischer Inferenz".

Absolviert von Leonie Mertes am Institut für Statistik der Ludwig-Maximilian-Universität München (06. Oktober 2026).

Betreut von Prof. Dr. Göran Kauermann.
Mitbetreut von Jan Anders.

## Kurzbeschreibung

Wahlbezirksergebnisse der Jahre 2021 und 2025 werden auf vergleichbare raeumliche Einheiten harmonisiert. Fuer Ostdeutschland ohne Berlin schaetzt `nslphom_dual` lokale Uebergangsmatrizen; anschliessend werden die geschaetzten AfD-Zufluesse aus fuenf Herkunftsgruppen über getrennte gewichtete linere Regressionsmodelle mit regionalen Strukturmerkmalen in Beziehung gesetzt. Ein Bootstrap untersucht die Unsicherheit der Regressionskoeffizienten in der zweistufigen Schätzkette.

## Projektstruktur

```text
Waehlendenwanderung/
  Scripts/          nummerierte Arbeits- und Diagnoseskripte
  Functions/        gemeinsam verwendete R-Funktionen
  Data/raw/         Rohdaten
  Data/cleaned/     harmonisierte Wahl- und Strukturdaten
  Data/modeloutput/ Fits, Regressions- und Bootstrapoutputs
  Data/validierung/ Checks und Validierungsergebnisse
  Charts/           erzeugte Grafiken
  paths.R           zentrale Ausgabepfade
  renv.lock         R-Paketversionen
```

Fuer einen vollstaendigen Neulauf muessen die unten genannten Rohdaten bereitgestellt werden. Ergebnisse unter `Data/cleaned/`, `Data/modeloutput/`, `Data/validierung/` und `Charts/` werden durch die Skripte erzeugt. Erzeugte Datensaetze werden als `.rds` gespeichert.

## Software und Installation

Das Projekt wurde mit **R 4.5.2** und den Paketversionen aus `renv.lock` vorbereitet. Auf einem neuen Rechner:

1. R installieren und `Waehlendenwanderung.Rproj` oeffnen.
2. Die benoetigten Pakete in der R-Konsole wiederherstellen:

```r
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}
renv::restore()
```

Die Datei `.Rprofile` aktiviert die Projektumgebung beim Start, installiert die Projektpakete aber nicht. `renv::restore()` liest `renv.lock` und installiert die dort festgehaltenen Versionen, darunter `dplyr`, `tidyr`, `readxl`, `sf`, `ggplot2`, `lphom`, `lpSolve` und `osqp`. Unter Linux koennen insbesondere fuer `sf` zusaetzliche Systembibliotheken erforderlich sein. Bei einer anderen R-Version als 4.5.2 sollte die Paketwiederherstellung anschliessend besonders sorgfaeltig geprueft werden.

## Eingabedaten

Folgende Eingaben werden fuer den vollstaendigen Workflow erwartet:

```text
Data/raw/gebietsänderungen/*.xlsx
Data/raw/btw21_wbz/btw21_wbz_ergebnisse.csv
Data/raw/btw25_wbz/btw25_wbz_ergebnisse.csv
Data/raw/inkar_2025/inkar_2025.csv
Data/raw/gebiete_visualisierung/vg250_01-01.utm32s.gpkg.ebenen/vg250_ebenen_0101/DE_VG250.gpkg
```

Die Wahlbezirksdaten stammen von der Bundeswahlleiterin, die Strukturindikatoren aus INKAR (BBSR), die Gemeindegeometrien vom BKG. Hinzu kommen die amtlichen Dateien zu Gemeindegebietsveraenderungen.

BKG: Verwaltungsgebiete 1:250 000 (VG250), Stand 01.01.2025
https://gdz.bkg.bund.de/index.php/default/verwaltungsgebiete-1-250-000-stand-01-01-vg250-01-01.html

Die Bundeswahlleiterin: Bundestagswahl 2021
https://www.bundeswahlleiterin.de/bundestagswahlen/2021/ergebnisse/weitere-ergebnisse.html

Die Bundeswahlleiterin: Bundestagswahl 2025
https://www.bundeswahlleiterin.de/bundestagswahlen/2025/ergebnisse/weitere-ergebnisse.html

BBSR: Ausgabe 07/2025
https://www.inkar.de/

Statistisches Bundesamt: 2021–24, Namens- und Gebietsänderungen der Gemeinden
https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Namens-Grenz-Aenderung/namens-grenz-aenderung.html

BKG: Verwaltungsgebiete 1:250 000 (VG250), Stand 01.01.2025
https://gdz.bkg.bund.de/index.php/default/verwaltungsgebiete-1-250-000-stand-01-01-vg250-01-01.html

## Hauptanalyse

Alle `source()`-Befehle aus dem Projektverzeichnis ausfuehren. Die Nummern geben die fachliche Reihenfolge an; `06` ist eine optionale Diagnose und aendert den Wert von `iter_max` im Hauptfit nicht automatisch.

| Schritt | Skript | Aufgabe | Wichtigster Output |
|---|---|---|---|
| 01 | `Scripts/01_mapping_gebiete.R` | Gemeindegebietsveraenderungen zu Aggregationsgruppen verbinden. | `Data/cleaned/mapping_gebietsaenderungen.rds` und gerichtetes Mapping |
| 02 | `Scripts/02_mapping_wahldaten.R` | Wahlbezirke beider Jahre ueber Briefwahl, § 68 BWO und Gebietsstand harmonisieren. | gemappte Wahldaten und finale Zuordnung in `Data/cleaned/` |
| 03 | `Scripts/03_prepare_nslphom_input.R` | Zweitstimmen, Nichtwaehlende und Parteigruppen vorbereiten; 2025 je Einheit auf die 2021-Gesamtmasse skalieren. | `Data/cleaned/nslphom_input_2021.rds`, `nslphom_input_2025.rds` |
| 04 | `Scripts/04_cleaning_strukturdaten.R` | Benötigte INKAR-Indikatoren fuer 2023 aus der Rohdatei extrahieren. | INKAR-Basis und Cache-Metadaten in `Data/cleaned/` |
| 05 | `Scripts/05_prepare_inkar_covariates.R` | Strukturmerkmale auf die Wahleinheiten aggregieren und die Grenzdistanz berechnen. | `Data/cleaned/inkar_kovariaten_2023.rds` |
| 07 | `Scripts/07_estimate_transitions.R` | Ostfit mit sechs Gruppen schaetzen; EHet-Diagnose automatisch erzeugen. | Fit und lokale/globale Matrizen in `Data/modeloutput/nslphom/ostdeutschland/` |
| 08 | `Scripts/08_model_transitions.R` | Fuenf gewichtete lineare Modelle fuer AfD-Zufluesse schaetzen. | Modelldaten, `lm`-Fits und Koeffizienten in `Data/modeloutput/regression/ostdeutschland/` |
| 09 | `Scripts/09_bootstrap_nslphom_regression.R` | Ost-Einheiten mit Zuruecklegen ziehen; pro Stichprobe Inferenz und Regression wiederholen. | Ziehungen, Intervalle und Iterationen in `Data/modeloutput/bootstrap/`; Grafik in `Charts/bootstrap/` |
| 10 | `Scripts/10_analyse_ergebnisse.R` | Hauptmodell und Bootstrap gemeinsam auswerten, ohne erneut zu schaetzen. | `Charts/Regression/ergebnisse/` und Ergebnis-RDS |

Ein vollstaendiger Hauptlauf erfolgt mit:

```r
source("Scripts/01_mapping_gebiete.R", encoding = "UTF-8")
source("Scripts/02_mapping_wahldaten.R", encoding = "UTF-8")
source("Scripts/03_prepare_nslphom_input.R", encoding = "UTF-8")
source("Scripts/04_cleaning_strukturdaten.R", encoding = "UTF-8")
source("Scripts/05_prepare_inkar_covariates.R", encoding = "UTF-8")
source("Scripts/07_estimate_transitions.R", encoding = "UTF-8")
source("Scripts/08_model_transitions.R", encoding = "UTF-8")
source("Scripts/09_bootstrap_nslphom_regression.R", encoding = "UTF-8")
source("Scripts/10_analyse_ergebnisse.R", encoding = "UTF-8")
```

Fuer die optionale `iter_max`-Diagnose nach 05 und vor 07 zusaetzlich `source("Scripts/06_find_optimal_iter_max_nslphom_dual.R", encoding = "UTF-8")` aufrufen. `Scripts/00_run_pipeline.R` ist nur eine **Teillauf-Pipeline** fuer 01 bis 05 sowie 07 und 08. Sie startet weder `06` noch Deutschlandfit, Bootstrap, Simulation oder Ergebnisanalyse. Wer `06` vor dem Fit pruefen will, fuehrt 01 bis 05 deshalb einzeln aus, statt anschliessend `00` noch einmal zu starten.

## Ergaenzende Analysen

Diese Skripte sind nicht Voraussetzung fuer die Ostregression; sie dokumentieren oder pruefen deren methodische Entscheidungen:

| Skript | Voraussetzung und Zweck | Output |
|---|---|---|
| `06_find_optimal_iter_max_nslphom_dual.R` | Nach 03; untersucht HETe fuer verschiedene `iter_max`-Werte auf den Ostdaten. Rechenintensiv. | `Data/validierung/iter_max/`, `Charts/iter_max/` |
| `06a_validate_final_method_dual_osqp.R` | Unabhaengige Drei-Parteien-Simulation mit bekannter Wahrheit und anschliessender Regression. | `Data/validierung/end_to_end_afd_validation/`, `Charts/validierung/end_to_end_afd_validation/` |
| `07a_fit_deutschland_homogenitaet.R` | Nach 03; separater Deutschlandfit als Vergleich fuer die Heterogenitaetsanalyse. Rechenintensiv. | `Data/modeloutput/nslphom/deutschland/`, EHet-Karten |
| `07b_visualize_homogenitaetsannahme.R` | Wird von 07 und 07a standardmaessig automatisch aufgerufen; kein separater Aufruf noetig. | `Data/validierung/homogenitaetsannahme/`, `Charts/homogenitaetsannahme/`, Union-Karten in `Charts/Homogenitaetsannahmentest/` |
| `07d_validate_lokale_matrix_residuen.R` | Nach 07; prueft die Reproduktion beobachteter Zielraender durch lokale Matrizen. | Residuen-RDS und Karte unter `Data/validierung/homogenitaetsannahme/` bzw. `Charts/homogenitaetsannahme/` |
| `07e_plot_global_matrices.R` | Nach 07 und 07a; zeichnet die beiden globalen Matrizen. | Zwei PNGs unter `Charts/` |
| `07f_visualize_tomography.R` | Nach 07; illustriert das Inferenzproblem anhand einer 2x2-Reduktion. | `Charts/Tomography/` |
| `07g_compare_solver_outputs.R` | Nach 07 und einem separaten `lp_solve`-Ostfit; vergleicht gespeicherte Ergebnisse zellgenau. | `Data/validierung/solververgleich/` |
| `07h_compare_nslphom_dual_speed.R` | Nach 03; schaetzt beide Solver-Varianten mehrfach fuer Laufzeit- und Ergebnisvergleich. | `Data/validierung/nslphom_dual_speed_comparison/`, `Charts/Validierung/nslphom_dual_speed_comparison/` |
| `08a_describe_strukturvariablen.R`, `08b_validate_regressionsannahmen.R`, `08c_visualize_leverage_ausschluss.R` | Nach 08; Deskriptivstatistik und Regressionsdiagnostik. | `Charts/Strukturvariablen/`, `Charts/Regression/`, zugehoerige RDS-Dateien |
| `08d_visualize_struktur_und_uebergaenge.R` | Nach 05 und 07; Ostkarten fuer Strukturmerkmale und lokale Uebergaenge. | `Charts/Strukturvariablen/` |

Ein bereits vorhandener `lp_solve`-Ostfit kann fuer `07g` wiederverwendet werden, wenn Eingaben, Parteigruppen, `iter_max` und `tol` mit dem OSQP-Ostfit uebereinstimmen. Fehlt er, wird er in einen **separaten** Ordner geschrieben:

```r
alte_optionen <- options(
  waehlendenwanderung.nslphom_solver = "lp_solve",
  waehlendenwanderung.nslphom_iter_max = 2L,
  waehlendenwanderung.nslphom_tol = 1e-5,
  waehlendenwanderung.ost_nslphom_output_dir =
    "Data/modeloutput/nslphom/ostdeutschland_lp_solve",
  waehlendenwanderung.ost_ehet_run = FALSE
)
tryCatch(
  source("Scripts/07_estimate_transitions.R", encoding = "UTF-8"),
  finally = options(alte_optionen)
)
source("Scripts/07g_compare_solver_outputs.R", encoding = "UTF-8")
```

## Reproduzierbarkeit und Grenzen

- Standard fuer den Ostfit ist die hybride Variante: OSQP schaetzt die globale Startmatrix, `lp_solve` die lokalen Probleme. `iter_max = 2`, OSQP `eps_abs = eps_rel = 1e-3` und `osqp_max_iter = 100000`. Der Deutschlandfit verwendet fuer OSQP `1000000` OSQP-Iterationen. Die Simulation `06a` verwendet dagegen `iter_max = 10`.
- Aktive R-`options()` koennen Defaults ueberschreiben. Die gespeicherten Settings der Fits und des Bootstrap vor gemeinsamen Auswertungen vergleichen. 
- Die Simulation und der Bootstrap setzen Seeds. Der Bootstrap laeuft standardmaessig mit 500 Wiederholungen und speichert jede Iteration separat. Bei einem Neustart werden nur zur aktuellen Konfiguration passende erfolgreiche Iterationen wiederverwendet; geaenderte Einstellungen koennen eine komplette Neuberechnung ausloesen.
- `09` und die beiden grossen Fits koennen erheblich Rechenzeit und Arbeitsspeicher benoetigen. `10` erwartet derzeit 1.078 Einheiten und vollstaendige Bootstrap-Intervalle; bei fehlgeschlagenen Bootstrap-Iterationen kann es abbrechen.
- EHet und lokale Randreproduktion beurteilen die Anpassung an beobachtete Wahlergebnisse. Sie beweisen nicht, dass unbekannte individuelle Uebergaenge richtig rekonstruiert wurden.
