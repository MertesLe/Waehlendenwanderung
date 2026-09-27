# Waehlendenwanderung

Ziel dieses Projekts ist die Untersuchung von Wählerwanderungen zwischen den Bundestagswahlen 2021 und 2025 auf möglichst kleinräumiger Ebene. Dafür werden amtliche Wahlbezirksergebnisse, gemeinsame Briefwahlauszählungen, §-68-BWO-Fälle und Gemeindegebietsänderungen zu harmonisierten Gemeindeaggregationen zusammengeführt.

Auf diesen Einheiten werden mit `nslphom` lokale Übergangsmatrizen geschätzt. Anschließend werden vor allem AfD-Zuflüsse 2025 mit regionalen Strukturmerkmalen aus INKAR 2023 in Beziehung gesetzt.

Alle selbst erzeugten Datensätze werden im Workflow als `.rds` gespeichert. CSV-Dateien bleiben nur als Rohdaten relevant.

## Hauptworkflow

`Scripts/run_pipeline.R` führt die Schritte 01 bis 05 sowie 07 und 08 gesammelt aus. Skript 06 ist eine optionale, rechenintensive Diagnose: Sein Ergebnis muss vor Schritt 07 geprüft und der gewählte Wert als `waehlendenwanderung.nslphom_iter_max` gesetzt werden. Der Deutschlandfit (07a), Grafiken, Validierungen und Bootstrap laufen separat.

| Reihenfolge | Skriptname | Was es tut | Output |
|--------------------:|-----------------|-----------------|-----------------|
| 1 | `Scripts/01_mapping_gebiete.R` | Liest amtliche Gemeindegebietsänderungen und bildet zusammenhängende Aggregationsgruppen. Gerichtete Alt-neu-Beziehungen bleiben für räumliche Zuordnungen erhalten. | `Data/cleaned/mapping_gebietsaenderungen.rds` und `mapping_gebietsaenderungen_gerichtet.rds`: Aggregationsschlüssel und gerichtete Änderungen. |
| 2 | `Scripts/02_mapping_wahldaten.R` | Harmonisiert Wahlbezirksergebnisse 2021/2025 über Briefwahl, § 68 BWO, validierte Textfälle und Gebietsänderungen. Beide Wahlen erhalten dieselben finalen Aggregationseinheiten. | `Data/cleaned/wahldaten2021_gemappt.rds`, `wahldaten2025_gemappt.rds` und `mapping_*_final_manuell_validiert.rds`; Textdiagnosen unter `Data/validierung/`. |
| 3 | `Scripts/03_prepare_nslphom_input.R` | Bereitet Zweitstimmen, Nichtwählende und Parteigruppen für `nslphom` auf und skaliert 2025 je Einheit auf die 2021-Gesamtmasse. CDU/CSU werden `Union`, LINKE/GRÜNE `LINKE_GRUENE`. | `Data/cleaned/nslphom_input_2021.rds`, `nslphom_input_2025.rds`, `nslphom_input_long.rds`, `partei_schwellenwerte.rds`; Checks unter `Data/validierung/`. |
| 4 | `Scripts/04_cleaning_strukturdaten.R` | Filtert die große INKAR-Rohdatei auf benötigte Indikatoren und 2023. Speichert eine kleine wiederverwendbare Basis mit Metadaten. | `Data/intermediate/inkar_basis_gemeinden_kreise.rds`, `inkar_basis_metadata.rds`. |
| 5 | `Scripts/05_prepare_inkar_covariates.R` | Aggregiert INKAR-Indikatoren fachlich gewichtet auf die finalen Einheiten. Berechnet zusätzlich die Grenzdistanz für Ostdeutschland ohne Berlin. | `Data/cleaned/inkar_agg_long.rds`, `inkar_kovariaten_2023.rds`. |
| 6 (optional, vor 7) | `Scripts/06_find_optimal_iter_max_nslphom_dual.R` | Berechnet den HETe-Verlauf für die Oststichprobe. Der gewählte `iter_max` wird anschließend bewusst für den Fit festgelegt; das Skript setzt ihn nicht automatisch. | `Data/validierung/iter_max/`: Sequenzen und Minimum als RDS; `Charts/iter_max/`: Verlaufsdiagramme. |
| 7 | `Scripts/07_estimate_transitions.R` | Schätzt den Ost-`nslphom_dual`-Fit ohne Berlin. Standardmäßig nutzt er OSQP global und `lp_solve` lokal; die EHet-Auswertung 07b wird bei aktiviertem `ost_ehet_run` automatisch aufgerufen. | `Data/modeloutput/nslphom/ostdeutschland/`: Fit, lokale und globale Matrizen, Settings, Checks; EHet-Diagnose unter `Data/validierung/homogenitaetsannahme/` und `Charts/homogenitaetsannahme/`. |
| 8 | `Scripts/08_model_transitions.R` | Schätzt für jede Herkunftsgruppe ein eigenes gewichtetes lineares Modell des AfD-Zuflusses mit Strukturmerkmalen 2023. Speichert vollständige `lm`-Objekte für die Diagnostik. | `Data/modeloutput/regression/ostdeutschland/`: Modelldaten, Fits, Koeffizienten und Checks. |
| 9 (separat, guter PC) | `Scripts/09_bootstrap_nslphom_regression.R` | Zieht Ost-Einheiten mit Zurücklegen, schätzt je Stichprobe `nslphom_dual` und danach dieselben AfD-Zuflussmodelle. Die Hauptpipeline startet diesen rechenintensiven Schritt nicht automatisch. | `Data/modeloutput/bootstrap/ostdeutschland/`: Beta-Ziehungen, Intervalle, Checks, Settings und Iterationsdateien; `Charts/bootstrap/ostdeutschland/`: Beta-Verteilungen. |

## Ergänzende Skripte

Die Buchstaben ordnen optionale Diagnosen und Grafiken unmittelbar nach dem letzten benötigten Hauptschritt ein. Schritte 07a und 07 können unabhängig voneinander nach 03 und 06 laufen; die nationale Flussgrafik in 07e benötigt beide Fits.

| Nach Schritt | Skriptname | Was es tut | Output |
|--------------------:|-----------------|-----------------|-----------------|
| 6 | `Scripts/06a_validate_final_method_dual_osqp.R` | Validiert die hybride Dual-Methode in Drei-Parteien-Simulationen mit bekannten Übergängen. Bewertet lokale Fehler, EI und die Wiedergewinnung der AfD-Koeffizienten. | `Data/validierung/final_method_dual_osqp/`: Simulationsresultate, Fehlermaße und Settings als RDS. |
| 7, guter PC | `Scripts/07a_fit_deutschland_homogenitaet.R` | Schätzt die nationale Dual-Matrix als Vergleich zum Ostfit. Ruft die EHet-Auswertung 07b für den Ost-West-Vergleich auf. | `Data/modeloutput/nslphom/deutschland/`: Fit und Matrizen; `Data/validierung/homogenitaetsannahme/`, `Charts/homogenitaetsannahme/`: EHet-Diagnose. |
| 7 oder 7a | `Scripts/07b_visualize_homogenitaetsannahme.R` | Berechnet und visualisiert den relativen EHet des angegebenen Fits. Wird bei aktivierter EHet-Option durch 07 und 07a bereits automatisch aufgerufen. | `Data/validierung/homogenitaetsannahme/`: Kennzahlen; `Charts/homogenitaetsannahme/`: Karten und Diagnoseplots. |
| 7a | `Scripts/07c_visualize_afd_homogenitaetsannahme.R` | Visualisiert den AfD-bezogenen EHet der gespeicherten Schätzung. Für andere Fits kann der Inputpfad per Option gesetzt werden. | `Charts/Homogenitaetsannahmentest/deutschland_afd_ehet_deutschlandkarte_agg.png`: AfD-EHet-Karte. |
| 7 | `Scripts/07d_validate_lokale_matrix_residuen.R` | Vergleicht die lokalen geschätzten Zielstimmen mit den beobachteten Rändern. Es ist eine Anpassungsdiagnose, kein Nachweis korrekter individueller Übergänge. | `Data/validierung/`: lokale Residuenkennzahlen; `Charts/homogenitaetsannahme/`: Residuenplots und Karte. |
| 7 und 7a | `Scripts/07e_plot_global_matrices.R` | Zeichnet Ost- und Deutschlandmatrix als Flussgrafiken mit 2021 links und 2025 rechts. Beide gespeicherten Fits sind erforderlich. | `Charts/nslphom_global_matrix_ostdeutschland_ungeblockt.png`, `nslphom_global_matrix_deutschland_ungeblockt.png`, `nslphom_global_matrizen_ungeblockt.pdf`. |
| 7 | `Scripts/07f_visualize_tomography.R` | Illustriert die Unterbestimmtheit anhand einer auf 2×2 reduzierten Darstellung der Ost-Daten. Die tatsächliche Schätzung bleibt das vollständige 6×6-Modell. | `Charts/Tomography/`: Linien- und Schätzprinzipgrafik als PNG. |
| 7 plus separater `lp_solve`-Fit | `Scripts/07g_compare_solver_outputs.R` | Vergleicht die gespeicherten Endoutputs von Hybrid-OSQP und reinem `lp_solve` zellgenau. Prüft dabei Einheitenreihenfolge, Parteischwelle, `iter_max` und Toleranz. | `Data/validierung/solververgleich/`: Identitätschecks und Abweichungen als RDS. |
| 3, guter PC | `Scripts/07h_compare_nslphom_dual_speed.R` | Schätzt dieselben Ost-Inputs mehrfach mit Hybrid-OSQP und reinem `lp_solve`. Vergleicht Laufzeit, Speicherbedarf, Matrizen und HETe. | `Data/validierung/nslphom_dual_speed_comparison/`: Vergleichstabellen; `Charts/Validierung/nslphom_dual_speed_comparison/`: Plots. |
| 8 | `Scripts/08a_describe_strukturvariablen.R` | Beschreibt die im Regressionsinput verwendeten Strukturvariablen und Übergangswahrscheinlichkeiten. Zeigt Verteilungen, Scatterplots, Boxplots und Korrelationen. | `Data/modeloutput/regression/ostdeutschland/deskriptiv/`: Kennzahlen; `Charts/Strukturvariablen/deskriptiv/`: PNG-Grafiken. |
| 8 | `Scripts/08b_validate_regressionsannahmen.R` | Prüft Linearität, Residuenverteilung, Varianzkonstanz, Multikollinearität und einflussreiche Einheiten der AfD-Zuflussmodelle. Die räumliche Unabhängigkeit bleibt eine gesonderte methodische Einschränkung. | `Data/modeloutput/regression/ostdeutschland/regressionsdiagnostik/`: Kennzahlen; `Charts/Regression/ostdeutschland/regressionsdiagnostik/`: PNG-Grafiken. |
| 8 | `Scripts/08c_visualize_leverage_ausschluss.R` | Untersucht die Sensitivität der Regressionskoeffizienten beim Ausschluss auffälliger Einheiten. Kennzeichnet diese Einheiten in einer Ostkarte. | `Data/validierung/regression_ostdeutschland/leverage_sensitivitaet_afd_zufluss.rds`; `Charts/Regression/ostdeutschland/leverage_ausschluss/`: Vergleichsplots und Karte. |
| 5 und 7 | `Scripts/08d_visualize_struktur_und_uebergaenge.R` | Kartiert ausgewählte Strukturvariablen und lokale Übergänge für die ostdeutschen Flächenländer. Vereinigt zusammengesetzte Aggregationsschlüssel über Gemeindegeometrien. | `Charts/Strukturvariablen/`: eine PNG-Karte je ausgewählter Variable bzw. Übergang. |

Für 07g zunächst einen separaten `lp_solve`-Fit mit denselben Einstellungen wie in `Data/modeloutput/nslphom/ostdeutschland/nslphom_settings.rds` erzeugen:

```r
options(
  waehlendenwanderung.nslphom_solver = "lp_solve",
  waehlendenwanderung.nslphom_iter_max = 2L,
  waehlendenwanderung.nslphom_tol = 1e-5,
  waehlendenwanderung.ost_nslphom_output_dir = "Data/modeloutput/nslphom/ostdeutschland_lp_solve",
  waehlendenwanderung.ost_ehet_run = FALSE
)
source("Scripts/07_estimate_transitions.R", encoding = "UTF-8")
source("Scripts/07g_compare_solver_outputs.R", encoding = "UTF-8")
```

`iter_max = 2L` ist ein Beispiel; vor dem Start die Einstellungen des Hauptfits abgleichen und danach geänderte Optionen für reguläre Läufe zurücksetzen.

## Historische Tests

Diese unnummerierten Skripte werden im aktuellen Hauptworkflow nicht mehr gebraucht und bleiben nur für ältere Sensitivitäts- und Belastungstests erhalten. Keines der oben genannten aktuellen Charts wird aus ihnen erzeugt.

| Skriptname | Zweck | Output |
|-----------------|-----------------|-----------------|
| `Scripts/validate_nslphom_simulation.R` | Frühere Simulation des einfachen `nslphom` mit Block- und Gruppierungssensitivität. Die finale Hybrid-Dual-Validierung steht in 06a. | `Data/validierung/nslphom_validation_*.rds`, `nslphom_sensitivity_*.rds`. |
| `Scripts/validate_nslphom_large_unblocked.R` | Frühere große Simulation des einfachen `nslphom`. Sie ist ein Belastungstest, nicht die Validierung des finalen Dual-Verfahrens. | `Data/validierung/large_unblocked/`. |
| `Scripts/estimate_nslphom_unblocked.R` | Früherer separater unblocked Fit außerhalb des Ost-Hauptoutputs. Der finale Ost- und Deutschlandfit erfolgt in 07 und 07a. | `Data/modeloutput/nslphom_unblocked/`. |
| `Scripts/test2000_nslphom_unblocked.R`, `Scripts/test2000_nslphom_dual.R` | Frühere Teilstichproben- und Speicherbelastungstests. Sie werden von keinem aktuellen Chart-Skript benötigt. | Testoutputs unter `Data/modeloutput/nslphom/`. |
| `Scripts/checks etc.R` | Alte interaktive Zusatzchecks zum Wahlmapping. Nicht Teil des reproduzierbaren Hauptlaufs. | Kein festgelegter Output. |

## Funktionsdateien

Die Dateien in `Functions/` werden nicht direkt ausgeführt. Sie bündeln wiederkehrende Hilfslogik für AGS-Verarbeitung, nslphom-Input, nslphom-Schätzungen, Regression und Bootstrap, damit dieselbe Logik nicht mehrfach in verschiedenen Skripten definiert wird.
