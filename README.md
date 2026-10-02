# Waehlendenwanderung

Ziel dieses Projekts ist die Untersuchung von Wählerwanderungen zwischen den Bundestagswahlen 2021 und 2025 auf möglichst kleinräumiger Ebene. Dafür werden amtliche Wahlbezirksergebnisse, gemeinsame Briefwahlauszählungen, §-68-BWO-Fälle und Gemeindegebietsänderungen zu harmonisierten Gemeindeaggregationen zusammengeführt.

Auf diesen Einheiten werden mit `nslphom` lokale Übergangsmatrizen geschätzt. Anschließend werden vor allem AfD-Zuflüsse 2025 mit regionalen Strukturmerkmalen aus INKAR 2023 in Beziehung gesetzt.

Alle selbst erzeugten Datensätze werden im Workflow als `.rds` gespeichert. CSV-Dateien bleiben nur als Rohdaten relevant.

## Voraussetzungen und Eingabedaten

Die Skripte werden aus dem Projektverzeichnis gestartet. `renv.lock` hält die R-Pakete für R 4.5.2 fest; auf einem neuen Rechner können sie mit `renv::restore()` wiederhergestellt werden. Insbesondere `sf` kann zusätzlich plattformspezifische Systembibliotheken benötigen.

Für einen vollständigen Neulauf müssen diese Dateien im Projektordner liegen:

```text
Data/raw/gebietsänderungen/*.xlsx
Data/raw/btw21_wbz/btw21_wbz_ergebnisse.csv
Data/raw/btw25_wbz/btw25_wbz_ergebnisse.csv
Data/raw/inkar_2025/inkar_2025.csv
Data/raw/gebiete_visualisierung/vg250_01-01.utm32s.gpkg.ebenen/vg250_ebenen_0101/DE_VG250.gpkg
```

`Data/` ist von Git ausgeschlossen. Ein direkt aus dem **vollständigen Projektordner** erstelltes ZIP kann die Daten enthalten; ein Git-Klon oder ein aus dem Repository erzeugtes ZIP enthält sie nicht. Vor der Abgabe deshalb den Inhalt des ZIP prüfen.

## Hauptworkflow

`Scripts/00_run_pipeline.R` ist eine **Teillauf-Pipeline**: Sie führt nur 01 bis 05 sowie 07 und 08 aus. Sie startet weder den Deutschlandfit noch die optionalen Diagnosen, die Simulation, die übrigen Grafiken, den Bootstrap oder die Ergebnisanalyse. `07` erstellt seine EHet-Grafiken über `07b` standardmäßig bereits mit. Ostfit, Deutschlandfit und Bootstrap verwenden ohne abweichende Option `iter_max = 2`; die separate Validierung 06a verwendet `iter_max = 10`.

```r
source("Scripts/00_run_pipeline.R", encoding = "UTF-8")
```

Für die vollständige Auswertung danach `07a`, die gewünschten ergänzenden Grafiken und Validierungen, `09` und zuletzt `10` separat ausführen. Skript `10` setzt derzeit 1.078 ostdeutsche Einheiten und vollständige Bootstrap-Intervalle voraus. Skript `06` ist eine optionale, rechenintensive Diagnose vor dem Fit; bei Verwendung die Schritte 01 bis 05 zuerst einzeln ausführen und anschließend mit 07 fortfahren, statt `00` dazwischen erneut zu starten.

| Reihenfolge | Skriptname | Was es tut | Output |
|--------------------:|-----------------|-----------------|-----------------|
| 1 | `Scripts/01_mapping_gebiete.R` | Liest amtliche Gemeindegebietsänderungen und bildet zusammenhängende Aggregationsgruppen. Gerichtete Alt-neu-Beziehungen bleiben für räumliche Zuordnungen erhalten. | `Data/cleaned/mapping_gebietsaenderungen.rds` und `mapping_gebietsaenderungen_gerichtet.rds`: Aggregationsschlüssel und gerichtete Änderungen. |
| 2 | `Scripts/02_mapping_wahldaten.R` | Harmonisiert Wahlbezirksergebnisse 2021/2025 über Briefwahl, § 68 BWO, Gebietsänderungen und eine manuell validierte Textkorrektur. Beide Wahlen erhalten dieselben finalen Aggregationseinheiten. | `Data/cleaned/wahldaten2021_gemappt.rds`, `wahldaten2025_gemappt.rds` und `mapping_*_final_manuell_validiert.rds`; Textdiagnosen unter `Data/validierung/`. |
| 3 | `Scripts/03_prepare_nslphom_input.R` | Bereitet Zweitstimmen, Nichtwählende und Parteigruppen für `nslphom` auf und skaliert 2025 je Einheit auf die 2021-Gesamtmasse. CDU/CSU werden `Union`, LINKE/GRÜNE `LINKE_GRUENE`. | `Data/cleaned/nslphom_input_2021.rds`, `nslphom_input_2025.rds`, `nslphom_input_long.rds`, `partei_schwellenwerte.rds`; Checks unter `Data/validierung/`. |
| 4 | `Scripts/04_cleaning_strukturdaten.R` | Filtert die große INKAR-Rohdatei auf benötigte Indikatoren und 2023. Speichert eine kleine wiederverwendbare Basis mit Metadaten. | `Data/cleaned/inkar_basis_gemeinden_kreise.rds`, `inkar_basis_metadata.rds`. |
| 5 | `Scripts/05_prepare_inkar_covariates.R` | Aggregiert INKAR-Indikatoren fachlich gewichtet auf die finalen Einheiten. Berechnet zusätzlich die Grenzdistanz für Ostdeutschland ohne Berlin. | `Data/cleaned/inkar_agg_long.rds`, `inkar_kovariaten_2023.rds`. |
| 6 (optional, vor 7) | `Scripts/06_find_optimal_iter_max_nslphom_dual.R` | Berechnet den HETe-Verlauf für die Oststichprobe. Das Skript setzt `iter_max` im Hauptfit nicht automatisch; dessen Standard bleibt 2. | `Data/validierung/iter_max/`: Sequenzen und Minimum als RDS; `Charts/iter_max/`: Verlaufsdiagramme. |
| 7 | `Scripts/07_estimate_transitions.R` | Schätzt den Ost-`nslphom_dual`-Fit ohne Berlin. Standardmäßig nutzt er OSQP global und `lp_solve` lokal; die EHet-Auswertung 07b wird bei aktiviertem `ost_ehet_run` automatisch aufgerufen. | `Data/modeloutput/nslphom/ostdeutschland/`: Fit, lokale und globale Matrizen, Settings, Checks; EHet-Diagnose unter `Data/validierung/homogenitaetsannahme/` und `Charts/homogenitaetsannahme/`. |
| 8 | `Scripts/08_model_transitions.R` | Schätzt für jede Herkunftsgruppe ein eigenes gewichtetes lineares Modell des AfD-Zuflusses mit Strukturmerkmalen 2023. Speichert vollständige `lm`-Objekte für die Diagnostik. | `Data/modeloutput/regression/ostdeutschland/`: Modelldaten, Fits, Koeffizienten und Checks. |
| 9 (separat, guter PC) | `Scripts/09_bootstrap_nslphom_regression.R` | Zieht Ost-Einheiten mit Zurücklegen, schätzt je Stichprobe `nslphom_dual` und danach dieselben AfD-Zuflussmodelle. Die Hauptpipeline startet diesen rechenintensiven Schritt nicht automatisch. | `Data/modeloutput/bootstrap/`: Beta-Ziehungen, Intervalle, Checks, Settings und `iterations/`; `Charts/bootstrap/`: Beta-Verteilungen. |
| 10 (nach 8 und 9) | `Scripts/10_analyse_ergebnisse.R` | Vergleicht Hauptkoeffizienten mit den Bootstrapintervallen und beschreibt Einheiten mit erhöhten AfD-Übergängen. Erstellt Koeffizientengrafiken, Strukturprofile und paarweise Ost-Karten ohne erneuten Modellfit. | `Data/modeloutput/regression/ostdeutschland/ergebnisanalyse_afd_zufluss.rds`; Grafiken unter `Charts/Regression/ergebnisse/`. |

## Ergänzende Skripte

Die Buchstaben kennzeichnen optionale Diagnosen und Grafiken; die Spalte „Nach Schritt“ gibt an, welche Daten zuerst vorliegen müssen. Schritte 07a und 07 können unabhängig voneinander nach 03 laufen; 06 ist optional. Die Flussgrafik in 07e benötigt beide Fits. Diese ergänzenden Skripte sind nicht direkt Teil von `00_run_pipeline.R`; 07b wird bei aktivierter EHet-Option jedoch von 07 beziehungsweise 07a aufgerufen.

| Nach Schritt | Skriptname | Was es tut | Output |
|--------------------:|-----------------|-----------------|-----------------|
| unabhängig | `Scripts/06a_validate_final_method_dual_osqp.R` | Simuliert Drei-Parteien-Wahlen mit festgelegten moderaten lokalen Unterschieden und bekannten AfD-Zufluss-Betas. Prüft hybrides `nslphom_dual` und die anschließende gewichtete Regression. | `Data/validierung/end_to_end_afd_validation/`: lokale Fehler, EI, Beta-Recovery und Einstellungen als RDS; `Charts/validierung/end_to_end_afd_validation/`: Grafiken. |
| 7, guter PC | `Scripts/07a_fit_deutschland_homogenitaet.R` | Schätzt die nationale Dual-Matrix als Vergleich zum Ostfit. Ruft die EHet-Auswertung 07b für den Ost-West-Vergleich auf. | `Data/modeloutput/nslphom/deutschland/`: Fit und Matrizen; `Data/validierung/homogenitaetsannahme/`, `Charts/homogenitaetsannahme/`: EHet-Diagnose. |
| 7 oder 7a | `Scripts/07b_visualize_homogenitaetsannahme.R` | Berechnet den relativen EHet des angegebenen Fits und visualisiert auch die signierte Union-Zielabweichung. Wird bei aktivierter EHet-Option durch 07 und 07a bereits automatisch aufgerufen. | `Data/validierung/homogenitaetsannahme/`: Kennzahlen; `Charts/homogenitaetsannahme/`: allgemeine Karte und Diagnoseplots (Ost: `ostdeutschland_ohne_berlin_ehet_verteilung_boxplot.png`, Deutschland: `deutschland_ehet_ost_west_boxplot.png`); `Charts/Homogenitaetsannahmentest/`: Union-Karte. |
| 7a | `Scripts/07c_visualize_afd_homogenitaetsannahme.R` | Visualisiert den AfD-bezogenen EHet der gespeicherten Schätzung. Für andere Fits kann der Inputpfad per Option gesetzt werden. | `Charts/Homogenitaetsannahmentest/deutschland_afd_ehet_deutschlandkarte_agg.png`: AfD-EHet-Karte. |
| 7 | `Scripts/07d_validate_lokale_matrix_residuen.R` | Vergleicht die lokalen geschätzten Zielstimmen mit den beobachteten Rändern. Es ist eine Anpassungsdiagnose, kein Nachweis korrekter individueller Übergänge. | `Data/validierung/homogenitaetsannahme/`: lokale Residuenkennzahlen; `Charts/homogenitaetsannahme/`: Residuenplots und Karte. |
| 7 und 7a | `Scripts/07e_plot_global_matrices.R` | Zeichnet Ost- und Deutschlandmatrix als Flussgrafiken mit 2021 links und 2025 rechts. Beide gespeicherten Fits sind erforderlich. | `Charts/nslphom_global_matrix_ostdeutschland_ungeblockt.png`, `nslphom_global_matrix_deutschland_ungeblockt.png`, `nslphom_global_matrizen_ungeblockt.pdf`. |
| 7 | `Scripts/07f_visualize_tomography.R` | Illustriert die Unterbestimmtheit anhand einer auf 2×2 reduzierten Darstellung der Ost-Daten. Die tatsächliche Schätzung bleibt das vollständige 6×6-Modell. | `Charts/Tomography/`: Linien- und Schätzprinzipgrafik als PNG. |
| 7 plus separater `lp_solve`-Fit | `Scripts/07g_compare_solver_outputs.R` | Vergleicht die gespeicherten Endoutputs von Hybrid-OSQP und reinem `lp_solve` zellgenau. Prüft dabei Einheitenreihenfolge, Parteischwelle, `iter_max` und Toleranz. | `Data/validierung/solververgleich/`: Identitätschecks und Abweichungen als RDS. |
| 3, guter PC | `Scripts/07h_compare_nslphom_dual_speed.R` | Schätzt dieselben Ost-Inputs mehrfach mit Hybrid-OSQP und reinem `lp_solve`. Vergleicht Laufzeit, Speicherbedarf, Matrizen und HETe. | `Data/validierung/nslphom_dual_speed_comparison/`: Vergleichstabellen; `Charts/Validierung/nslphom_dual_speed_comparison/`: Plots. |
| 8 | `Scripts/08a_describe_strukturvariablen.R` | Beschreibt die im Regressionsinput verwendeten Strukturvariablen und Übergangswahrscheinlichkeiten. Zeigt Verteilungen, Scatterplots, Boxplots und Korrelationen. | `Data/modeloutput/regression/ostdeutschland/deskriptiv/`: Kennzahlen; `Charts/Strukturvariablen/`: PNG-Grafiken. |
| 8 | `Scripts/08b_validate_regressionsannahmen.R` | Prüft Linearität, Residuenverteilung, Varianzkonstanz, Multikollinearität und einflussreiche Einheiten der AfD-Zuflussmodelle. Die räumliche Unabhängigkeit bleibt eine gesonderte methodische Einschränkung. | `Data/modeloutput/regression/ostdeutschland/regressionsdiagnostik/`: Kennzahlen; `Charts/Regression/regressionsdiagnostik/`: PNG-Grafiken. |
| 8 | `Scripts/08c_visualize_leverage_ausschluss.R` | Untersucht die Sensitivität der Regressionskoeffizienten beim Ausschluss auffälliger Einheiten. Kennzeichnet diese Einheiten in einer Ostkarte. | `Data/validierung/regression_ostdeutschland/leverage_sensitivitaet_afd_zufluss.rds`; `Charts/Regression/leverage_ausschluss/`: Vergleichsplots und Karte. |
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

`iter_max = 2L` entspricht dem Standard des Hauptfits. Nach einem Vergleichslauf geänderte Optionen für reguläre Läufe zurücksetzen.

## End-to-End-Validierung

`06a` kann unabhaengig von den empirischen Wahl- und INKAR-Daten laufen. Der Standardlauf verwendet 150 Simulationen, 120 Einheiten und `iter_max = 10`:

```r
source("Scripts/06a_validate_final_method_dual_osqp.R", encoding = "UTF-8")
```

Die aktuellen Grafiken liegen direkt unter `Charts/validierung/end_to_end_afd_validation/`. Zwei Bilder eines früheren Laufs mit denselben Dateinamen bleiben unter `Charts/validierung/end_to_end_afd_validation_alt/` erhalten und werden von `06a` nicht überschrieben.

Die vorgegebenen Koeffizienten bestimmen die lokale Uebergangsheterogenitaet; die Grundniveaus bleiben separat festgelegt. Ob diese simulierte Heterogenitaet der unbekannten realen Heterogenitaet entspricht, ist nicht bekannt.

Zuerst `final_validation_run_status.rds` auf Fehllauefe pruefen. In `final_validation_beta_summary.rds` soll `true_probability` praktisch null Bias haben; `realized_probability` misst die zusaetzliche Zufallsstreuung, `estimated_probability` die Recovery nach oekologischer Inferenz. Die Grafiken verwenden Rot fuer den vorgegebenen Beta-Wert und Blau fuer die aus den Simulationen geschaetzten Betas. Die gezeigten 95-%-Bereiche sind Quantile ueber erfolgreiche Simulationen, keine Konfidenzintervalle des empirischen Ostmodells. Das reduzierte 3x3-Szenario mit zwei Kovariaten ersetzt keine Validierung des empirischen 6x6-Fits mit zehn Kovariaten.

## Frühere Tests

Frühere `test2000`-, `testSymphony`- und einfache `nslphom`-Versuche können noch als gespeicherte Outputs oder Charts vorliegen. Die zugehörigen unnummerierten Testskripte sind **nicht mehr in `Scripts/` vorhanden**. Diese Altdateien werden von der oben beschriebenen Analyse nicht benötigt und durch den aktuellen Workflow nicht neu erzeugt.

## Funktionsdateien

Die Dateien in `Functions/` werden nicht direkt ausgeführt. Sie bündeln wiederkehrende Hilfslogik für AGS-Verarbeitung, nslphom-Input, nslphom-Schätzungen, Regression und Bootstrap, damit dieselbe Logik nicht mehrfach in verschiedenen Skripten definiert wird.
