# Waehlendenwanderung

Ziel dieses Projekts ist die Untersuchung von Wählerwanderungen zwischen den Bundestagswahlen 2021 und 2025 auf möglichst kleinräumiger Ebene. Dafür werden amtliche Wahlbezirksergebnisse, gemeinsame Briefwahlauszählungen, §-68-BWO-Fälle und Gemeindegebietsänderungen zu harmonisierten Gemeindeaggregationen zusammengeführt.

Auf diesen Einheiten werden mit `nslphom` lokale Übergangsmatrizen geschätzt. Anschließend werden vor allem AfD-Zuflüsse 2025 mit regionalen Strukturmerkmalen aus INKAR 2023 in Beziehung gesetzt.

Alle selbst erzeugten Datensätze werden im Workflow als `.rds` gespeichert. CSV-Dateien bleiben nur als Rohdaten relevant.

## Hauptworkflow

`Scripts/00_run_pipeline.R` kann den Hauptworkflow gesammelt ausführen. Für Kontrolle und Fehlersuche ist es aber meistens besser, die Skripte einzeln in der folgenden Reihenfolge laufen zu lassen.

| Reihenfolge | Skriptname | Was es tut | Output |
|--------------------:|-----------------|-----------------|-----------------|
| 0 | `Scripts/00_run_pipeline.R` | Führt den Hauptworkflow als Sammelskript aus. Es erzeugt selbst keine neuen Daten, sondern ruft die Hauptskripte in Reihenfolge auf. | Kein eigener Output. Outputs entstehen in den jeweils aufgerufenen Skripten. |
| 1 | `Scripts/mapping_gebiete.R` | Liest die amtlichen Gemeindegebietsänderungen ein und bildet daraus zusammenhängende Aggregationsgruppen. | `Data/cleaned/mapping_gebietsaenderungen.rds`: Gemeindeschlüssel mit zugehörigem Aggregationsschlüssel aus Gebietsänderungen. |
| 2 | `Scripts/mapping_wahldaten.R` | Liest die Wahlbezirksergebnisse 2021 und 2025 ein und harmonisiert sie über Briefwahlgruppen, §-68-BWO-Fälle, manuell validierte Textfälle und Gebietsänderungen. Am Ende liegen beide Wahlen auf denselben finalen Aggregationseinheiten. | `Data/cleaned/wahldaten2021_gemappt.rds`, `Data/cleaned/wahldaten2025_gemappt.rds`: gemappte Wahldaten. `Data/cleaned/mapping_wahldaten_final_manuell_validiert.rds`, `Data/cleaned/mapping_gemeinden_final_manuell_validiert.rds`: finales Mapping. `Data/validierung/textausweisungen_*.rds`: Diagnose der Textausweisungen. |
| 3 | `Scripts/01_prepare_nslphom_input.R` | Bereitet die gemappten Wahldaten für `nslphom` vor. Verwendet Zweitstimmen, fasst CDU und CSU zu `Union` zusammen, gruppiert Parteien unter der Schwelle zu `Andere`, berechnet `Nichtwaehler` und skaliert 2025 je Einheit auf die 2021-Gesamtmasse. | `Data/cleaned/vorlaeufig_nslphom_input_2021.rds`, `Data/cleaned/vorlaeufig_nslphom_input_2025.rds`: Inputmatrizen. `Data/cleaned/vorlaeufig_nslphom_input_long.rds`, `Data/cleaned/vorlaeufig_partei_schwellenwerte.rds`: Longformat und Parteischwellen. `Data/validierung/vorlaeufig_nslphom_input_*.rds`: Inputchecks und 2025-Skalierungsfaktoren. |
| 4 | `Scripts/02_estimate_transitions.R` | Schätzt `nslphom` blockweise auf den vorbereiteten Inputs. Die Blöcke basieren auf Gebietsprefixen mit Sonderbehandlung der Stadtstaaten; die lokalen Übergangsmatrizen bleiben auf Ebene der finalen `agg_schluessel`. | `Data/modeloutput/nslphom/vorlaeufig_nslphom_fit.rds`: blockweise Fits und Settings. `Data/modeloutput/nslphom/vorlaeufig_transition_matrices_long.rds` und `_wide.rds`: lokale Übergangsmatrizen. `Data/modeloutput/nslphom/vorlaeufig_transition_checks.rds`: Rekonstruktions- und Plausibilitätschecks. |
| 5 | `Scripts/cleaning_strukturdaten.R` | Liest die große INKAR-Rohdatei nur für die benötigten Indikatoren und das Strukturjahr 2023 ein. Speichert daraus eine kleine Zwischenbasis, damit spätere Änderungen am Cleaning nicht die komplette Rohdatei neu einlesen müssen. | `Data/intermediate/vorlaeufig_inkar_basis_gemeinden_kreise.rds`: gefilterte INKAR-Basis. `Data/intermediate/vorlaeufig_inkar_basis_metadata.rds`: verwendete Indikatoren, Raumbezüge und Cache-Metadaten. |
| 6 | `Scripts/03_prepare_inkar_covariates.R` | Aggregiert die INKAR-Strukturmerkmale auf die finalen `agg_schluessel`. Aktuell werden 2023-Niveauwerte für Arbeitslosigkeit, Ausländeranteil und Kaufkraft vorbereitet. | `Data/cleaned/vorlaeufig_inkar_agg_long.rds`: aggregierte Strukturwerte im Longformat. `Data/cleaned/vorlaeufig_inkar_kovariaten_2023.rds`: eine Zeile je `agg_schluessel` mit Kovariaten-Spalten für die Regression. |
| 7 | `Scripts/04_model_transitions.R` | Modelliert AfD-Zuflüsse aus den geschätzten Übergangsmatrizen mit INKAR-Kovariaten 2023. Es werden getrennte lineare Modelle je Herkunftsgruppe und Zielgröße gespeichert. | `Data/modeloutput/regression/vorlaeufig_modell_afd_*.rds`: AfD-Zuflussdaten, Koeffizienten, Checks und Herkunftszusammenfassung. `Data/modeloutput/regression/vorlaeufig_modell_*.rds`: allgemeine Übergangsmodelle als Zusatzoutput. |
| 8 | `Scripts/07_plot_block_global_matrices.R` | Visualisiert die globalen Übergangsmatrizen der blockweisen `nslphom`-Fits. Zusätzlich wird aus den absoluten Blockübergängen eine aggregierte Deutschlandmatrix erzeugt. | `Charts/nslphom_global_matrizen_blockweise.pdf`: Sammel-PDF aller Blockgrafiken. `Charts/nslphom_global_matrix_deutschland_aggregiert.png`: aggregierte nationale Übergangsgrafik. |

## Optionale Schwerläufe

Diese Skripte gehören nicht zum normalen schnellen Hauptlauf. Sie sind für den leistungsstärkeren Rechner oder für Belastungstests gedacht.

| Reihenfolge | Skriptname | Was es tut | Output |
|--------------------:|-----------------|-----------------|-----------------|
| nach Hauptworkflow-Schritt 3 | `Scripts/06_estimate_nslphom_unblocked.R` | Schätzt `nslphom` ohne Blockaufteilung auf allen finalen Aggregationseinheiten. Der Fit ist speicherintensiv und kann über `options(waehlendenwanderung.unblocked_run_fit = FALSE)` als Dry-Run nur die Inputs und Settings speichern. | `Data/modeloutput/nslphom_unblocked/vorlaeufig_nslphom_unblocked_*.rds`: Inputkopien, Settings, Fit, lokale Matrizen, globale Matrix und Checks. |
| nach Hauptworkflow-Schritt 3 | `Scripts/test2000_nslphom_unblocked.R` | Führt einen unblocked `nslphom`-Belastungstest mit 2000 sortierten `agg_schluessel` aus. Das Skript ist nur zum Testen gedacht und speichert nicht den vollen Fit. | `Data/modeloutput/nslphom/test2000/vorlaeufig_test2000_nslphom_unblocked_endoutput.rds`: lokale und globale Übergangsmatrizen plus Settings. |
| nach Hauptworkflow-Schritt 3 und 6 | `Scripts/08_bootstrap_nslphom_regression.R` | Führt Bootstrap-Läufe aus: bundesweites Ziehen mit Zurücklegen, unblocked `nslphom` je Stichprobe und anschließende Regression. Die künstlichen Bootstrap-Schlüssel werden vor der Regression auf die originalen `agg_schluessel` zurückgemappt und geprüft. | `Data/modeloutput/bootstrap/vorlaeufig_bootstrap_*.rds`: Beta-Ziehungen, Konfidenzintervalle, nslphom-Checks, Mappingchecks, Stichprobenzusammenfassung und Settings. `Data/modeloutput/bootstrap/iterations/`: einzelne Iterationsergebnisse zur Wiederaufnahme. `Charts/vorlaeufig_bootstrap_beta_verteilungen.pdf`: Verteilungen der Bootstrap-Betas. |

## Validierung

Diese Skripte prüfen die Güte der lokalen `nslphom`-Schätzungen in Simulationen. Sie sind nicht Voraussetzung für den Hauptworkflow, sondern dienen der methodischen Absicherung.

| Reihenfolge | Skriptname | Was es tut | Output |
|--------------------:|-----------------|-----------------|-----------------|
| optional | `Scripts/05_validate_nslphom_simulation.R` | Simuliert bekannte Übergangswahrscheinlichkeiten und prüft, ob `nslphom` diese lokal und auf Basis der joined distribution wiederfindet. Enthält zusätzlich Sensitivitätschecks zu Blockdefinitionen, Parteigruppierung und größeren Aggregationseinheiten. | `Data/validierung/vorlaeufig_nslphom_validation_*.rds`: lokale Fehler, EI, Beta-Recovery, Settings und Status. `Data/validierung/vorlaeufig_nslphom_sensitivity_*.rds`: Sensitivitätsoutputs. |
| optional, guter PC | `Scripts/05b_validate_nslphom_large_unblocked.R` | Führt eine größere unblocked Validierung mit vielen Einheiten und weniger Wiederholungen aus. Das Skript ist als Belastungs- und Plausibilitätstest für den leistungsstärkeren Rechner gedacht. | `Data/validierung/large_unblocked/vorlaeufig_nslphom_large_unblocked_*.rds`: lokale Fehler, EI, Beta-Recovery, Status, Settings und wahre Simulationsparameter. |

## Funktionsdateien

Die Dateien in `Functions/` werden nicht direkt ausgeführt. Sie bündeln wiederkehrende Hilfslogik für AGS-Verarbeitung, nslphom-Input, nslphom-Schätzungen, Regression und Bootstrap, damit dieselbe Logik nicht mehrfach in verschiedenen Skripten definiert wird.
