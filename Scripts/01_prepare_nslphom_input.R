library(dplyr)
library(tidyr)
library(stringr)

source("paths.R", encoding = "UTF-8")

ensure_data_dirs()

get_agg_col <- function(data) {
  grep("^agg\\.", names(data), value = TRUE)[1]
}

first_existing <- function(data, patterns) {
  for (pattern in patterns) {
    hit <- grep(pattern, names(data), value = TRUE)
    if (length(hit) > 0) {
      return(hit[[1]])
    }
  }
  NA_character_
}

standardize_party <- function(x) {
  x <- x %>%
    str_replace("^Z_", "") %>%
    str_replace("\\.\\.\\.Zweitstimmen$", "") %>%
    str_replace_all("\\.", "_")

  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""

  party <- x %>%
    str_replace_all("[^A-Za-z0-9_]", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "")

  recode(
    party,
    AFD = "AfD",
    DIE_LINKE = "Die_Linke",
    GRUENE = "GRUNE",
    .default = party
  )
}

get_second_vote_columns <- function(data, jahr, invalid_col, valid_col) {
  if (jahr == 2021) {
    cols <- grep("^Z_", names(data), value = TRUE)
  } else {
    cols <- grep("\\.\\.\\.Zweitstimmen$", names(data), value = TRUE)
  }

  setdiff(cols, c(invalid_col, valid_col))
}

prepare_vote_year <- function(data, jahr, threshold = 0.12, keep_parties = NULL) {
  agg_col <- get_agg_col(data)
  a_col <- first_existing(data, c("^Wahlberechtigte"))
  b_col <- first_existing(data, c("^W.hlende"))
  invalid_col <- if (jahr == 2021) {
    first_existing(data, c("^Z_Ung.ltige$"))
  } else {
    first_existing(data, c("^Ung.ltige\\.\\.\\.Zweitstimmen$"))
  }
  valid_col <- if (jahr == 2021) {
    first_existing(data, c("^Z_G.ltige$"))
  } else {
    first_existing(data, c("^G.ltige\\.\\.\\.Zweitstimmen$"))
  }

  stopifnot(!is.na(agg_col), !is.na(a_col), !is.na(b_col))
  stopifnot(!is.na(invalid_col), !is.na(valid_col))

  party_cols <- get_second_vote_columns(data, jahr, invalid_col, valid_col)
  stopifnot(length(party_cols) > 0)

  party_lookup <- tibble(
    source_col = party_cols,
    party = standardize_party(party_cols)
  )

  national <- data %>%
    summarise(
      across(all_of(party_cols), ~ sum(.x, na.rm = TRUE)),
      valid_total = sum(.data[[valid_col]], na.rm = TRUE)
    ) %>%
    pivot_longer(
      cols = all_of(party_cols),
      names_to = "source_col",
      values_to = "votes"
    ) %>%
    left_join(party_lookup, by = "source_col") %>%
    mutate(
      threshold = threshold,
      national_share_valid = votes / valid_total,
      keep_party_year = national_share_valid >= threshold
    )

  if (is.null(keep_parties)) {
    keep_parties <- national %>%
      filter(keep_party_year) %>%
      pull(party)
  }

  keep_parties <- sort(unique(keep_parties))
  output_groups <- c(keep_parties, "Andere", "Nichtwaehler")
  national <- national %>%
    mutate(
      keep_party = party %in% keep_parties
    )

  long <- data %>%
    transmute(
      agg_schluessel = .data[[agg_col]],
      wahlberechtigte = .data[[a_col]],
      waehlende = .data[[b_col]],
      ungueltig_stimmen = .data[[invalid_col]],
      gueltig_stimmen = .data[[valid_col]],
      across(all_of(party_cols))
    ) %>%
    pivot_longer(
      cols = all_of(party_cols),
      names_to = "source_col",
      values_to = "votes"
    ) %>%
    left_join(party_lookup, by = "source_col") %>%
    mutate(
      gruppe = if_else(party %in% keep_parties, party, "Andere")
    ) %>%
    group_by(
      agg_schluessel,
      gruppe
    ) %>%
    summarise(
      votes = sum(votes, na.rm = TRUE),
      .groups = "drop"
    )

  residual <- data %>%
    transmute(
      agg_schluessel = .data[[agg_col]],
      Andere = .data[[invalid_col]],
      Nichtwaehler = pmax(.data[[a_col]] - .data[[b_col]], 0)
    ) %>%
    pivot_longer(
      cols = c("Andere", "Nichtwaehler"),
      names_to = "gruppe",
      values_to = "votes"
    ) %>%
    group_by(
      agg_schluessel,
      gruppe
    ) %>%
    summarise(
      votes = sum(votes, na.rm = TRUE),
      .groups = "drop"
    )

  counts <- bind_rows(long, residual) %>%
    group_by(
      agg_schluessel,
      gruppe
    ) %>%
    summarise(
      votes = sum(votes, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    complete(
      agg_schluessel,
      gruppe = output_groups,
      fill = list(votes = 0)
    )

  wide <- counts %>%
    mutate(
      gruppe = factor(gruppe, levels = output_groups)
    ) %>%
    pivot_wider(
      names_from = gruppe,
      values_from = votes,
      values_fill = 0
    ) %>%
    arrange(agg_schluessel) %>%
    select(agg_schluessel, all_of(output_groups))

  check <- data %>%
    transmute(
      agg_schluessel = .data[[agg_col]],
      wahlberechtigte = .data[[a_col]],
      waehlende = .data[[b_col]],
      gueltig_stimmen = .data[[valid_col]],
      ungueltig_stimmen = .data[[invalid_col]]
    ) %>%
    group_by(agg_schluessel) %>%
    summarise(
      wahlberechtigte = sum(wahlberechtigte, na.rm = TRUE),
      waehlende = sum(waehlende, na.rm = TRUE),
      gueltig_stimmen = sum(gueltig_stimmen, na.rm = TRUE),
      ungueltig_stimmen = sum(ungueltig_stimmen, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      counts %>%
        group_by(agg_schluessel) %>%
        summarise(input_sum = sum(votes, na.rm = TRUE), .groups = "drop"),
      by = "agg_schluessel"
    ) %>%
    mutate(
      input_reference = pmax(wahlberechtigte, waehlende),
      differenz_input_zu_wahlberechtigten = input_sum - wahlberechtigte,
      differenz_input_zu_referenz = input_sum - input_reference,
      differenz_stimmen_zu_waehlenden = gueltig_stimmen + ungueltig_stimmen - waehlende,
      flag_waehlende_groesser_wahlberechtigte = waehlende > wahlberechtigte
    )

  list(
    wide = wide,
    long = counts %>% mutate(Jahr = jahr, .before = 1),
    national = national %>% mutate(Jahr = jahr, .before = 1),
    check = check %>% mutate(Jahr = jahr, .before = 1)
  )
}

wahldaten2021 <- readRDS(file.path(data_dir_cleaned, "wahldaten2021_gemappt.rds"))
wahldaten2025 <- readRDS(file.path(data_dir_cleaned, "wahldaten2025_gemappt.rds"))

# Parteien bleiben separat, wenn sie bundesweit in mindestens einer der beiden
# Wahlen mindestens 12 Prozent der gueltigen Zweitstimmen erreichen.
initial2021 <- prepare_vote_year(wahldaten2021, 2021)
initial2025 <- prepare_vote_year(wahldaten2025, 2025)

keep_parties <- bind_rows(initial2021$national, initial2025$national) %>%
  filter(keep_party_year) %>%
  pull(party) %>%
  unique() %>%
  sort()

prepared2021 <- prepare_vote_year(wahldaten2021, 2021, keep_parties = keep_parties)
prepared2025 <- prepare_vote_year(wahldaten2025, 2025, keep_parties = keep_parties)

common_ids <- intersect(prepared2021$wide$agg_schluessel, prepared2025$wide$agg_schluessel)
stopifnot(length(common_ids) == nrow(prepared2021$wide))
stopifnot(length(common_ids) == nrow(prepared2025$wide))

prepared2021$wide <- prepared2021$wide %>% filter(agg_schluessel %in% common_ids) %>% arrange(agg_schluessel)
prepared2025$wide <- prepared2025$wide %>% filter(agg_schluessel %in% common_ids) %>% arrange(agg_schluessel)

stopifnot(identical(prepared2021$wide$agg_schluessel, prepared2025$wide$agg_schluessel))
stopifnot(all(abs(prepared2021$check$differenz_input_zu_referenz) < 1e-8))
stopifnot(all(abs(prepared2025$check$differenz_input_zu_referenz) < 1e-8))
stopifnot(all(abs(prepared2021$check$differenz_stimmen_zu_waehlenden) < 1e-8))
stopifnot(all(abs(prepared2025$check$differenz_stimmen_zu_waehlenden) < 1e-8))

saveRDS(prepared2021$wide, file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_2021.rds"))
saveRDS(prepared2025$wide, file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_2025.rds"))
saveRDS(bind_rows(prepared2021$long, prepared2025$long), file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_long.rds"))
saveRDS(bind_rows(prepared2021$national, prepared2025$national), file.path(data_dir_cleaned, "vorlaeufig_partei_schwellenwerte.rds"))
saveRDS(bind_rows(prepared2021$check, prepared2025$check), file.path(data_dir_validation, "vorlaeufig_nslphom_input_checks.rds"))

write.csv(prepared2021$wide, file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_2021.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(prepared2025$wide, file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_2025.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(bind_rows(prepared2021$long, prepared2025$long), file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_long.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(bind_rows(prepared2021$national, prepared2025$national), file.path(data_dir_cleaned, "vorlaeufig_partei_schwellenwerte.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(bind_rows(prepared2021$check, prepared2025$check), file.path(data_dir_validation, "vorlaeufig_nslphom_input_checks.csv"), row.names = FALSE, fileEncoding = "UTF-8")
