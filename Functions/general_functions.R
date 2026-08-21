# Allgemeine Hilfsfunktionen fuer Gemeindeschluessel, Aggregationen und gewichtete Mittelwerte.

# Namen der Aggregationsschluessel-Spalte eines Datensatzes ermitteln.
get_agg_col <- function(data) {
  grep("^agg\\.", names(data), value = TRUE)[1]
}

# Erste vorhandene Spalte zurueckgeben, deren Name auf eines der Suchmuster passt.
first_existing <- function(data, patterns) {
  for (pattern in patterns) {
    hit <- grep(pattern, names(data), value = TRUE)

    if (length(hit) > 0) {
      return(hit[[1]])
    }
  }

  NA_character_
}

# Gewichteten Mittelwert berechnen und fehlende oder ungueltige Gewichte sicher behandeln.
weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0

  if (!any(ok)) {
    return(NA_real_)
  }

  sum(x[ok] * w[ok]) / sum(w[ok])
}

# Achtstelligen amtlichen Gemeindeschluessel aus den amtlichen Gebietsteilen zusammensetzen.
make_ags <- function(data) {
  data %>%
    dplyr::mutate(
      "Gemeindeschl\u00fcssel" := dplyr::if_else(
        is.na(.data$Land) | is.na(.data$Regierungsbezirk) | is.na(.data$Kreis) | is.na(.data$Gemeinde),
        NA_character_,
        paste0(
          stringr::str_pad(.data$Land, width = 2, pad = "0"),
          stringr::str_pad(.data$Regierungsbezirk, width = 1, pad = "0"),
          stringr::str_pad(.data$Kreis, width = 2, pad = "0"),
          stringr::str_pad(.data$Gemeinde, width = 3, pad = "0")
        )
      )
    )
}

# Vollstaendig leere Zeilen aus einem Datensatz entfernen.
drop_empty_rows <- function(data) {
  is_empty_value <- function(x) {
    is.na(x) | stringr::str_trim(as.character(x)) == ""
  }

  data %>%
    dplyr::filter(
      !dplyr::if_all(
        dplyr::everything(),
        is_empty_value
      )
    )
}

# Gemeindeschluessel als achtstellige Zeichenketten vereinheitlichen.
normalize_ags <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- stringr::str_replace(x, "\\.0$", "")
  x[x == "" | x == "NA"] <- NA_character_
  stringr::str_pad(x, width = 8, pad = "0")
}

# Kommagetrennte Aggregationsschluessel in einzelne Gemeindeschluessel zerlegen.
split_keys <- function(x) {
  x <- x[!is.na(x) & x != ""]

  if (length(x) == 0) {
    return(character())
  }

  keys <- unlist(strsplit(x, ",\\s*"), use.names = FALSE)
  keys <- stringr::str_trim(keys)
  sort(unique(keys[!is.na(keys) & keys != ""]))
}

# Aggregationseinheiten der ostdeutschen Flaechenlaender ohne Berlin erkennen.
is_ostdeutschland_ohne_berlin <- function(agg_schluessel) {
  ost_laender <- c("12", "13", "14", "15", "16")

  vapply(
    lapply(agg_schluessel, split_keys),
    function(keys) length(keys) > 0 && all(substr(keys, 1, 2) %in% ost_laender),
    logical(1)
  )
}

# Gemeindeschluessel sortieren, doppelte entfernen und wieder komma-getrennt zusammenfassen.
collapse_keys <- function(x) {
  x <- x[!is.na(x) & x != ""]
  paste(sort(unique(x)), collapse = ", ")
}

# Alle direkt oder indirekt verbundenen Gemeindeschluessel zu Komponenten zusammenfassen.
connected_components <- function(agg_strings) {
  groups <- lapply(agg_strings, split_keys)
  groups <- groups[lengths(groups) > 0]

  if (length(groups) == 0) {
    return(tibble::tibble("agg.schl\u00fcssel" = character()))
  }

  all_keys <- sort(unique(unlist(groups, use.names = FALSE)))
  parent <- stats::setNames(all_keys, all_keys)

  find_root <- function(x) {
    root <- x

    while (!identical(parent[[root]], root)) {
      root <- parent[[root]]
    }

    while (!identical(parent[[x]], x)) {
      next_x <- parent[[x]]
      parent[[x]] <<- root
      x <- next_x
    }

    root
  }

  union_keys <- function(a, b) {
    root_a <- find_root(a)
    root_b <- find_root(b)

    if (!identical(root_a, root_b)) {
      parent[[root_b]] <<- root_a
    }
  }

  for (group in groups) {
    if (length(group) > 1) {
      first_key <- group[[1]]

      for (key in group[-1]) {
        union_keys(first_key, key)
      }
    }
  }

  roots <- vapply(all_keys, find_root, character(1))
  components <- split(all_keys, roots)

  tibble::tibble(
    "agg.schl\u00fcssel" = vapply(
      components,
      collapse_keys,
      character(1)
    )
  ) %>%
    dplyr::arrange(.data[["agg.schl\u00fcssel"]])
}
