# Funktionen fuer nslphom-, nslphom_dual- und OSQP-Schaetzungen sowie deren Aufbereitung.

# Sicherstellen, dass das fuer die Schaetzung benoetigte Paket lphom installiert ist.
check_lphom_available <- function() {
  if (!requireNamespace("lphom", quietly = TRUE)) {
    stop(
      "Das Paket 'lphom' ist nicht installiert. ",
      "Installiere es mit install.packages('lphom'), damit lphom::nslphom() laufen kann."
    )
  }

  invisible(TRUE)
}

# Sicherstellen, dass OSQP und die Sparse-Matrix-Unterstuetzung installiert sind.
check_osqp_available <- function() {
  if (!requireNamespace("osqp", quietly = TRUE)) {
    stop(
      "Das Paket 'osqp' ist nicht installiert. ",
      "Installiere es mit install.packages('osqp'), damit der OSQP-Solver laufen kann."
    )
  }

  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Das Paket 'Matrix' wird fuer sparse OSQP-Matrizen benoetigt.")
  }

  invisible(TRUE)
}

# Eine nicht exportierte Hilfsfunktion aus dem lphom-Namespace laden.
get_lphom_internal <- function(name) {
  get(name, envir = asNamespace("lphom"), inherits = FALSE)
}

# Zentral vorbereitete nslphom-Inputs laden und in identischer Schluesselreihenfolge ausgeben.
read_prepared_nslphom_inputs <- function() {
  files <- c(
    input2021 = file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_2021.rds"),
    input2025 = file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_2025.rds"),
    input_long = file.path(data_dir_cleaned, "vorlaeufig_nslphom_input_long.rds"),
    party_thresholds = file.path(data_dir_cleaned, "vorlaeufig_partei_schwellenwerte.rds"),
    input_checks = file.path(data_dir_validation, "vorlaeufig_nslphom_input_checks.rds")
  )

  missing_files <- files[!file.exists(files)]

  if (length(missing_files) > 0) {
    stop(
      "Zentrale nslphom-Inputdateien fehlen. Fuehre zuerst ",
      "Scripts/01_prepare_nslphom_input.R aus. Fehlend: ",
      paste(missing_files, collapse = ", ")
    )
  }

  list(
    input2021 = readRDS(files[["input2021"]]) %>% dplyr::arrange(.data$agg_schluessel),
    input2025 = readRDS(files[["input2025"]]) %>% dplyr::arrange(.data$agg_schluessel),
    input_long = readRDS(files[["input_long"]]),
    party_thresholds = readRDS(files[["party_thresholds"]]),
    input_checks = readRDS(files[["input_checks"]])
  )
}

# Fertige Wahlinputs auf gleiche Einheiten, Gruppen, Massen und Schwellenwerte pruefen.
validate_prepared_nslphom_inputs <- function(inputs, threshold = 0.12) {
  stopifnot(identical(inputs$input2021$agg_schluessel, inputs$input2025$agg_schluessel))
  stopifnot(identical(names(inputs$input2021), names(inputs$input2025)))

  group_names <- setdiff(names(inputs$input2021), "agg_schluessel")
  stopifnot(!any(c("CDU", "CSU") %in% group_names))
  stopifnot("Union" %in% group_names)
  stopifnot(all(rowSums(inputs$input2021[group_names]) == rowSums(inputs$input2025[group_names])))
  stopifnot(all(abs(inputs$input_checks$differenz_input_zu_referenz) < 1e-8))
  stopifnot(all(abs(inputs$input_checks$differenz_stimmen_zu_waehlenden) < 1e-8))
  stopifnot(all(make_count_matrix(inputs$input2021) >= 0, na.rm = TRUE))
  stopifnot(all(make_count_matrix(inputs$input2025) >= 0, na.rm = TRUE))

  threshold_check <- inputs$party_thresholds %>%
    dplyr::mutate(expected_keep_party_year = .data$national_share_valid >= threshold) %>%
    dplyr::filter(.data$keep_party_year != .data$expected_keep_party_year)

  if (nrow(threshold_check) > 0) {
    stop(
      "Die zentralen nslphom-Inputs passen nicht zur ",
      threshold * 100,
      "%-Schwelle. Fuehre zuerst Scripts/01_prepare_nslphom_input.R neu aus."
    )
  }

  kept_parties <- inputs$party_thresholds %>%
    dplyr::filter(.data$keep_party) %>%
    dplyr::pull(party) %>%
    unique() %>%
    sort()

  expected_kept_parties <- inputs$party_thresholds %>%
    dplyr::group_by(.data$party) %>%
    dplyr::summarise(
      expected_keep_party = any(.data$national_share_valid >= threshold),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$expected_keep_party) %>%
    dplyr::pull(party) %>%
    sort()

  stopifnot(identical(kept_parties, expected_kept_parties))
  stopifnot(setequal(c(kept_parties, "Andere", "Nichtwaehler"), group_names))

  list(
    group_names = group_names,
    kept_parties = kept_parties
  )
}

# Inputtabelle ohne Schluessel in eine numerische Zaehldatenmatrix umwandeln.
make_count_matrix <- function(data, id_col = "agg_schluessel") {
  mat <- data %>%
    dplyr::select(-dplyr::all_of(id_col)) %>%
    as.matrix()

  storage.mode(mat) <- "numeric"
  rownames(mat) <- data[[id_col]]

  mat
}

# Ein lineares Programm als OSQP-Spezialfall mit P = 0 und Sparse-Matrix loesen.
solve_lp_osqp <- function(
    objective,
    constraint_matrix,
    rhs,
    rhs_tolerance = 0,
    scale_constraints = FALSE,
    context = "LP") {
  check_osqp_available()

  objective <- as.numeric(objective)
  rhs <- as.numeric(rhs)
  rhs_tolerance <- rep(as.numeric(rhs_tolerance), length.out = length(rhs))
  n_vars <- length(objective)

  if (ncol(constraint_matrix) != n_vars) {
    stop("OSQP-", context, ": Die Zahl der Spalten passt nicht zur Zielfunktion.")
  }

  equality_matrix <- Matrix::Matrix(constraint_matrix, sparse = TRUE)

  if (isTRUE(scale_constraints)) {
    row_scale <- pmax(
      as.numeric(Matrix::rowSums(abs(equality_matrix))),
      abs(rhs),
      1
    )
    equality_matrix <- Matrix::Diagonal(x = 1 / row_scale) %*% equality_matrix
    rhs <- rhs / row_scale
    rhs_tolerance <- rhs_tolerance / row_scale
  }

  # OSQP formuliert Nebenbedingungen als l <= A %*% x <= u.
  # Gleichungen erhalten deshalb l = rhs und u = rhs; zusaetzlich werden
  # die Nichtnegativitaetsbedingungen aus lpSolve/Rsymphony als x >= 0 ergaenzt.
  osqp_matrix <- rbind(equality_matrix, Matrix::Diagonal(n_vars))
  lower_bounds <- c(rhs - rhs_tolerance, rep(0, n_vars))
  upper_bounds <- c(rhs + rhs_tolerance, rep(Inf, n_vars))
  quadratic_regularization <- getOption("waehlendenwanderung.osqp_quadratic_regularization", 0)
  quadratic_matrix <- if (quadratic_regularization > 0) {
    Matrix::Diagonal(n_vars, x = quadratic_regularization)
  } else {
    Matrix::sparseMatrix(
      i = integer(),
      j = integer(),
      x = numeric(),
      dims = c(n_vars, n_vars)
    )
  }

  settings <- osqp::osqpSettings(
    verbose = isTRUE(getOption("waehlendenwanderung.osqp_verbose", FALSE)),
    max_iter = as.integer(getOption("waehlendenwanderung.osqp_max_iter", 100000L)),
    eps_abs = getOption("waehlendenwanderung.osqp_eps_abs", 1e-3),
    eps_rel = getOption("waehlendenwanderung.osqp_eps_rel", 1e-3),
    polishing = isTRUE(getOption("waehlendenwanderung.osqp_polishing", TRUE))
  )

  solution <- osqp::solve_osqp(
    P = quadratic_matrix,
    q = objective,
    A = osqp_matrix,
    l = lower_bounds,
    u = upper_bounds,
    pars = settings
  )

  status <- solution$info$status
  if (!status %in% c("solved", "solved inaccurate")) {
    stop(
      "OSQP-",
      context,
      " wurde nicht geloest. Status: ",
      status,
      "; prim_res = ",
      solution$info$prim_res,
      "; dual_res = ",
      solution$info$dual_res,
      "; iter = ",
      solution$info$iter
    )
  }

  x <- as.numeric(solution$x)
  if (anyNA(x)) {
    stop("OSQP-", context, " enthaelt fehlende Loesungswerte.")
  }

  x[abs(x) < getOption("waehlendenwanderung.osqp_zero_tolerance", 1e-10)] <- 0

  list(
    solution = x,
    objval = sum(objective * x),
    status = status,
    info = solution$info
  )
}

# Unter allen L1-optimalen lokalen Loesungen die global naechste per QP auswaehlen.
solve_local_tiebreak_qp_osqp <- function(
    base_matrix,
    base_rhs,
    first_objective,
    first_objective_value,
    global_probability,
    context = "lokaler Tie-Break") {
  check_osqp_available()

  base_matrix <- Matrix::Matrix(base_matrix, sparse = TRUE)
  base_rhs <- as.numeric(base_rhs)
  first_objective <- as.numeric(first_objective)
  global_probability <- as.numeric(global_probability)
  n_vars <- length(first_objective)
  n_probability_vars <- length(global_probability)

  if (n_vars < n_probability_vars) {
    stop("OSQP-", context, ": unplausible Variablenzahl.")
  }

  objective_tolerance <- max(
    getOption("waehlendenwanderung.osqp_local_objective_tolerance", 1e-5),
    abs(first_objective_value) * getOption("waehlendenwanderung.osqp_local_objective_relative_tolerance", 1e-6)
  )

  quadratic_matrix <- Matrix::sparseMatrix(
    i = seq_len(n_probability_vars),
    j = seq_len(n_probability_vars),
    x = rep(1, n_probability_vars),
    dims = c(n_vars, n_vars)
  )
  linear_objective <- c(-global_probability, rep(0, n_vars - n_probability_vars))
  objective_row <- Matrix::Matrix(first_objective, nrow = 1L, sparse = TRUE)
  osqp_matrix <- rbind(base_matrix, objective_row, Matrix::Diagonal(n_vars))

  lower_bounds <- c(base_rhs, -Inf, rep(0, n_vars))
  upper_bounds <- c(base_rhs, first_objective_value + objective_tolerance, rep(Inf, n_vars))

  settings <- osqp::osqpSettings(
    verbose = isTRUE(getOption("waehlendenwanderung.osqp_verbose", FALSE)),
    max_iter = as.integer(getOption("waehlendenwanderung.osqp_max_iter", 100000L)),
    eps_abs = getOption("waehlendenwanderung.osqp_eps_abs", 1e-3),
    eps_rel = getOption("waehlendenwanderung.osqp_eps_rel", 1e-3),
    polishing = isTRUE(getOption("waehlendenwanderung.osqp_polishing", TRUE))
  )

  solution <- osqp::solve_osqp(
    P = quadratic_matrix,
    q = linear_objective,
    A = osqp_matrix,
    l = lower_bounds,
    u = upper_bounds,
    pars = settings
  )

  status <- solution$info$status
  if (!status %in% c("solved", "solved inaccurate")) {
    stop(
      "OSQP-",
      context,
      " wurde nicht geloest. Status: ",
      status,
      "; prim_res = ",
      solution$info$prim_res,
      "; dual_res = ",
      solution$info$dual_res,
      "; iter = ",
      solution$info$iter
    )
  }

  x <- as.numeric(solution$x)
  x[abs(x) < getOption("waehlendenwanderung.osqp_zero_tolerance", 1e-10)] <- 0

  list(
    solution = x,
    objval = solution$info$obj_val,
    status = status,
    info = solution$info
  )
}

# Globales lphom-Optimierungssystem mit Wahrscheinlichkeiten und Slackvariablen sparse aufbauen.
model_lphom_apriori_1_2_sparse <- function(X, Y, P0, lambda, uniform) {
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  J <- ncol(X)
  K <- ncol(Y)
  I <- nrow(X)
  JK <- J * K
  IK <- I * K
  xt <- colSums(X)
  yt <- colSums(Y)

  if (lambda == 1) {
    stop("Der OSQP-Solver ist im Workflow fuer lambda < 1 implementiert.")
  }

  # Variablenindizes fuer globale Wahrscheinlichkeiten sowie positive und negative Abweichungen.
  p_index <- matrix(seq_len(JK), nrow = J, ncol = K, byrow = TRUE)
  slack_global_minus <- JK + p_index
  slack_global_plus <- 2L * JK + p_index
  slack_local_plus <- 3L * JK + matrix(seq_len(IK), nrow = I, ncol = K, byrow = TRUE)
  slack_local_minus <- 3L * JK + IK + matrix(seq_len(IK), nrow = I, ncol = K, byrow = TRUE)
  n_vars <- 3L * JK + 2L * IK

  n_uniform_rows <- if (uniform) J - 1L else 0L
  row_a2 <- seq_len(J)
  row_a3 <- J + seq_len(K)
  row_a4 <- J + K + seq_len(JK)
  row_a5_start <- J + K + JK
  row_a9_start <- row_a5_start + n_uniform_rows
  n_rows <- row_a9_start + IK

  i_idx <- integer()
  j_idx <- integer()
  x_val <- numeric()

  add_entries <- function(rows, cols, values) {
    i_idx <<- c(i_idx, as.integer(rows))
    j_idx <<- c(j_idx, as.integer(cols))
    x_val <<- c(x_val, as.numeric(values))
  }

  # Jede Herkunftszeile der globalen Matrix muss sich zu 1 summieren.
  for (j in seq_len(J)) {
    add_entries(rep(row_a2[[j]], K), p_index[j, ], rep(1, K))
  }

  # Die globale Matrix muss die bundesweiten Zielstimmen jeder Gruppe reproduzieren.
  for (k in seq_len(K)) {
    add_entries(rep(row_a3[[k]], J), p_index[, k], xt)
  }

  # Abweichungen von vorgegebenen A-priori-Wahrscheinlichkeiten ueber Slackvariablen abbilden.
  p0_vector <- as.vector(t(P0))
  non_missing_p0 <- which(!is.na(p0_vector))
  if (length(non_missing_p0) > 0) {
    add_entries(row_a4[non_missing_p0], non_missing_p0, rep(1, length(non_missing_p0)))
    add_entries(row_a4[non_missing_p0], JK + non_missing_p0, rep(-1, length(non_missing_p0)))
    add_entries(row_a4[non_missing_p0], 2L * JK + non_missing_p0, rep(1, length(non_missing_p0)))
  }

  if (uniform) {
    for (j in seq_len(J - 1L)) {
      current_row <- row_a5_start + j
      add_entries(
        rep(current_row, 2),
        c(p_index[1L, K], p_index[j + 1L, K]),
        c(1, -1)
      )
    }
  }

  # Lokale Zielabweichungen je Einheit und Zielgruppe ebenfalls linear erfassen.
  for (i in seq_len(I)) {
    for (k in seq_len(K)) {
      current_row <- row_a9_start + (i - 1L) * K + k
      non_zero_origin <- which(X[i, ] != 0)

      if (length(non_zero_origin) > 0) {
        add_entries(
          rep(current_row, length(non_zero_origin)),
          p_index[non_zero_origin, k],
          X[i, non_zero_origin]
        )
      }

      add_entries(
        rep(current_row, 2),
        c(slack_local_plus[i, k], slack_local_minus[i, k]),
        c(1, -1)
      )
    }
  }

  # Nur tatsaechlich belegte Koeffizienten in der Sparse-Matrix speichern.
  A <- Matrix::sparseMatrix(
    i = i_idx,
    j = j_idx,
    x = x_val,
    dims = c(n_rows, n_vars)
  )

  b <- c(
    rep(1, J),
    yt,
    ifelse(is.na(p0_vector), 0, p0_vector),
    if (uniform) rep(0, J - 1L) else numeric(),
    as.vector(t(Y))
  )

  fp <- rep(0, JK)
  fs <- lambda * rep(xt, each = K)
  fs[is.na(p0_vector)] <- 0
  fe <- rep(1 - lambda, 2L * IK)

  list(
    A = A,
    b = b,
    f = c(fp, fs, fs, fe)
  )
}

# Globale lphom-Startmatrix mit OSQP schaetzen und in ein lphom-aehnliches Objekt ueberfuehren.
nslphom_lphom_osqp <- function(
    votes_election1,
    votes_election2,
    new_and_exit_voters = "simultaneous",
    apriori = NULL,
    lambda = 0.5,
    uniform = TRUE,
    structural_zeros = NULL,
    integers = FALSE,
    verbose = TRUE,
    ...) {
  check_lphom_available()
  check_osqp_available()

  if (!isFALSE(integers)) {
    stop("Der OSQP-Solver ist hier nur fuer kontinuierliche Uebergangswerte implementiert.")
  }

  if (!is.null(structural_zeros)) {
    stop("Structural zeros sind im OSQP-Solver noch nicht implementiert.")
  }

  # Originale lphom-Pruefungen und dessen Aufbereitung der Wahlmatrizen weiterverwenden.
  matrix_votes <- get_lphom_internal("tests_inputs_lphom")(
    c(
      as.list(environment()),
      list(solver = "lp_solve", integers.solver = "symphony"),
      list(...)
    )
  )

  inputs <- list(
    votes_election1 = votes_election1,
    votes_election2 = votes_election2,
    new_and_exit_voters = new_and_exit_voters[1],
    apriori = apriori,
    lambda = lambda,
    uniform = uniform,
    structural_zeros = structural_zeros,
    integers = FALSE,
    verbose = verbose,
    solver = "osqp",
    integers.solver = NA_character_
  )

  x0 <- matrix_votes$x
  y0 <- matrix_votes$y

  if (any(abs(rowSums(x0) - rowSums(y0)) > 1e-8)) {
    stop("Der OSQP-Hauptsolver erwartet bereits skalierte gleiche Zeilensummen.")
  }

  scenario <- new_and_exit_voters[1]
  if (!scenario %in% c("simultaneous", "raw")) {
    stop("Der OSQP-Hauptsolver ist fuer den aktuellen Workflow mit gleichen Zeilensummen implementiert.")
  }

  # Bei gleichen Zeilensummen entstehen im simultaneous-Szenario keine Entry/Exit-Gruppen.
  net <- get_lphom_internal("compute_net_voters")(x0 = x0, y0 = y0)
  x <- net$x
  y <- net$y
  apriori <- get_lphom_internal("completar_apriori")(net = net, apriori = apriori)

  J <- ncol(x)
  K <- ncol(y)
  JK <- J * K
  names1 <- colnames(x)
  names2 <- colnames(y)

  if (verbose) {
    message("Schaetze globale nslphom-Startmatrix mit OSQP.")
  }

  # Inhaltlich dasselbe globale LP wie lphom aufbauen, aber als Sparse-Matrix.
  sistema <- model_lphom_apriori_1_2_sparse(
    X = x,
    Y = y,
    P0 = apriori,
    lambda = lambda,
    uniform = FALSE
  )

  # Das lineare Problem als quadratisches OSQP-Problem mit P = 0 loesen.
  sol <- solve_lp_osqp(
    objective = sistema$f,
    constraint_matrix = sistema$A,
    rhs = sistema$b,
    context = "globale Startmatrix"
  )

  z <- sol$solution
  pjk <- matrix(z[seq_len(JK)], J, K, TRUE, dimnames = list(names1, names2))
  # EHet misst je Einheit die Zielabweichung unter der globalen Startmatrix.
  eik <- y - x %*% pjk
  colnames(eik) <- names2
  rownames(eik) <- rownames(x)
  EHet <- eik

  vjk <- pjk * colSums(x)
  vjk.complete <- vjk
  pkj <- t(vjk) / colSums(vjk)
  filas0 <- which(rowSums(vjk) == 0L)
  colum0 <- which(colSums(vjk) == 0L)
  pjk[filas0, ] <- 0L
  pkj[colum0, ] <- 0L
  pjk.complete <- pjk
  HIe <- 100 * sum(abs(eik)) / sum(vjk.complete)
  pjk <- round(100 * pjk, 2)
  pkj <- round(100 * pkj, 2)

  det_bounds <- get_lphom_internal("bounds_compound")(
    origin = x,
    destination = y,
    zeros = structural_zeros
  )[c(1, 2)]

  output <- list(
    VTM = pjk,
    VTM.votes = vjk,
    OTM = pkj,
    HETe = HIe,
    VTM.complete = pjk.complete,
    VTM.complete.votes = vjk.complete,
    deterministic.bounds = det_bounds,
    inputs = inputs,
    origin = x,
    destination = y,
    EHet = EHet
  )
  class(output) <- c("lphom", "ei_lp")
  output
}

# Lokale Matrix einer Einheit mit minimaler Abweichung zur globalen Matrix per OSQP bestimmen.
lphom_local_abs_osqp <- function(lphom.object, iii) {
  xt <- lphom.object$origin[iii, ]
  yt <- lphom.object$destination[iii, ]
  filas0 <- which(rowSums(lphom.object$VTM.complete) == 0)
  pg <- lphom.object$VTM.complete / rowSums(lphom.object$VTM.complete)
  pg[filas0, ] <- 0
  ceros <- get_lphom_internal("determinar_zeros_estructurales")(lphom.object)
  nj <- length(xt)
  nk <- length(yt)
  njk <- nj * nk

  # Nebenbedingungen fuer Zeilensummen, lokale Zielraender und Abstand zur globalen Matrix bauen.
  a1 <- kronecker(diag(nj), t(rep(1L, nk)))
  b1 <- rep(1L, nj)
  at <- t(kronecker(xt, diag(nk)))
  bt <- yt
  ajk <- cbind(
    kronecker(diag(xt), diag(nk)),
    t(kronecker(diag(njk), c(1L, -1L)))
  )
  bjk <- as.vector(t(xt * pg))
  a <- rbind(cbind(rbind(a1, at), matrix(0L, nj + nk, 2L * njk)), ajk)
  b <- c(b1, bt, bjk)

  if (length(ceros) > 0) {
    ast <- matrix(0L, length(ceros), ncol(a))
    bst <- rep(0L, length(ceros))

    for (i in seq_along(ceros)) {
      ast[i, nk * (ceros[[i]][1L] - 1L) + ceros[[i]][2L]] <- 1L
    }

    a <- rbind(a, ast)
    b <- c(b, bst)
  }

  fun.obj <- c(rep(0L, njk), rep(1L, 2L * njk))
  # Schritt 1 minimiert den absoluten Abstand zur aktuellen globalen Matrix.
  sol <- solve_lp_osqp(
    fun.obj,
    a,
    b,
    context = paste0("lokale Einheit ", iii, " Schritt 1")
  )
  # OSQP ist fuer quadratische Programme gebaut. Der zweite lokale Schritt
  # ersetzt deshalb den linearen Tie-Break aus lphom durch einen QP-Tie-Break:
  # Unter dem minimalen L1-Abstand aus Schritt 1 wird die lokale Matrix moeglichst
  # nah an der aktuellen globalen Matrix gehalten.
  nsol <- solve_local_tiebreak_qp_osqp(
    base_matrix = a,
    base_rhs = b,
    first_objective = fun.obj,
    first_objective_value = sol$objval,
    global_probability = as.vector(t(pg)),
    context = paste0("lokale Einheit ", iii, " Schritt 2")
  )

  matrix(
    nsol$solution[seq_len(njk)],
    nj,
    nk,
    TRUE,
    dimnames = dimnames(lphom.object$VTM.complete)
  )
}

# nslphom iterativ mit einer OSQP-Startmatrix und waehlbarem lokalen Solver schaetzen.
nslphom_osqp <- function(
    votes_election1,
    votes_election2,
    new_and_exit_voters = "simultaneous",
    apriori = NULL,
    lambda = 0.5,
    uniform = TRUE,
    iter.max = 10,
    min.first = FALSE,
    structural_zeros = NULL,
    integers = FALSE,
    distance.local = "abs",
    verbose = TRUE,
    burnin = 0,
    tol = 10^-5,
    ...) {
  if (iter.max < 0 | iter.max %% 1 > 0) {
    stop("iter.max must be a positive integer")
  }

  if (distance.local[1] != "abs" || !isTRUE(uniform)) {
    stop("Der OSQP-Solver ist fuer distance.local = 'abs' und uniform = TRUE implementiert.")
  }

  if (!isFALSE(integers)) {
    stop("Der OSQP-Solver ist hier nur fuer kontinuierliche Uebergangswerte implementiert.")
  }

  if (iter.max <= burnin) {
    stop("The number of iterations (iter.max) must be higher than burnin")
  }

  # Zuerst eine globale homogene Startmatrix ueber alle Einheiten schaetzen.
  lphom_inic <- nslphom_lphom_osqp(
    votes_election1 = votes_election1,
    votes_election2 = votes_election2,
    new_and_exit_voters = new_and_exit_voters,
    apriori = apriori,
    lambda = lambda,
    uniform = uniform,
    structural_zeros = structural_zeros,
    integers = integers,
    verbose = verbose,
    ...
  )
  lphom0 <- lphom_inic
  zeros <- get_lphom_internal("determinar_zeros_estructurales")(lphom_inic)
  local_solver <- getOption("waehlendenwanderung.osqp_local_solver", "lp_solve")
  local_solver <- match.arg(local_solver, c("lp_solve", "symphony", "osqp"))

  # Globale, lokale und EHet-Ergebnisse jeder Iteration fuer die spaetere Auswahl speichern.
  VTM.sequence <- array(NA, c(dim(lphom_inic$VTM.complete), iter.max + 1L))
  VTM_votos.sequence <- array(NA, c(dim(lphom_inic$VTM.complete), iter.max + 1L))
  VTM_units.sequence <- array(NA, c(dim(lphom_inic$VTM.complete), nrow(lphom_inic$origin), iter.max + 1L))
  votos_units.sequence <- array(NA, c(dim(lphom_inic$VTM.complete), nrow(lphom_inic$origin), iter.max + 1L))
  EHet.sequence <- array(NA, c(dim(lphom_inic$EHet), iter.max + 1L))

  iter <- 0L
  dif.max <- Inf
  VTM.iter <- lphom0$VTM.complete <- lphom_inic$VTM.complete
  VTM.sequence[, , iter + 1L] <- VTM.iter
  HETe.sequence <- lphom_inic$HETe
  EHet.sequence[, , iter + 1L] <- lphom_inic$EHet

  while (iter < iter.max & dif.max > tol) {
    if (verbose) {
      message("OSQP-nslphom Iteration ", iter + 1L, " von ", iter.max, ".")
    }

    VTM_units <- votos_units <- array(NA, c(dim(lphom_inic$VTM.complete), nrow(lphom_inic$origin)))

    # Fuer jede Aggregationseinheit eine Matrix passend zu ihren beiden Wahlraendern schaetzen.
    for (i in seq_len(nrow(lphom_inic$origin))) {
      VTM_units[, , i] <- if (local_solver == "osqp") {
        lphom_local_abs_osqp(lphom.object = lphom0, iii = i)
      } else {
        get_lphom_internal("lphom_local_abs")(
          lphom.object = lphom0,
          iii = i,
          solver = local_solver
        )
      }
      votos_units[, , i] <- VTM_units[, , i] / rowSums(VTM_units[, , i]) * lphom_inic$origin[i, ]
      VTM_units[lphom_inic$origin[i, ] == 0L, , i] <- 0L
    }

    votos_units[is.na(votos_units)] <- 0L
    # Lokale absolute Matrizen addieren und daraus die naechste globale Matrix ableiten.
    VTM_votos_homogeneos <- get_lphom_internal("HET_MT.votos_MT.prop_Y")(votos_units)
    VTM_votos <- VTM_votos_homogeneos$MT.votos
    VTM.complete <- VTM_votos_homogeneos$MT.pro
    iter <- iter + 1L
    dif.max <- max(abs(VTM.complete - VTM.iter))
    VTM.iter <- lphom0$VTM.complete <- VTM.complete
    VTM.sequence[, , iter + 1L] <- VTM.iter
    HETe.sequence <- c(HETe.sequence, VTM_votos_homogeneos$HET)
    VTM_votos.sequence[, , iter + 1L] <- VTM_votos
    VTM_units.sequence[, , , iter + 1L] <- VTM_units
    votos_units.sequence[, , , iter + 1L] <- votos_units
    EHet.sequence[, , iter + 1L] <- VTM_votos_homogeneos$EHet

    if (min.first & (HETe.sequence[iter + 1L] > HETe.sequence[iter])) {
      dif.max <- -Inf
    }
  }

  dimnames(VTM.sequence) <- c(dimnames(lphom_inic$VTM.complete), list(paste0("iter = ", 0L:iter.max)))
  VTM.sequence <- VTM.sequence[, , 1L:(iter + 1L)]
  VTM_votos.sequence <- VTM_votos.sequence[, , 1L:(iter + 1L)]
  VTM_units.sequence <- VTM_units.sequence[, , , 1L:(iter + 1L)]
  votos_units.sequence <- votos_units.sequence[, , , 1L:(iter + 1L)]

  if (iter < burnin) {
    burnin <- iter - 1L
  }

  # Nicht zwingend die letzte, sondern die Iteration mit minimalem HETe auswaehlen.
  iter.select <- which.min(HETe.sequence[(burnin + 2L):(iter + 1L)])
  VTM.complete <- VTM.sequence[, , burnin + 1L + iter.select]
  VTM_votos <- VTM_votos.sequence[, , burnin + 1L + iter.select]
  VTM_units <- VTM_units.sequence[, , , burnin + 1L + iter.select]
  votos_units <- votos_units.sequence[, , , burnin + 1L + iter.select]
  OTM <- round(t(VTM_votos) / colSums(VTM_votos) * 100, 2)
  OTM <- OTM[seq_len(nrow(lphom_inic$OTM)), seq_len(ncol(lphom_inic$OTM))]
  EHet <- EHet.sequence[, , burnin + 1L + iter.select]
  dimnames(OTM) <- dimnames(lphom_inic$OTM)
  HETe <- HETe.sequence[burnin + 1L + iter.select]
  dimnames(VTM_units) <- dimnames(votos_units) <- c(
    dimnames(VTM.complete),
    list(rownames(lphom_inic$origin))
  )
  dimnames(EHet) <- dimnames(lphom_inic$EHet)
  dimnames(VTM_votos) <- dimnames(lphom_inic$VTM.complete)
  VTM <- round(VTM.complete[seq_len(nrow(lphom_inic$VTM)), seq_len(ncol(lphom_inic$VTM))] * 100, 2)
  VTM.votes <- VTM_votos[seq_len(nrow(lphom_inic$VTM)), seq_len(ncol(lphom_inic$VTM))]
  lphom_inic$inputs$verbose <- verbose
  inputs <- c(
    lphom_inic$inputs,
    iter.max = iter.max,
    min.first = min.first,
    uniform = uniform,
    distance.local = distance.local,
    burnin = burnin,
    tol = tol
  )
  inputs$osqp_local_solver <- local_solver
  inic <- lphom_inic[c(1L:6L, 10L)]
  names(inic) <- paste0(names(inic), "_init")
  filas0 <- which(rowSums(VTM_votos) == 0)
  colum0 <- which(colSums(VTM_votos) == 0)
  VTM[filas0, ] <- 0
  VTM.complete[filas0, ] <- 0
  OTM[colum0, ] <- 0
  det.bounds <- get_lphom_internal("bounds_compound")(
    origin = lphom_inic$origin,
    destination = lphom_inic$destination,
    zeros = zeros
  )

  output <- list(
    VTM = VTM,
    VTM.votes = VTM.votes,
    OTM = OTM,
    HETe = HETe,
    VTM.complete = VTM.complete,
    VTM.complete.votes = VTM_votos,
    VTM.sequence = VTM.sequence,
    HETe.sequence = HETe.sequence,
    VTM.prop.units = VTM_units,
    VTM.votes.units = votos_units,
    zeros = zeros,
    iter = iter,
    iter.min = burnin + iter.select,
    EHet = EHet,
    deterministic.bounds = det.bounds,
    inputs = inputs,
    origin = lphom_inic$origin,
    destination = lphom_inic$destination,
    solution_init = inic,
    argg = c(as.list(environment()), list(...))
  )
  class(output) <- c("nslphom", "ei_lp", "lphom")
  output
}

# Einfaches nslphom mit OSQP, Symphony oder lp_solve ausfuehren.
fit_nslphom_model <- function(
    origin_counts,
    destination_counts,
    iter_max = getOption("waehlendenwanderung.nslphom_iter_max", 10L),
    tol = getOption("waehlendenwanderung.nslphom_tol", 1e-5),
    solver = getOption("waehlendenwanderung.nslphom_solver", "osqp"),
    verbose = FALSE,
    method = "lphom::nslphom") {
  check_lphom_available()
  solver <- match.arg(solver, c("lp_solve", "symphony", "osqp"))

  if (solver == "osqp") {
    fit <- nslphom_osqp(
      votes_election1 = as.data.frame(origin_counts),
      votes_election2 = as.data.frame(destination_counts),
      new_and_exit_voters = "simultaneous",
      apriori = NULL,
      uniform = TRUE,
      iter.max = iter_max,
      min.first = FALSE,
      structural_zeros = NULL,
      integers = FALSE,
      distance.local = "abs",
      verbose = verbose,
      burnin = 0,
      tol = tol
    )
  } else {
    fit <- lphom::nslphom(
      votes_election1 = as.data.frame(origin_counts),
      votes_election2 = as.data.frame(destination_counts),
      new_and_exit_voters = "simultaneous",
      apriori = NULL,
      uniform = TRUE,
      iter.max = iter_max,
      min.first = FALSE,
      structural_zeros = NULL,
      integers = FALSE,
      distance.local = "abs",
      verbose = verbose,
      solver = solver,
      burnin = 0,
      tol = tol
    )
  }

  attr(fit, "method_label") <- method
  attr(fit, "solver") <- solver
  fit
}

# Lokale absolute Uebergangszahlen zeilenweise in Wahrscheinlichkeiten umrechnen.
local_votes_to_probabilities <- function(votes_units) {
  probabilities <- votes_units

  for (i in seq_len(dim(votes_units)[[3]])) {
    origin_counts <- rowSums(votes_units[, , i], na.rm = TRUE)
    probabilities[, , i] <- sweep(votes_units[, , i], 1L, origin_counts, "/")
    probabilities[origin_counts == 0, , i] <- 0
  }

  probabilities[!is.finite(probabilities)] <- 0
  probabilities
}

# Eine absolute Uebergangsmatrix so standardisieren, dass jede Herkunftszeile 1 ergibt.
row_standardize_matrix <- function(votes_matrix) {
  row_totals <- rowSums(votes_matrix, na.rm = TRUE)
  probabilities <- sweep(votes_matrix, 1L, row_totals, "/")
  probabilities[row_totals == 0, ] <- 0
  probabilities[!is.finite(probabilities)] <- 0
  probabilities
}

# Vorwaerts- und transponierte Rueckwaertsmatrizen mitteln und invers nach HETe gewichten.
combine_dual_votes <- function(votes_12, votes_21, HETe_12, HETe_21) {
  votes_21 <- aperm(votes_21, c(2L, 1L, 3L))
  votes_average <- (votes_12 + votes_21) / 2

  if (HETe_12 == 0 && HETe_21 == 0) {
    votes_weighted <- votes_average
  } else if (HETe_12 == 0) {
    votes_weighted <- votes_12
  } else if (HETe_21 == 0) {
    votes_weighted <- votes_21
  } else {
    votes_weighted <- (
      votes_12 / HETe_12 + votes_21 / HETe_21
    ) / (1 / HETe_12 + 1 / HETe_21)
  }

  list(average = votes_average, weighted = votes_weighted)
}

# Zwei OSQP-nslphom-Richtungen schaetzen und wie nslphom_dual kombinieren.
nslphom_dual_osqp <- function(
    votes_election1,
    votes_election2,
    iter.max = 10L,
    min.first = FALSE,
    integers = FALSE,
    tol = 1e-5,
    verbose = TRUE) {
  # Vorwaertsrichtung: Verteilung 2021 als Herkunft und 2025 als Ziel.
  fit_12 <- nslphom_osqp(
    votes_election1 = votes_election1,
    votes_election2 = votes_election2,
    new_and_exit_voters = "simultaneous",
    iter.max = iter.max,
    min.first = min.first,
    integers = integers,
    tol = tol,
    verbose = verbose
  )
  fit_12$VTM.sequence <- NULL
  fit_12$HETe.sequence <- NULL
  fit_12$argg <- NULL
  gc()

  # Rueckwaertsrichtung: Rollen der beiden Wahlen vertauschen.
  fit_21 <- nslphom_osqp(
    votes_election1 = votes_election2,
    votes_election2 = votes_election1,
    new_and_exit_voters = "simultaneous",
    iter.max = iter.max,
    min.first = min.first,
    integers = integers,
    tol = tol,
    verbose = verbose
  )
  fit_21$VTM.sequence <- NULL
  fit_21$HETe.sequence <- NULL
  fit_21$argg <- NULL

  # Rueckwaertsmatrizen transponieren und beide Richtungen lokal kombinieren.
  local_votes <- combine_dual_votes(
    votes_12 = fit_12$VTM.votes.units,
    votes_21 = fit_21$VTM.votes.units,
    HETe_12 = fit_12$HETe,
    HETe_21 = fit_21$HETe
  )
  global_votes_average <- apply(local_votes$average, c(1L, 2L), sum)
  global_votes_weighted <- apply(local_votes$weighted, c(1L, 2L), sum)

  output <- list(
    VTM.votes.w = global_votes_weighted,
    VTM.votes.units.w = local_votes$weighted,
    VTM.votes.a = global_votes_average,
    VTM.votes.units.a = local_votes$average,
    HETe.w = get_lphom_internal("HET_MT.votos_MT.prop_Y")(local_votes$weighted)$HET,
    HETe.a = get_lphom_internal("HET_MT.votos_MT.prop_Y")(local_votes$average)$HET,
    VTM12.w = row_standardize_matrix(global_votes_weighted),
    VTM21.w = row_standardize_matrix(t(global_votes_weighted)),
    VTM12.a = row_standardize_matrix(global_votes_average),
    VTM21.a = row_standardize_matrix(t(global_votes_average)),
    nslphom.object.12 = fit_12,
    nslphom.object.21 = fit_21,
    inputs = list(
      votes_election1 = votes_election1,
      votes_election2 = votes_election2,
      iter.max = iter.max,
      min.first = min.first,
      integers = integers,
      solver = "osqp",
      tol = tol
    )
  )
  class(output) <- c("nslphom_dual", "ei_dual", "lphom")
  output
}

# Dimensionsnamen, EHet und Solvermetadaten am Dual-Fit fuer den Workflow ergaenzen.
prepare_dual_fit_for_workflow <- function(fit, solver) {
  fit_12 <- fit$nslphom.object.12
  matrix_dimnames <- list(
    colnames(fit_12$origin),
    colnames(fit_12$destination)
  )
  local_matrix_dimnames <- c(matrix_dimnames, list(rownames(fit_12$origin)))

  dimnames(fit$VTM.votes.units.w) <- local_matrix_dimnames
  dimnames(fit$VTM.votes.units.a) <- local_matrix_dimnames
  dimnames(fit$VTM.votes.w) <- matrix_dimnames
  dimnames(fit$VTM.votes.a) <- matrix_dimnames
  dimnames(fit$VTM12.w) <- matrix_dimnames
  dimnames(fit$VTM12.a) <- matrix_dimnames
  dimnames(fit$VTM21.w) <- rev(matrix_dimnames)
  dimnames(fit$VTM21.a) <- rev(matrix_dimnames)

  fit$origin <- fit_12$origin
  fit$destination <- fit_12$destination
  fit$EHet <- fit$destination - fit$origin %*% fit$VTM12.w
  fit$inputs$solver <- solver
  fit$inputs$osqp_local_solver <- if (identical(solver, "osqp")) {
    getOption("waehlendenwanderung.osqp_local_solver", "lp_solve")
  } else {
    NA_character_
  }
  fit
}

# nslphom_dual mit OSQP, Symphony oder lp_solve ausfuehren und speichersparend aufbereiten.
fit_nslphom_dual_model <- function(
    origin_counts,
    destination_counts,
    iter_max = getOption("waehlendenwanderung.nslphom_iter_max", 10L),
    tol = getOption("waehlendenwanderung.nslphom_tol", 1e-5),
    solver = getOption("waehlendenwanderung.nslphom_solver", "osqp"),
    verbose = FALSE,
    method = "lphom::nslphom_dual") {
  check_lphom_available()
  solver <- match.arg(solver, c("lp_solve", "symphony", "osqp"))

  # OSQP braucht den eigenen Dual-Wrapper; die anderen Solver bietet lphom direkt an.
  if (solver == "osqp") {
    fit <- nslphom_dual_osqp(
      votes_election1 = as.data.frame(origin_counts),
      votes_election2 = as.data.frame(destination_counts),
      iter.max = iter_max,
      min.first = FALSE,
      integers = FALSE,
      tol = tol,
      verbose = verbose
    )
  } else {
    fit <- lphom::nslphom_dual(
      votes_election1 = as.data.frame(origin_counts),
      votes_election2 = as.data.frame(destination_counts),
      iter.max = iter_max,
      min.first = FALSE,
      integers = FALSE,
      solver = solver,
      tol = tol
    )
  }

  # Iterationshistorien entfernen; fuer Hauptanalyse und Regression werden nur Endmatrizen benoetigt.
  fit$nslphom.object.12$VTM.sequence <- NULL
  fit$nslphom.object.12$HETe.sequence <- NULL
  fit$nslphom.object.12$argg <- NULL
  fit$nslphom.object.21$VTM.sequence <- NULL
  fit$nslphom.object.21$HETe.sequence <- NULL
  fit$nslphom.object.21$argg <- NULL
  fit <- prepare_dual_fit_for_workflow(fit, solver = solver)
  attr(fit, "method_label") <- method
  attr(fit, "solver") <- solver
  fit
}

# Lokale Matrizen in eine Zeile je Einheit, Herkunft und Zielgruppe umformen.
local_matrices_to_long <- function(fit, ids, method = "lphom::nslphom") {
  if (inherits(fit, "nslphom_dual")) {
    votes_units <- fit[["VTM.votes.units.w"]]
    prop_units <- local_votes_to_probabilities(votes_units)
  } else {
    prop_units <- fit[["VTM.prop.units"]]
    votes_units <- fit[["VTM.votes.units"]]
  }

  stopifnot(length(dim(prop_units)) == 3)
  stopifnot(identical(dim(prop_units), dim(votes_units)))
  stopifnot(dim(prop_units)[[3]] == length(ids))

  dplyr::bind_rows(lapply(seq_along(ids), function(i) {
    prop_matrix <- prop_units[, , i]
    votes_matrix <- votes_units[, , i]

    origin_count <- rowSums(votes_matrix, na.rm = TRUE)
    destination_count <- colSums(votes_matrix, na.rm = TRUE)
    matrix_cells <- as.data.frame(as.table(prop_matrix), stringsAsFactors = FALSE) %>%
      dplyr::mutate(
        from = as.character(.data$Var1),
        to = as.character(.data$Var2),
        row_index = match(.data$from, rownames(votes_matrix)),
        col_index = match(.data$to, colnames(votes_matrix))
      )

    matrix_cells %>%
      dplyr::transmute(
        agg_schluessel = ids[[i]],
        from = .data$from,
        to = .data$to,
        transition_probability = as.numeric(.data$Freq),
        origin_count = as.numeric(origin_count[.data$from]),
        destination_count = as.numeric(destination_count[.data$to]),
        estimated_transition_count = as.numeric(votes_matrix[cbind(.data$row_index, .data$col_index)]),
        method = method
      )
  }))
}

# Eine globale Matrix in eine Zeile je Herkunft-Ziel-Kombination umformen.
matrix_to_long <- function(prop_matrix, votes_matrix, matrix_scope, method = "lphom::nslphom") {
  origin_count <- rowSums(votes_matrix, na.rm = TRUE)
  destination_count <- colSums(votes_matrix, na.rm = TRUE)
  matrix_cells <- as.data.frame(as.table(prop_matrix), stringsAsFactors = FALSE) %>%
    dplyr::mutate(
      from = as.character(.data$Var1),
      to = as.character(.data$Var2),
      row_index = match(.data$from, rownames(votes_matrix)),
      col_index = match(.data$to, colnames(votes_matrix))
    )

  matrix_cells %>%
    dplyr::transmute(
      matrix_scope = matrix_scope,
      from = .data$from,
      to = .data$to,
      transition_probability = as.numeric(.data$Freq),
      origin_count = as.numeric(origin_count[.data$from]),
      destination_count = as.numeric(destination_count[.data$to]),
      estimated_transition_count = as.numeric(votes_matrix[cbind(.data$row_index, .data$col_index)]),
      method = method
    )
}

# Zeilensummen, Wahlraender, Iterationen, HETe und Solver des Fits kontrollieren.
make_nslphom_checks <- function(
    fit,
    transition_long,
    block_id = NA_character_,
    method = "lphom::nslphom",
    threshold = NA_real_,
    blocked = TRUE) {
  is_dual <- inherits(fit, "nslphom_dual")
  if (is_dual) {
    votes_units <- fit[["VTM.votes.units.w"]]
    prop_units <- local_votes_to_probabilities(votes_units)
    origin_used <- as.matrix(fit$nslphom.object.12$origin)
    destination_used <- as.matrix(fit$nslphom.object.12$destination)
  } else {
    prop_units <- fit[["VTM.prop.units"]]
    votes_units <- fit[["VTM.votes.units"]]
    origin_used <- as.matrix(fit[["origin"]])
    destination_used <- as.matrix(fit[["destination"]])
  }
  solver_used <- if (!is.null(fit[["inputs"]][["solver"]])) {
    fit[["inputs"]][["solver"]]
  } else if (!is.null(attr(fit, "solver"))) {
    attr(fit, "solver")
  } else {
    NA_character_
  }
  osqp_local_solver <- if (!is.null(fit[["inputs"]][["osqp_local_solver"]])) {
    fit[["inputs"]][["osqp_local_solver"]]
  } else {
    NA_character_
  }

  origin_from_units <- t(vapply(
    seq_len(dim(votes_units)[[3]]),
    function(i) rowSums(votes_units[, , i], na.rm = TRUE),
    numeric(dim(votes_units)[[1]])
  ))
  colnames(origin_from_units) <- dimnames(votes_units)[[1]]

  destination_from_units <- t(vapply(
    seq_len(dim(votes_units)[[3]]),
    function(i) colSums(votes_units[, , i], na.rm = TRUE),
    numeric(dim(votes_units)[[2]])
  ))
  colnames(destination_from_units) <- dimnames(votes_units)[[2]]

  origin_used <- origin_used[, colnames(origin_from_units), drop = FALSE]
  destination_used <- destination_used[, colnames(destination_from_units), drop = FALSE]

  tibble::tibble(
    nslphom_block = block_id,
    n_units = dim(prop_units)[[3]],
    n_origin_groups = dim(prop_units)[[1]],
    n_destination_groups = dim(prop_units)[[2]],
    max_abs_row_sum_error = transition_long %>%
      dplyr::group_by(.data$agg_schluessel, .data$from) %>%
      dplyr::summarise(
        origin_count = max(.data$origin_count, na.rm = TRUE),
        row_sum = sum(.data$transition_probability),
        .groups = "drop"
      ) %>%
      dplyr::filter(.data$origin_count > 0) %>%
      dplyr::summarise(value = max(abs(.data$row_sum - 1), na.rm = TRUE)) %>%
      dplyr::pull(value),
    max_abs_origin_reconstruction_error = max(abs(origin_from_units - origin_used), na.rm = TRUE),
    max_abs_destination_reconstruction_error = max(abs(destination_from_units - destination_used), na.rm = TRUE),
    model = if (is_dual) "nslphom_dual" else "nslphom",
    iter = if (is_dual) {
      max(fit$nslphom.object.12$iter, fit$nslphom.object.21$iter)
    } else {
      fit[["iter"]]
    },
    iter_min = if (is_dual) NA_integer_ else fit[["iter.min"]],
    iter_12 = if (is_dual) fit$nslphom.object.12$iter else NA_integer_,
    iter_21 = if (is_dual) fit$nslphom.object.21$iter else NA_integer_,
    iter_min_12 = if (is_dual) fit$nslphom.object.12$iter.min else NA_integer_,
    iter_min_21 = if (is_dual) fit$nslphom.object.21$iter.min else NA_integer_,
    HETe = if (is_dual) fit[["HETe.w"]] else fit[["HETe"]],
    HETe_12 = if (is_dual) fit$nslphom.object.12$HETe else NA_real_,
    HETe_21 = if (is_dual) fit$nslphom.object.21$HETe else NA_real_,
    HETe_init = if (is_dual) NA_real_ else fit[["solution_init"]][["HETe_init"]],
    method = method,
    package = "lphom",
    package_version = as.character(utils::packageVersion("lphom")),
    new_and_exit_voters = "simultaneous",
    solver = solver_used,
    osqp_local_solver = osqp_local_solver,
    threshold = threshold,
    blocked = blocked
  )
}

# Eine Zeile je agg_schluessel mit Spalten wie p_SPD_to_AfD erzeugen.
make_transition_wide <- function(transition_long) {
  transition_long %>%
    dplyr::mutate(transition = paste0("p_", .data$from, "_to_", .data$to)) %>%
    dplyr::select(agg_schluessel, transition, transition_probability) %>%
    tidyr::pivot_wider(
      names_from = transition,
      values_from = transition_probability
    ) %>%
    dplyr::arrange(.data$agg_schluessel)
}

# Zentrale Modellparameter und Inputdimensionen als reproduzierbare Settings-Tabelle festhalten.
make_unblocked_settings <- function(
    inputs,
    validation,
    threshold = 0.12,
    iter_max = 10L,
    tol = 1e-5,
    solver = getOption("waehlendenwanderung.nslphom_solver", "osqp"),
    blocked = FALSE,
    model = "nslphom") {
  tibble::tibble(
    threshold = threshold,
    model = model,
    keep_parties = paste(validation$kept_parties, collapse = ", "),
    groups = paste(validation$group_names, collapse = ", "),
    blocked = blocked,
    n_units = nrow(inputs$input2021),
    n_groups = length(validation$group_names),
    iter_max = iter_max,
    tol = tol,
    new_and_exit_voters = "simultaneous",
    solver = solver,
    osqp_local_solver = if (identical(solver, "osqp")) {
      getOption("waehlendenwanderung.osqp_local_solver", "lp_solve")
    } else {
      NA_character_
    }
  )
}
