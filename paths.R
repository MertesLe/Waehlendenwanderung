# Zentrale Ein- und Ausgabeordner des Projekts definieren und bei Bedarf anlegen.

data_dir_cleaned <- "Data/cleaned"
data_dir_intermediate <- "Data/intermediate"
data_dir_validation <- "Data/validierung"
data_dir_model_nslphom <- "Data/modeloutput/nslphom"
data_dir_model_regression <- "Data/modeloutput/regression"
data_dir_model_bootstrap <- "Data/modeloutput/bootstrap"
data_dir_model_nslphom_ost <- file.path(data_dir_model_nslphom, "ostdeutschland")
data_dir_model_nslphom_deutschland <- file.path(data_dir_model_nslphom, "deutschland")
data_dir_model_regression_ost <- file.path(data_dir_model_regression, "ostdeutschland")
data_dir_model_bootstrap_ost <- file.path(data_dir_model_bootstrap, "ostdeutschland")

# Alle im Workflow verwendeten Datenordner rekursiv anlegen.
ensure_data_dirs <- function() {
  dirs <- c(
    data_dir_cleaned,
    data_dir_intermediate,
    data_dir_validation,
    data_dir_model_nslphom,
    data_dir_model_nslphom_ost,
    data_dir_model_nslphom_deutschland,
    data_dir_model_regression,
    data_dir_model_regression_ost,
    data_dir_model_bootstrap,
    data_dir_model_bootstrap_ost
  )

  for (dir in dirs) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  invisible(dirs)
}
