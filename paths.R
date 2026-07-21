data_dir_cleaned <- "Data/cleaned"
data_dir_intermediate <- "Data/intermediate"
data_dir_validation <- "Data/validierung"
data_dir_model_nslphom <- "Data/modeloutput/nslphom"
data_dir_model_regression <- "Data/modeloutput/regression"
data_dir_model_bootstrap <- "Data/modeloutput/bootstrap"

ensure_data_dirs <- function() {
  dirs <- c(
    data_dir_cleaned,
    data_dir_intermediate,
    data_dir_validation,
    data_dir_model_nslphom,
    data_dir_model_regression,
    data_dir_model_bootstrap
  )

  for (dir in dirs) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  invisible(dirs)
}
