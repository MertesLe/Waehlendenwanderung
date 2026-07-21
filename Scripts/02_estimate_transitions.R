library(dplyr)
library(tidyr)

source("paths.R", encoding = "UTF-8")
source("Functions/general_functions.R", encoding = "UTF-8")
source("Functions/nslphom_functions.R", encoding = "UTF-8")

ensure_data_dirs()

threshold <- getOption("waehlendenwanderung.party_threshold", 0.12)
block_prefix_length <- getOption("waehlendenwanderung.nslphom_block_prefix_length", 3L)
iter_max <- getOption("waehlendenwanderung.nslphom_iter_max", 10L)
tol <- getOption("waehlendenwanderung.nslphom_tol", 1e-5)

inputs <- read_prepared_nslphom_inputs()
validation <- validate_prepared_nslphom_inputs(inputs, threshold = threshold)

block_index <- make_nslphom_block_index(
  inputs$input2021,
  prefix_length = block_prefix_length
)

block_results <- fit_nslphom_blocks(
  block_index = block_index,
  input2021 = inputs$input2021,
  input2025 = inputs$input2025,
  iter_max = iter_max,
  tol = tol,
  verbose = FALSE,
  threshold = threshold
)

nslphom_fit <- make_nslphom_fit_bundle(
  block_results = block_results,
  block_index = block_index,
  prefix_length = block_prefix_length,
  iter_max = iter_max,
  tol = tol
)

transition_long <- bind_rows(lapply(block_results, `[[`, "transition_long")) %>%
  arrange(
    agg_schluessel,
    from,
    to
  )

transition_wide <- make_transition_wide(transition_long)
checks <- nslphom_fit$checks %>%
  arrange(nslphom_block)

write_blocked_nslphom_outputs(
  nslphom_fit = nslphom_fit,
  transition_long = transition_long,
  transition_wide = transition_wide,
  checks = checks,
  write_csv = TRUE
)
