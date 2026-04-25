# Test script for clean_lab_main
# This script will run clean_lab_main on all Example datasets and compare output to ground truth

library(data.table)
source("R/CleanLabValuesDataset.R")

examples <- c("Example 1", "Example 2", "Example 3")
base_path <- "tests/data"

for (ex in examples) {
  cat("\nRunning test for", ex, "...\n")
  input_dir <- file.path(base_path, ex, "i_input")
  gt_dir <- file.path(base_path, ex, "i_ground_truth")

  dataset_lab_values <- fread(file.path(input_dir, "dataset_lab_values.csv"))
  path_lab_target_units <- file.path(input_dir, "LAB_target_units.csv")
  path_unit_conversion <- file.path(input_dir, "LAB_unit_conversion.csv")
  path_lab_thresholds <- file.path(input_dir, "LAB_threshold.csv")

  # Run cleaning
  cleaned <- clean_lab_main(
    dataset = dataset_lab_values,
    lab_target_units = path_lab_target_units,
    lab_unit_conversion = path_unit_conversion,
    lab_thresholds = path_lab_thresholds
  )

  # Load ground truth
  gt_file <- file.path(gt_dir, "dataset_cleaned_lab_values.csv")
  if (file.exists(gt_file)) {
    gt <- fread(gt_file)
    # Order both by person_id and concept_id for fair comparison (no date column)
    setorder(cleaned, person_id, concept_id)
    setorder(gt, person_id, concept_id)

    res <- all.equal(cleaned, gt, check.attributes = FALSE)
    if (isTRUE(res)) {
      cat("Test PASSED for", ex, "\n")
    } else {
      cat("Test FAILED for", ex, "\n")
    }
  } else {
    cat("Ground truth file missing for", ex, "\n")
  }
}
