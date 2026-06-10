# Test script for debugging
# This script will run CleanLabValuesDataset on an Example dataset, save the output and compare output to ground truth, if any

library(data.table)
# Load modular code
source("R/load_dependencies.R")
load_cleanlab()

examples <- c("Example 5")
base_path <- "tests/data"

for (ex in examples) {
  cat("\nRunning test for", ex, "...\n")
  input_dir <- file.path(base_path, ex, "i_input")
  gt_dir <- file.path(base_path, ex, "i_ground_truth")
  out_dir <- file.path(base_path, ex, "g_output")
  dir.create(out_dir, showWarnings = F)
  
  dataset_lab_values <- fread(file.path(input_dir, "dataset_lab_values.csv"))
  path_lab_target_units <- file.path(input_dir, "LAB_target_units.csv")
  path_unit_conversion <- file.path(input_dir, "LAB_unit_conversion.csv")
#  path_unit_conversion <- file.path(input_dir, "LAB_unit_conversion_no_DS.csv")
  path_lab_thresholds <- file.path(input_dir, "LAB_threshold.csv")

  # Run cleaning
  cleaned <- CleanLabValuesDataset(
    dataset = dataset_lab_values,
    datasource = "DS_A",
    lab_target_units = path_lab_target_units,
    lab_unit_conversion = path_unit_conversion,
    lab_thresholds = path_lab_thresholds
  )

  # Save output
  fwrite(cleaned, file.path(out_dir,"dataset_cleaned_output_lab_values.csv"))
  
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
      # print differences for debugging
      print(res)
    }
  } else {
    cat("Ground truth file missing for", ex, "\n")
  }
}
