library(CleanLabValues)
library(data.table)

# Verify that using an incorrect unit-conversion file (which passes schema
# validation but encodes the wrong conversion formula) produces results that
# differ from the ground truth, demonstrating the importance of accurate
# metadata.
test_that("Example 4 with wrong unit conversion does not match ground truth", {
  input_dir <- test_path("data", "Example 4", "i_input")
  gt_dir <- test_path("data", "Example 4", "i_ground_truth")

  dataset <- fread(file.path(input_dir, "dataset_lab_values.csv"))
  path_target_units <- file.path(input_dir, "LAB_target_units.csv")
  path_unit_conversion <- file.path(input_dir, "LAB_unit_conversion_wrong.csv")
  path_thresholds <- file.path(input_dir, "LAB_threshold.csv")

  cleaned <- CleanLabValuesDataset(
    dataset             = dataset,
    lab_target_units    = path_target_units,
    lab_unit_conversion = path_unit_conversion,
    lab_thresholds      = path_thresholds
  )

  gt <- fread(file.path(gt_dir, "dataset_cleaned_lab_values.csv"))

  setorder(cleaned, person_id, concept_id)
  setorder(gt, person_id, concept_id)

  expect_false(isTRUE(all.equal(cleaned, gt, check.attributes = FALSE)))
})
