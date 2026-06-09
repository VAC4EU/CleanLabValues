library(CleanLabValues)
library(data.table)

base_path <- function(...) test_path("data", ...)

run_example <- function(ex) {
  input_dir <- base_path(ex, "i_input")
  gt_dir    <- base_path(ex, "i_ground_truth")

  dataset              <- fread(file.path(input_dir, "dataset_lab_values.csv"))
  path_target_units    <- file.path(input_dir, "LAB_target_units.csv")
  path_unit_conversion <- file.path(input_dir, "LAB_unit_conversion.csv")
  path_thresholds      <- file.path(input_dir, "LAB_threshold.csv")

  cleaned <- CleanLabValuesDataset(
    dataset          = dataset,
    lab_target_units = path_target_units,
    lab_unit_conversion = path_unit_conversion,
    lab_thresholds   = path_thresholds
  )

  gt <- fread(file.path(gt_dir, "dataset_cleaned_lab_values.csv"))

  setorder(cleaned, person_id, concept_id)
  setorder(gt,      person_id, concept_id)

  all.equal(cleaned, gt, check.attributes = FALSE)
}

test_that("Example 1 matches ground truth", {
  expect_true(isTRUE(run_example("Example 1")))
})

test_that("Example 2 matches ground truth", {
  expect_true(isTRUE(run_example("Example 2")))
})

test_that("Example 3 matches ground truth", {
  expect_true(isTRUE(run_example("Example 3")))
})

test_that("Example 4 matches ground truth", {
  expect_true(isTRUE(run_example("Example 4")))
})
