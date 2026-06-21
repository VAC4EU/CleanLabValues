library(CleanLabValues)
library(data.table)
library(testthat)

if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

num_example <- 7

input_dir <- file.path(thisdir,"..","..","tests","testthat","data",paste0("Example ",num_example),"i_input")

dataset <- fread(file.path(input_dir, "dataset_lab_values.csv"))
path_target_units <- file.path(input_dir, "LAB_target_units.csv")
path_unit_conversion <- file.path(input_dir, "LAB_unit_conversion.csv")
path_thresholds <- file.path(input_dir, "LAB_threshold.csv")

cleaned <- CleanLabValuesDataset(
  dataset = dataset,
  lab_target_units = path_target_units,
  lab_unit_conversion = path_unit_conversion,
  lab_thresholds = path_thresholds
)


setorder(cleaned, person_id, concept_id)

View(cleaned)

