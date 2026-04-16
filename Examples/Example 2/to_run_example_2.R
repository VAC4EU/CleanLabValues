# Example 2

# Threshold depending on age

rm(list=ls(all.names=TRUE))

if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

if (!require("data.table")) install.packages("data.table")
library(data.table)

suppressWarnings(dir.create(file.path(thisdir,"g_output"), recursive = T))

# load the function

source(file.path("..","..","R", "CleanLabValuesDataset.R"))

# set arguments

dataset_lab_values <- data.table::fread(file.path(thisdir,"i_input","dataset_lab_values.csv"))
path_lab_target_units <- file.path(thisdir,"i_input","LAB_target_units.csv")
path_unit_conversion <- file.path(thisdir,"i_input","LAB_unit_conversion.csv")
path_lab_thresholds <-  file.path(thisdir,"i_input","LAB_threshold.csv")

# run the function

cleaned_dataset <- CleanLabValuesDataset(dataset = dataset_lab_values, 
                                             lab_target_units = path_lab_target_units, 
                                             lab_unit_conversion = path_unit_conversion, 
                                             lab_thresholds = path_lab_thresholds
                                         )