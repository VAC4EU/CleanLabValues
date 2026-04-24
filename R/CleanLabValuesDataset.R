# Authors: Rosa Gini, Yinan Mao

# 16 Apr 2026

# Version 0.2
# Set up checks for the arguments
# set up Examples 1, ..., 3

# 15 Apr 2026

# Version 0.1
# Set up function

#' CleanLabValuesDataset
#'
#' The function CleanLabValuesDataset ingests instructions to clean datasets containing results from laboratory analysis. The instructions specify which unit of measurement is desired for each laboratory analysis, what the conversion rules are, what to do if unit of measurement is missing, what the values should be considered absurd and discarded.
#'
#'
#' @param dataset the name of a data.table file in memory that contains a dataset of results of laboratory analyses that needs cleaning
#' @param list_analyses a string vector containing the names of the laboratory analyses to be cleaned. If the argument is not specified, all the laboratory analyses are cleaned
#' @param lab_target_units a string containing the path towards a csv files containing one record per each type of laboratory analysis in list_analyses and specifying the desired unit of measurement
#' @param lab_unit_conversions a string containing the path towards a csv files containing the specifications to convert the values in the dataset to the target unit of measurement
#' @param lab_thresholds a string containing the path towards a csv files containing the specifications of which values should be considered absurd and discarded, possibly depending on other variables such as age
#' @param datasource (non mandatory) a string containing name of the datasource that can be stored in lab_unit_conversions to produce a datasource-specific assumption on what to do if the unit of measurement is missing
#'
#'
#' @details
#'
#'  ...
#' @examples
#' # Example usage with Example 1
if (!require("data.table")) install.packages("data.table")
library(data.table)
# Source all modular R scripts so clean_lab_main and dependencies are available
source("R/check_metadata.R")
source("R/fill_missing_unit.R")
source("R/convert_unit.R")
source("R/mo_convert.R")
source("R/apply_thresholds.R")
source("R/clean_lab_main.R")
#'
#' # Load input files
#' dataset_lab_values <- fread("tests/data/Example 1/i_input/dataset_lab_values.csv")
#' path_lab_target_units <- "tests/data/Example 1/i_input/LAB_target_units.csv"
#' path_unit_conversion <- "tests/data/Example 1/i_input/LAB_unit_conversion.csv"
#' path_lab_thresholds <- "tests/data/Example 1/i_input/LAB_threshold.csv"
#'
#' # Run metadata checks
#' check_dataset_model(dataset_lab_values)
#' check_lab_target_units(path_lab_target_units)
#' check_lab_unit_conversion(path_unit_conversion, "", c("WEIGHT", "HEIGHT", "LAB_BILIRUBIN"), list())
#' check_lab_thresholds(path_lab_thresholds, dataset_lab_values)
#'
#' # Run main cleaning function
#' cleaned <- clean_lab_main(
#'   dataset = dataset_lab_values,
#'   list_analyses = c("WEIGHT", "HEIGHT", "LAB_BILIRUBIN"),
#'   lab_target_units = path_lab_target_units,
#'   lab_unit_conversion = path_unit_conversion,
#'   lab_thresholds = path_lab_thresholds
#' )
#' @seealso
#'
#' ...
#'
#'
#

CleanLabValuesDataset <- function(dataset, list_analyses = c(), lab_target_units, lab_unit_conversion, lab_thresholds, datasource = "") {
  ##############################
  # dataset of lab values

  # check data model

  for (varname in c("concept_id", "value", "unit")) {
    if (!(varname %in% names(dataset))) {
      errmess <- paste("The file 'dataset' should include the variable", varname, "in its data model")
      stop(errmess)
    }
  }


  ##############################
  # load lab unit

  if (!(file.exists(lab_target_units))) {
    errmess <- paste("The file", lab_target_units, "cannot be found")
    stop(errmess)
  }

  METADATA_lab_target_units <- fread(lab_target_units)

  # if a lab measurement has no unit this is "NA"

  METADATA_lab_target_units[is.na(unit_target), unit_target := "NA"]

  # check data model

  if (!("concept_id" %in% names(METADATA_lab_target_units)) | !("unit_target" %in% names(METADATA_lab_target_units))) {
    errmess <- paste("The file", lab_target_units, "should be a csv file with data model concept_id,unit_target")
    stop(errmess)
  }

  # 1 lab_target_units - if list_analysis is empty, then all lab values are analysed

  if (length(list_analyses) == 0) {
    list_analyses <- unique(unlist(METADATA_lab_target_units[, .(concept_id)]))
  }

  # 2 lab_target_units -assign target_unit to each concepts_id of lab value

  target_unit <- list()

  for (variable in unique(c(list_analyses))) {
    if (METADATA_lab_target_units[concept_id == variable, uniqueN(unit_target)] != 1) {
      stop(paste0(
        "Expected exactly one unit_target for concept_id ", variable, ", but found ",
        METADATA_lab_target_units[concept_id == variable, uniqueN(unit_target)], "."
      ))
    }
    target_unit[[variable]] <- trimws(unlist(METADATA_lab_target_units[concept_id == variable, .(unit_target)]))
  }

  ##############################
  # load lab unit conversion

  if (!(file.exists(lab_unit_conversion))) {
    # Main cleaning function (logic ported from clean_lab_values.R)
    cleaned_dataset <-
      clean_lab_main(dataset, list_analyses, lab_target_units, lab_unit_conversion, lab_thresholds, datasource)
  }

  return(cleaned_dataset)
}
