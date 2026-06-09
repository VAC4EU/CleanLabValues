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
#' @seealso
#'
#' ...
#'
#'
#

CleanLabValuesDataset <- function(dataset, list_analyses = c(), lab_target_units, lab_unit_conversion, lab_thresholds, datasource = "") {
  # Basic validation of dataset and metadata files
  if (!is.data.frame(dataset) && !data.table::is.data.table(dataset)) stop("`dataset` must be a data.frame or data.table")
  for (varname in c("concept_id", "value", "unit")) if (!(varname %in% names(dataset))) stop(paste("dataset must contain column", varname))
  for (path in c(lab_target_units, lab_unit_conversion, lab_thresholds)) if (!file.exists(path)) stop(paste("Missing metadata file:", path))

  # Run metadata checks (these functions also return the parsed metadata invisibly)
  check_dataset_model(dataset)
  meta_target_units <- check_lab_target_units(lab_target_units)

  # If list_analyses is empty, derive from target units
  if (length(list_analyses) == 0) list_analyses <- unique(meta_target_units$concept_id)

  # Build target unit mapping required by check_lab_unit_conversion
  target_unit <- setNames(meta_target_units$unit_target, meta_target_units$concept_id)
  check_lab_unit_conversion(lab_unit_conversion, datasource, list_analyses, target_unit)
  check_lab_thresholds(lab_thresholds, dataset)

  # Delegate to clean_lab_main which contains the full cleaning logic
  res <- clean_lab_main(
    dataset = dataset,
    list_analyses = list_analyses,
    lab_target_units = lab_target_units,
    lab_unit_conversion = lab_unit_conversion,
    lab_thresholds = lab_thresholds,
    datasource = datasource
  )
  return(res)
}
