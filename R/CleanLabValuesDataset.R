# Authors: Rosa Gini, Yinan Mao

# 15 Apr 2026

# Version 0.1
# Set up function

#'CleanLabValuesDataset
#'
#' The function CleanLabValuesDataset ingests instructions to clean datasets containing results from laboratory analysis. The instructions specify which unit of measurement is desired for each laboratory analysis, what the conversion rules are, what to do if unit of measurement is missing, what the values should be considered absurd and discarded.
#'
#'
#' @param dataset the name of a data.table file in memory that contains a dataset of results fro laboratory analyses that needs cleaning
#' @param par_target_units a string containing the path towards a csv files containing one record per each type of laboratory analysis and specifying the edsired unit of measurement
#' @param par_conversions ...
#' @param par_thresholds ...
#' @param diroutput (optional) the directory where the output concept sets datasets will be saved. If not provided the working directory is considered.

#'
#'
#' @details
#'
#'  ...
#'
#' @seealso
#'
#' ...
#'
#'
# 

CleanLabValuesDataset <- function(dataset, par_target_units, par_conversions, par_thresholds, diroutput = getwd()) {
  
}
  
