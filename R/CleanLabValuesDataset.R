# Authors: Rosa Gini, Yinan Mao

# 15 Apr 2026

# Version 0.1
# Set up function

#'CleanLabValuesDataset
#'
#' The function CleanLabValuesDataset ingests instructions to clean datasets containing results from laboratory analysis. The instructions specify which unit of measurement is desired for each laboratory analysis, what the conversion rules are, what to do if unit of measurement is missing, what the values should be considered absurd and discarded.
#'
#'
#' @param dataset the name of a data.table file in memory that contains a dataset of results of laboratory analyses that needs cleaning
#' @param list_analyses a string vector containing the names of the laboratory analyses to be cleaned  
#' @param lab_target_units a string containing the path towards a csv files containing one record per each type of laboratory analysis in list_analyses and specifying the desired unit of measurement
#' @param lab_unit_conversions a string containing the path towards a csv files containing ...
#' @param lab_thresholds a string containing the path towards a csv files containing...
#' @param datasource a string containing name of the datasource that can be stored in lab_unit_conversions to produce a datasource-specific assumption on what to do if the unit of measurement is missing
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

CleanLabValuesDataset <- function(dataset, list_analyses = c(), lab_target_units, lab_unit_conversion, lab_thresholds, datasource = "") {
  
  METADATA_lab_unit_conversion <- fread(lab_unit_conversion)
  
  # clean
  
  METADATA_lab_unit_conversion[is.na(unit_target), unit_target := "NA"]
  
  
  
  ##
  # verify that assignments are correct
  # 1 - all target units are consistent with those assigned in LAB_target_units
  
  
  for (variable in unique(c(concept_ids_lab_values))) {
    if (trim(unique(METADATA_lab_unit_conversion[concept_id == variable,.(unit_target)])) != target_unit[[variable]]) {
      errmess <- paste("In the lab_unit_conversion, the concept_id",variable,"is described with target units that are inconsistent with the target unit assigned in the tab LAB_target_units, which is",target_unit[[variable]] )
      stop(errmess)
    }
  }
  
  # 2 - METADATA_lab_unit_conversion when unit_origin != "MISSING" has unique key (concept_id,unit_origin,unit_target)
  
  listvar <- c("concept_id","unit_origin","unit_target")
  if (METADATA_lab_unit_conversion[unit_origin != "MISSING", .N, by = listvar][, max(N)] > 1) {
    temp <- METADATA_lab_unit_conversion[unit_origin != "MISSING", .N, by = listvar]
    print(temp[N>1,])
    errmess <- paste("In the file lab_unit_conversion there are values of concept_id that have more than one row for the same combination of (unit_origin,unit_target), they are listed above (we are excluding rows where unit_origin == 'MISSING')")
    stop(errmess)
  }

  # 3 - METADATA_lab_unit_conversion has never next_attempt missing
  
  if (METADATA_lab_unit_conversion[is.na(next_attempt), .N] > 0) {
    temp <- METADATA_lab_unit_conversion[is.na(next_attempt),.(concept_id) ]
    print(temp[,])
    errmess <- paste("In the file lab_unit_conversion there are rows with next_attempt empty, the corresponding concept_ids are listed above. This cannot happen! Admissible values are 1, 2, ..., 99")
    stop(errmess)
  }
  
  # 4 - METADATA_lab_unit_conversion with MISSING unit_origin have only one row per combination of concept_id,database,condition_on_value,next_attempt
  
  
  listvar <- c("concept_id","datasource","condition_on_value","next_attempt")
  if (METADATA_lab_unit_conversion[unit_origin == "MISSING", .N, by = listvar][, max(N)] > 1) {
    temp <- METADATA_lab_unit_conversion[unit_origin == "MISSING", .N, by = listvar]
    print(temp[N>1,])
    errmess <- paste("In the file lab_unit_conversion there are values of concept_id with MISSING unit_origin that have more than one row for the same combination of (datasource,condition_on_value,next_attempt), they are listed above")
    stop(errmess)
  }
  
  
  
  processing <- dataset
  # ...
  
  return(processing)
}
  
