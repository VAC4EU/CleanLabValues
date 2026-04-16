# Authors: Rosa Gini, Yinan Mao

# 16 Apr 2026

# Version 0.2
# Set up checks for the arguments
# set up Examples 1, ..., 3

# 15 Apr 2026

# Version 0.1
# Set up function

#'CleanLabValuesDataset
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
#'
#' @seealso
#'
#' ...
#'
#'
# 

CleanLabValuesDataset <- function(dataset, list_analyses = c(), lab_target_units, lab_unit_conversion, lab_thresholds, datasource = "") {
  
  if (!require("data.table")) install.packages("data.table")
  library(data.table)
  
  # this function checks whether a string is a valid logical condition for a data.table
  is_valid_dt_condition <- function(dt, cond) {
    # the condition must be a single character string
    if (!is.character(cond) || length(cond) != 1L || is.na(cond)) {
      return(FALSE)
    }
    
    # try to parse the string
    expr <- tryCatch(parse(text = cond), error = function(e) NULL)
    if (is.null(expr) || length(expr) != 1L) {
      return(FALSE)
    }
    
    expr <- expr[[1L]]
    
    # check that all variable names used in the expression exist in dt
    vars_used <- all.vars(expr)
    if (!all(vars_used %in% names(dt))) {
      return(FALSE)
    }
    
    # try to evaluate the expression inside dt
    value <- tryCatch(dt[, eval(expr)], error = function(e) NULL)
    if (is.null(value)) {
      return(FALSE)
    }
    
    # the result must be logical
    if (!is.logical(value)) {
      return(FALSE)
    }
    
    # the logical result must have length 1 or nrow(dt)
    if (!(length(value) %in% c(1L, nrow(dt)))) {
      return(FALSE)
    }
    
    TRUE
  }
  
  
  ##############################
  # dataset of lab values
  
  # check data model
  
  for (varname in c("concept_id","value","unit")) {
    if (!(varname %in% names(dataset))) {
      errmess <- paste("The file 'dataset' should include the variable",varname,"in its data model" )
      stop(errmess)
    }
  }
  
    
  ##############################
  # load lab unit 
  
  if (!(file.exists(lab_target_units))) {
    errmess <- paste("The file",lab_target_units,"cannot be found" )
    stop(errmess)
  
  }
  
  METADATA_lab_target_units <- fread(lab_target_units)

  # if a lab measurement has no unit this is "NA"

  METADATA_lab_target_units[is.na(unit_target), unit_target := "NA"]  

  # check data model
  
  if (!("concept_id" %in% names(METADATA_lab_target_units)) | !("unit_target"  %in% names(METADATA_lab_target_units))) {
    errmess <- paste("The file",lab_target_units,"should be a csv file with data model concept_id,unit_target" )
    stop(errmess)
  }
  
  # 1 lab_target_units - if list_analysis is empty, then all lab values are analysed
  
  if (length(list_analyses) == 0) {
    list_analyses <- unique(unlist(METADATA_lab_target_units[,.(concept_id)]))
  }
  
  # 2 lab_target_units -assign target_unit to each concepts_id of lab value
  
  target_unit <- list() 
  
  for (variable in unique(c(list_analyses))) {
    if (METADATA_lab_target_units[concept_id == variable, uniqueN(unit_target)] != 1) {
      stop(paste0("Expected exactly one unit_target for concept_id ", variable, ", but found ",
                  METADATA_lab_target_units[concept_id == variable, uniqueN(unit_target)], "."))
    }
    target_unit[[variable]] <- trimws(unlist(METADATA_lab_target_units[concept_id == variable,.(unit_target)]))
  }
  
  ##############################
  # load lab unit conversion

  if (!(file.exists(lab_unit_conversion))) {
    errmess <- paste("The file",lab_unit_conversion,"cannot be found" )
    stop(errmess)
  }
    
  METADATA_lab_unit_conversion <- fread(lab_unit_conversion)
  
  # check data model
  
  for (varname in c("concept_id","unit_origin","unit_target","multiplication_factor_from_origin_to_target","conversion_rate","condition_on_value","assumed_unit_if_missing","next_attempt")) {
    if (!(varname %in% names(METADATA_lab_unit_conversion))) {
      errmess <- paste("The file",lab_unit_conversion,"should include the variable",varname,"in its data model" )
      stop(errmess)
    }
  }
 
  if (datasource != "" & !("datasource" %in% names(METADATA_lab_unit_conversion))) {
    errmess <- paste("You specified the argument 'datasource' when calling the function CleanLabValues, but in this case the file",lab_unit_conversion,"should include the variable 'datasource' in its data model, but it does not" )
    stop(errmess)
  }
  
  METADATA_lab_unit_conversion[is.na(unit_target), unit_target := "NA"]
  
  
  
  ##
  # verify that assignments are correct
  # 1 unit thresholds - all target units are consistent with those assigned in LAB_target_units
  
  
  for (variable in unique(c(list_analyses))) {
    if (nrow(METADATA_lab_unit_conversion[concept_id == variable,.(unit_target)]) > 0) {
      if (trimws(unique(METADATA_lab_unit_conversion[concept_id == variable,.(unit_target)])) != target_unit[[variable]] ) {
        errmess <- paste("In the lab_unit_conversion, the concept_id",variable,"is described with target units that are inconsistent with the target unit assigned in the tab LAB_target_units, which is",target_unit[[variable]] )
        stop(errmess)
      } 
    }
  }
  
  # 2 unit thresholds - METADATA_lab_unit_conversion when unit_origin != "MISSING" has unique key (concept_id,unit_origin,unit_target)
  
  listvar <- c("concept_id","unit_origin","unit_target")
  if (nrow(METADATA_lab_unit_conversion[unit_origin != "MISSING",]) > 0) {
    if (METADATA_lab_unit_conversion[unit_origin != "MISSING", .N, by = listvar][, max(N)] > 1) {
      temp <- METADATA_lab_unit_conversion[unit_origin != "MISSING", .N, by = listvar]
      print(temp[N>1,])
      errmess <- paste("In the file lab_unit_conversion there are values of concept_id that have more than one row for the same combination of (unit_origin,unit_target), they are listed above (we are excluding rows where unit_origin == 'MISSING')")
      stop(errmess) 
    }
  }

  # 3 unit thresholds - METADATA_lab_unit_conversion has never next_attempt missing
  
  if (nrow(METADATA_lab_unit_conversion[is.na(next_attempt),]) > 0) {
    if (METADATA_lab_unit_conversion[is.na(next_attempt), .N] > 0) {
      temp <- METADATA_lab_unit_conversion[is.na(next_attempt),.(concept_id) ]
      print(temp[,])
      errmess <- paste("In the file lab_unit_conversion there are rows with next_attempt empty, the corresponding concept_ids are listed above. This cannot happen! Admissible values are 1, 2, ..., 99")
      stop(errmess)
    }
    
  }
  
  # 4 unit thresholds - METADATA_lab_unit_conversion with MISSING unit_origin have only one row per combination of concept_id,database,condition_on_value,next_attempt
  
  
  listvar <- c("concept_id","datasource","condition_on_value","next_attempt")
  if (nrow(METADATA_lab_unit_conversion[unit_origin == "MISSING",]) > 0) {
    if (METADATA_lab_unit_conversion[unit_origin == "MISSING", .N, by = listvar][, max(N)] > 1) {
      temp <- METADATA_lab_unit_conversion[unit_origin == "MISSING", .N, by = listvar]
      print(temp[N>1,])
      errmess <- paste("In the file lab_unit_conversion there are values of concept_id with MISSING unit_origin that have more than one row for the same combination of (datasource,condition_on_value,next_attempt), they are listed above")
      stop(errmess)
    }
    
  }
  
  
  ##############################
  # load lab unit thresholds

  if (!(file.exists(lab_thresholds))) {
    errmess <- paste("The file",lab_thresholds,"cannot be found" )
    stop(errmess)
  }
  
    
  METADATA_lab_threshold <- fread(lab_thresholds)
  
  # check data model
  
  for (varname in c("concept_id","Min","Max","unit_target","condition_on_variable","variable")) {
    if (!(varname %in% names(METADATA_lab_threshold))) {
      errmess <- paste("The file",lab_threshold,"should include the variable",varname,"in its data model" )
      stop(errmess)
    }
  }
  
  # 1 unit thresholds - if 'variable' is non-empty, it should contain a space-separated list of variables of the dataset
  
  variables_condition <- unique(METADATA_lab_threshold[!is.na(variable),.(variable)])
  if (nrow(variables_condition) > 0) {
    variables_condition <- unlist(strsplit(trimws(variables_condition), " +"))
    
    for (varname in variables_condition) {
      if (!(varname %in% names(dataset))) {
        errmess <- paste("The dataset you are cleaning should include the variable",varname,"in its data model because it is used as a condition for one of the thresholds, as indicated in the column 'variable' of the file",lab_thresholds,"\n" )
        stop(errmess)
      }
    }
    
    conditions_on_variable <- unlist(unique(METADATA_lab_threshold[,.(condition_on_variable)]))
    
    # if 'condition_on_variable' is non-empty, it should contain a valid logical expression
    
    dt <- dataset[1:max(2,nrow(dataset)),..variables_condition]
    
    for (thiscond in conditions_on_variable) {
      check <- is_valid_dt_condition(dt, thiscond)
      if (!check) {
        errmess <- paste("The column 'condition_on_variable' of the file",lab_thresholds,"should contain valid conditions on the dataset, limited to the variable(s)",variables_condition,"\n" )
        stop(errmess)
      }
    }
    
  }
  
  # 2 unit thresholds -  the non-missing condition_on_value of METADATA_lab_unit_conversion with MISSING unit_origin are valid conditions on dataset, limited to the variable 'value'
  
  conditions_for_conversion <- unique(METADATA_lab_unit_conversion[unit_origin== "MISSING" & !is.na(condition_on_value) & condition_on_value != "" ,.(condition_on_value)])
  if (nrow(  conditions_for_conversion) > 0) {
    conditions_for_conversion <- unlist(unique(METADATA_lab_unit_conversion[unit_origin== "MISSING" & !is.na(condition_on_value) & condition_on_value != "",.(condition_on_value)]))
    
    # if 'conditions_for_conversion' is non-empty, it should contain a valid logical expression
    
    dt <- dataset[1:max(2,nrow(dataset)),.(value)]

    for (thiscond in conditions_for_conversion) {
      check <- is_valid_dt_condition(dt, thiscond)
      if (!check) {
        errmess <- paste("The column 'condition_on_value' of the file",lab_unit_conversion,"should contain valid conditions on the dataset, limited to the variable 'value'\n" )
        stop(errmess)
      }
    }
    
  }
  
  
  # 3 unit thresholds -  in rows of METADATA_lab_unit_conversion with MISSING unit_origin, the values of next_attempt that are not 99 should be an uninterrupted sequence from 1 to the maximum number (< 99)
  
  rows_with_attemps <- unique(METADATA_lab_unit_conversion[unit_origin == "MISSING" & next_attempt != 99,])
  
  concept_id_with_attempts <- unlist(unique(rows_with_attemps[,.(concept_id)]))
  
  for (conc in concept_id_with_attempts) {
    thiscolumn <- unique(rows_with_attemps[concept_id == conc,.(next_attempt)])
    setorder(thiscolumn,- next_attempt)
    if (unlist(thiscolumn[1,.(next_attempt)]) != nrow(thiscolumn)) {
      errmess <- paste("The concept_id",conc, "in the file",lab_unit_conversion,"in the rows with unit_origin == 'MISSING', has the column 'next_attempt' should contain an uninterrupted series of integers between 1 its maximum < 99, while it has some gaps instead\n" )
      stop(errmess)
    }
  }
  
  
    
  ###########################################################
  #
  # START FUNCTION
  #
  ###########################################################
  
  processing <- dataset
  
  # ...
  
  return(processing)
}
  
