# Functions for metadata and input checks

is_valid_dt_condition <- function(dt, cond) {
  if (!is.character(cond) || length(cond) != 1L || is.na(cond)) {
    return(FALSE)
  }
  expr <- tryCatch(parse(text = cond), error = function(e) NULL)
  if (is.null(expr) || length(expr) != 1L) {
    return(FALSE)
  }
  expr <- expr[[1L]]
  vars_used <- all.vars(expr)
  if (!all(vars_used %in% names(dt))) {
    return(FALSE)
  }
  value <- tryCatch(dt[, eval(expr)], error = function(e) NULL)
  if (is.null(value)) {
    return(FALSE)
  }
  if (!is.logical(value)) {
    return(FALSE)
  }
  if (!(length(value) %in% c(1L, nrow(dt)))) {
    return(FALSE)
  }
  TRUE
}

check_dataset_model <- function(dataset) {
  for (varname in c("concept_id", "value", "unit")) {
    if (!(varname %in% names(dataset))) {
      message <- paste("The file 'dataset' should include the variable", varname, "in its data model")
      stop(message)
    }
  }
  message("[CleanLabValues] Dataset model check passed successfully.")
}

check_lab_target_units <- function(lab_target_units) {
  if (!(file.exists(lab_target_units))) {
    stop(paste("The file", lab_target_units, "cannot be found"))
  }
  dt <- data.table::fread(lab_target_units)
  if (!all(c("concept_id", "unit_target") %in% names(dt))) {
    stop(paste("The file", lab_target_units, "should be a csv file with data model concept_id,unit_target"))
  }
  message("[CleanLabValues] LAB_target_units check passed successfully.")
  invisible(dt)
}

check_lab_unit_conversion <- function(lab_unit_conversion, datasource, list_analyses, target_unit) {
  if (!(file.exists(lab_unit_conversion))) {
    stop(paste("The file", lab_unit_conversion, "cannot be found"))
  }
  dt <- data.table::fread(lab_unit_conversion)
  required <- c("concept_id", "unit_origin", "unit_target", "multiplication_factor_from_origin_to_target", "condition_on_value", "assumed_unit_if_missing", "next_attempt")
  for (varname in required) {
    if (!(varname %in% names(dt))) {
      stop(paste("The file", lab_unit_conversion, "should include the variable", varname, "in its data model"))
    }
  }
  dt[,multiplication_factor_from_origin_to_target := as.numeric(multiplication_factor_from_origin_to_target)]
  if (nrow(dt[is.na(multiplication_factor_from_origin_to_target),]) > 0) {
    varname <- "conversion_not_multiplication"
    if (!(varname %in% names(dt))) {
      nummissing_mult <- nrow(dt[is.na(multiplication_factor_from_origin_to_target),])
      stop(paste("The file", lab_unit_conversion, "has", nummissing_mult,"record(s) whose variable multiplication_factor_from_origin_to_target is missing or nonmeric. This can only happen if the variable", varname, "is in its data model, and no record of the file can have both variables missing"))
    }else{
      dt[,(varname) := as.character(get(varname))]
    nummissing_both_specifications <- nrow(dt[is.na(multiplication_factor_from_origin_to_target) & nchar(get(varname)) == 0,])
    if (nummissing_both_specifications > 0) {
      stop(paste("The file", lab_unit_conversion, "has", nummissing_both_specifications,"record(s) whose variables multiplication_factor_from_origin_to_target and",varname,"are both missing. No record of the file can have both variables missing. This error is also triggered if multiplication_factor_from_origin_to_target has some non-numeric values."))
    }
  }
  }
  if (datasource != "" & !("datasource" %in% names(dt))) {
    stop(paste("You specified the argument 'datasource' but the file", lab_unit_conversion, "should include the variable 'datasource' in its data model, while it does not"))
  }
  for (variable in unique(c(list_analyses))) {
    if (nrow(dt[concept_id == variable, .(unit_target)]) > 0) {
      if (trimws(unique(dt[concept_id == variable, .(unit_target)])) != target_unit[[variable]]) {
        stop(paste("In the lab_unit_conversion, the concept_id", variable, "is described with target units that are inconsistent with the target unit assigned in the tab LAB_target_units, which is", target_unit[[variable]]))
      }
    }
  }
  message("[CleanLabValues] LAB_unit_conversion check passed successfully.")
  invisible(dt)
}

check_lab_thresholds <- function(lab_thresholds, dataset) {
  if (!(file.exists(lab_thresholds))) {
    stop(paste("The file", lab_thresholds, "cannot be found"))
  }
  dt <- data.table::fread(lab_thresholds)
  required <- c("concept_id", "Min", "Max", "unit_target", "condition_on_variable", "variable")
  for (varname in required) {
    if (!(varname %in% names(dt))) {
      stop(paste("The file", lab_thresholds, "should include the variable", varname, "in its data model"))
    }
  }
  message("[CleanLabValues] LAB_thresholds check passed successfully.")
  invisible(dt)
}
