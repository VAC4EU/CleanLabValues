#' Validate a condition string against a data.table
#'
#' Check that `cond` is a single, parseable R expression that only
#' references columns present in `dt` and evaluates to a logical vector of
#' length 1 or `nrow(dt)`.
#'
#' @param dt A `data.frame` or `data.table` containing the variables used in `cond`.
#' @param cond A single string containing an R expression that can be evaluated
#'   in the context of `dt` (e.g. "value > 0").
#' @return `TRUE` if the condition is valid, `FALSE` otherwise.
#' @keywords internal
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

#' Check dataset model
#'
#' Ensure `dataset` contains the minimal variables required by the
#' cleaning pipeline (`concept_id`, `value`, `unit`).
#'
#' @param dataset A `data.frame` or `data.table` representing the input dataset.
#' @return Invisibly returns `NULL` on success, otherwise throws an error.
#' @keywords internal
check_dataset_model <- function(dataset) {
  for (varname in c("concept_id", "value", "unit")) {
    if (!(varname %in% names(dataset))) {
      message <- paste("The file 'dataset' should include the variable", varname, "in its data model")
      stop(message)
    }
  }
  # we want to allow for the dataset to have non-numric values, this will be counted at the end of the cleaning process
  # for (varname in c("value")) {
  #   vec_ <- dataset[[varname]]
  #   is_num_ <- suppressWarnings(as.numeric(vec_))
  #   invalid_ <- is.na(is_num_)
  #   num_nonnumeric <- sum(invalid_)
  #   if (num_nonnumeric > 0) {
  #     stop(paste("The file 'dataset' has", num_nonnumeric,"record(s) where the variable ", varname," contains a non-numeric value."))
  #   }
  # }
  logger::log_info("[CleanLabValues] Dataset model check passed successfully.")
}

#######################################################
# lab_target_units

#' Check LAB_target_units file
#'
#' Validate that the `LAB_target_units` CSV exists and contains at least
#' `concept_id` and `unit_target` columns. Returns the parsed data.table
#' invisibly on success.
#'
#' @param lab_target_units Path to the `LAB_target_units` CSV file.
#' @return A `data.table` read from `lab_target_units` (invisibly).
#' @keywords internal
check_lab_target_units <- function(lab_target_units) {
  if (!(file.exists(lab_target_units))) {
    stop(paste("The file", lab_target_units, "cannot be found"))
  }
  dt <- data.table::fread(lab_target_units)
  if (!all(c("concept_id", "unit_target") %in% names(dt))) {
    stop(paste("The file", lab_target_units, "should be a csv file with data model concept_id,unit_target"))
  }
  logger::log_info("[CleanLabValues] LAB_target_units check passed successfully.")
  invisible(dt)
}

#######################################################
# lab_unit_conversion

#' Check LAB_unit_conversion file
#'
#' Validate the unit conversion metadata file and basic consistency with
#' `LAB_target_units`. Returns the parsed `data.table` invisibly.
#'
#' @param lab_unit_conversion Path to the `LAB_unit_conversion` CSV file.
#' @param datasource Optional datasource identifier (string) used to check
#'   for a `datasource` column when provided.
#' @param list_analyses Character vector of `concept_id` values expected.
#' @param target_unit Named character vector mapping `concept_id` -> `unit_target`.
#' @return A `data.table` read from `lab_unit_conversion` (invisibly).
#' @keywords internal
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

  # check that whenever multiplication_factor_from_origin_to_target is missing in at least one row, then the variable conversion_not_multiplication exists, and that there is no row where both variables are missing
  mult_raw <- dt[["multiplication_factor_from_origin_to_target"]]
  mult_chr <- trimws(as.character(mult_raw))
  is_missing_mult <- is.na(mult_raw) | mult_chr == ""
  nummissing_mult <- sum(is_missing_mult)

  mult_num <- suppressWarnings(as.numeric(mult_chr))
  invalid_mult <- !is_missing_mult & is.na(mult_num)
  num_nonnumeric <- sum(invalid_mult)

  if (num_nonnumeric > 0) {
    stop(paste("The file", lab_unit_conversion, "has", num_nonnumeric, "record(s) where the conversion method multiplication_factor_from_origin_to_target contains a non-numeric value."))
  }

  varconv <- "conversion_not_multiplication"
  exists_conv <- varconv %in% names(dt)

  if (exists_conv) {
    conv_raw <- dt[[varconv]]
    conv_chr <- trimws(as.character(conv_raw))
    is_missing_conv <- is.na(conv_raw) | conv_chr == ""
    # count rows where both conversion methods are missing
    both_present_mult_and_conv <- !is_missing_mult & !is_missing_conv
    numboth_present_mult_and_conv <- sum(both_present_mult_and_conv)
    # count rows where both conversion methods are included
    both_missing_mult_and_conv <- is_missing_mult & is_missing_conv
    nummissing_mult_and_conv <- sum(both_missing_mult_and_conv)
  } else {
    # conv_raw <- dt[["conversion_not_multiplication"]]
    # conv_chr <- trimws(as.character(conv_raw))
    # is_missing_conv <- is.na(conv_raw) | conv_chr == ""
  }

  if (nummissing_mult > 0) {
    if (!exists_conv) {
      stop(paste("The file", lab_unit_conversion, "has", nummissing_mult, "record(s) where the concversion method multiplication_factor_from_origin_to_target is missing. This can only happen if there is an alternative conversion method stored in a variable named", varconv, "."))
    } else {
      # Check 1: no row should have both conversion methods missing.


      if (nummissing_mult_and_conv > 0) {
        stop(
          "There are ",
          nummissing_mult_and_conv,
          " row(s) where both multiplication_factor_from_origin_to_target ",
          "and conversion_not_multiplication are missing."
        )
      }

      # Check 2: no row should have both conversion methods non-missing.

      if (numboth_present_mult_and_conv > 0) {
        stop(
          "In the file ", lab_unit_conversion, " there are ",
          numboth_present_mult_and_conv,
          " row(s) where both multiplication_factor_from_origin_to_target ",
          "and conversion_not_multiplication are non-missing. ",
          "Only one conversion method is allowed per row."
        )
      }
    }
  } else {
    # if multiplication_factor_from_origin_to_target does not have missing values, we still need to check that there is no conflicting information on the conversion
    if (exists_conv) {
      if (numboth_present_mult_and_conv > 0) {
        stop(
          "In the file", lab_unit_conversion, "there are ",
          numboth_present_mult_and_conv,
          " row(s) where both multiplication_factor_from_origin_to_target ",
          "and conversion_not_multiplication are non-missing. ",
          "Only one conversion method is allowed per row."
        )
      }
    }
  }

  # If the alternative conversion method exists or the condition_on_value exists, check that it contains valid R expressions using dataset$value.
  for (varconv in c("conversion_not_multiplication", "condition_on_value")) {
    if (varconv %in% names(dt)) {
      conv_raw <- dt[[varconv]]

      if (is.character(conv_raw)) {
        conv_chr <- trimws(conv_raw)
      } else {
        conv_chr <- trimws(as.character(conv_raw))
      }

      is_missing_conv <- is.na(conv_raw) | conv_chr == ""

      has_nonempty_conv <- any(!is_missing_conv)

      if (has_nonempty_conv) {
        if (!is.character(conv_raw)) {
          stop(
            "The column ", varconv, " of the file ", lab_unit_conversion, " must be character."
          )
        }

        rows_with_conv <- which(!is_missing_conv)

        bad_parse_conv <- rep(FALSE, nrow(dt))
        bad_eval_conv <- rep(FALSE, nrow(dt))
        eval_error_conv <- rep(NA_character_, nrow(dt))

        for (i in rows_with_conv) {
          expr_txt <- conv_chr[i]

          parsed_expr <- tryCatch(
            parse(text = expr_txt),
            error = function(e) e
          )

          if (inherits(parsed_expr, "error") || length(parsed_expr) != 1L) {
            bad_parse_conv[i] <- TRUE
            eval_error_conv[i] <- conditionMessage(parsed_expr)
            next
          }

          eval_result <- tryCatch(
            eval(
              parse(text = expr_txt),
              envir = list(value = c(1, 2, NA_real_)),
              enclos = baseenv()
            ),
            error = function(e) e
          )

          if (inherits(eval_result, "error")) {
            bad_eval_conv[i] <- TRUE
            eval_error_conv[i] <- conditionMessage(eval_result)
          }
        }

        numbad_parse_conv <- sum(bad_parse_conv)
        numbad_eval_conv <- sum(bad_eval_conv)

        if (numbad_parse_conv > 0) {
          stop(
            "The column ", varconv, " of the file ", lab_unit_conversion, " contains ",
            numbad_parse_conv,
            " expression(s) that cannot be parsed as valid R expressions."
          )
        }

        if (numbad_eval_conv > 0) {
          stop(
            "The column ", varconv, " of the file ", lab_unit_conversion, " contains ",
            numbad_eval_conv,
            " expression(s) that cannot be evaluated as R expressions involving a numeric variable named value."
          )
        }
      }
    }
  }

  # if the non-mandatory argument 'datasource' is used, check that it corresponds to a variable in lab_unit_conversion
  if (datasource != "" & !("datasource" %in% names(dt))) {
    stop(paste("You specified the argument 'datasource' but in this case the file", lab_unit_conversion, "should include the variable 'datasource' in its data model, while it does not"))
  }
  for (variable in unique(c(list_analyses))) {
    if (nrow(dt[concept_id == variable, .(unit_target)]) > 0) {
      target_unit_in_data <- trimws(unique(dt[concept_id == variable, .(unit_target)]))
      target_unit_in_meta <- target_unit[[variable]]
      if (!is.na(target_unit_in_data) & !is.na(target_unit_in_meta)) {
        if (target_unit_in_data != target_unit_in_meta) {
          stop(paste("In the lab_unit_conversion, the concept_id", variable, "is described with target units that are inconsistent with the target unit assigned in the tab LAB_target_units, which is", target_unit_in_meta))
        }
      }
    }
  }
  logger::log_info("[CleanLabValues] LAB_unit_conversion check passed successfully.")
  invisible(dt)
}

#####################################
# lab_thresholds

#' Check LAB_thresholds file
#'
#' Validate the `LAB_thresholds` CSV file structure and ensure `Min`/`Max`
#' columns are numeric where required.
#'
#' @param lab_thresholds Path to the `LAB_thresholds` CSV file.
#' @param dataset The input dataset (used to validate numeric variables referenced by thresholds).
#' @return A `data.table` read from `lab_thresholds` (invisibly).
#' @keywords internal
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

  for (varname in c("Min", "Max")) {
    vec_ <- dt[[varname]]
    is_num_ <- suppressWarnings(as.numeric(vec_))
    invalid_ <- is.na(is_num_)
    num_nonnumeric <- sum(invalid_)

    if (num_nonnumeric > 0) {
      stop(paste("The file", lab_thresholds, "has", num_nonnumeric, "record(s) where the variable ", varname, " contains a non-numeric value."))
    }
  }



  logger::log_info("[CleanLabValues] LAB_thresholds check passed successfully.")
  invisible(dt)
}
