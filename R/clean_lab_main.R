##' Main cleaning pipeline implementation
#'
#' Internal implementation of the lab cleaning pipeline. Use
#' `CleanLabValuesDataset()` as the user-facing wrapper.
#' @param dataset A `data.frame` or `data.table` with lab measurements.
#' @param list_analyses Character vector of `concept_id` to process (default: all).
#' @param lab_target_units Path to the `LAB_target_units` CSV file.
#' @param lab_unit_conversion Path to the `LAB_unit_conversion` CSV file.
#' @param lab_thresholds Path to the `LAB_thresholds` CSV file.
#' @param datasource Optional datasource identifier (string).
#' @return A `data.table` containing cleaned rows and appended result columns.
#' - `included`: 1/0 whether the value is kept
#' - `value`: cleaned/converted value when `included == 1`, otherwise NA
#' - `unit_target`: the target unit assigned for the concept
#' - `conversion`: integer code indicating conversion origin/type (0/1/2/3)
#' - `rule_applied`: integer code indicating which rule was applied or failure reason
#' @keywords internal
#
clean_lab_main <- function(dataset, list_analyses = c(), lab_target_units, lab_unit_conversion, lab_thresholds, datasource = "") {
  # Ensure input is a data.table and capture original input column order
  dataset <- data.table::as.data.table(dataset)
  input_cols <- names(dataset)
  # add a temporary order id to preserve original row ordering; removed before return
  dataset[, .order_id := seq_len(.N)]

  # Metadata checks are expected to be performed by the top-level wrapper
  # (CleanLabValuesDataset) which calls the `check_*` functions. Avoid
  # duplicating checks here to keep the main pipeline focused on processing.

  # Load metadata
  meta_target_units <- data.table::fread(lab_target_units)
  meta_unit_conv <- data.table::fread(lab_unit_conversion)
  meta_thresholds <- data.table::fread(lab_thresholds)

  requested_datasource <- datasource
  if (requested_datasource != "" && "datasource" %in% names(meta_unit_conv)) {
    meta_unit_conv <- meta_unit_conv[
      is.na(datasource) | trimws(as.character(datasource)) == "" | datasource == requested_datasource
    ]
  }

  # If list_analyses is empty, use all concept_ids from target units
  if (length(list_analyses) == 0) {
    list_analyses <- unique(meta_target_units$concept_id)
  }

  # Assign target_unit for each concept_id
  target_unit <- setNames(meta_target_units$unit_target, meta_target_units$concept_id)

  # Filter dataset to only relevant analyses (keep original columns and __order__)
  dt <- data.table::copy(dataset[concept_id %in% list_analyses])
  # Preserve original value and unit columns for later use
  dt[, value_origin := value]
  dt[, unit_origin := unit]

  # Step 1: Fill missing units
  dt <- fill_missing_unit(dt, meta_unit_conv, target_unit)

  # Prepare result list
  result_list <- list()
  for (cid in unique(dt$concept_id)) {
    # Preserve all columns from the original input for thresholding (e.g., age)
    dt_cid <- dt[concept_id == cid, .SD, .SDcols = names(dt)]
    meta_cid <- meta_unit_conv[concept_id == cid]
    target_unit_cid <- target_unit[[cid]]
    if (nrow(dt_cid) == 0) next
    logger::log_info(paste0("[CleanLabValues] Processing concept_id ", cid, " with ", nrow(dt_cid), " row(s)."))
    # Step 2: Prepare unit_matched (unit_filled if present, else unit)
    dt_cid[, unit_matched := unit_filled]
    dt_cid[is.na(unit_matched) | unit_matched == "", unit_matched := target_unit_cid]
    dt_cid[, unit_target := target_unit_cid]
    # Mark rows with missing original unit
    dt_cid[, unit_missing := is.na(unit_origin) | unit_origin == ""]
    # For missing unit, set unit_matched to assumed_unit_if_missing from metadata (per-row, not just first)
    if ("assumed_unit_if_missing" %in% names(meta_cid)) {
      idx_missing <- which(dt_cid$unit_missing)
      if (length(idx_missing) > 0) {
        for (j in idx_missing) {
          assumed_unit <- meta_cid[unit_target == dt_cid$unit_target[j] & !is.na(assumed_unit_if_missing) & assumed_unit_if_missing != "", assumed_unit_if_missing][1]
          if (!is.na(assumed_unit) && assumed_unit != "") {
            dt_cid$unit_matched[j] <- assumed_unit
          }
        }
      }
    }

    # Step 3: Build conversion metadata as in reference script
    # Always include assumed_unit_if_missing if present
    # Always include assumed_unit_if_missing if present
    # Always include assumed_unit_if_missing if present, and all columns needed for conversion logic
    # Always include assumed_unit_if_missing and unit_matched for missing unit logic
    keep_cols <- unique(c(
      "concept_id", "unit_target", "unit_origin", "multiplication_factor_from_origin_to_target", "next_attempt",
      "assumed_unit_if_missing", "unit_matched", "conversion_not_multiplication", "condition_on_value", "condition_on_variable", "variable"
    ))
    keep_cols <- intersect(keep_cols, names(meta_cid))
    meta_cid_full <- meta_cid[, ..keep_cols]
    # Always ensure direct match row (unit_matched == unit_target, factor 1, next_attempt from existing rows) is present and first
    # Use the first available next_attempt for this concept_id and unit_target, or 0 if none
    direct_next_attempt <- 0
    if (nrow(meta_cid[unit_target == target_unit_cid & !is.na(next_attempt)]) > 0) {
      direct_next_attempt <- meta_cid[unit_target == target_unit_cid & !is.na(next_attempt)][[1, "next_attempt"]]
    }
    direct_row <- data.table::data.table(
      concept_id = cid,
      unit_target = target_unit_cid,
      unit_origin = target_unit_cid,
      multiplication_factor_from_origin_to_target = 1,
      next_attempt = direct_next_attempt
    )
    meta_cid_full <- rbind(direct_row, meta_cid_full, fill = TRUE)
    # Join thresholds on concept_id and unit_target only
    meta_thresholds_cid <- meta_thresholds[concept_id == cid]
    meta_cid_full <- merge(meta_cid_full, meta_thresholds_cid, by = c("concept_id", "unit_target"), all.x = TRUE)
    # For compatibility with mo_convert, rename unit_origin to unit_matched
    setnames(meta_cid_full, "unit_origin", "unit_matched", skip_absent = TRUE)
    # Step 4: mo_convert with full metadata (unit conversion and thresholding)
    dt_cid <- mo_convert(
      dat_unit_matched = dt_cid,
      metadata_convert = meta_cid_full
    )
    dt_cid[, value := data.table::fifelse(included == 1, value_converted, NA_real_)]
    dt_cid[, unit_target := target_unit_cid]

    # Build output preserving all original input columns (in original order),
    # but expose the original measurement as `value_origin`/`unit_origin` (these are created above),
    # then append the cleaning result columns.
    result_cols <- c("included", "value", "unit_target", "conversion", "rule_applied")
    # map input column names to output names: original `value` -> `value_origin`, `unit` -> `unit_origin`
    input_out_cols <- input_cols
    if ("value" %in% input_out_cols) input_out_cols[input_out_cols == "value"] <- "value_origin"
    if ("unit" %in% input_out_cols) input_out_cols[input_out_cols == "unit"] <- "unit_origin"
    out_cols <- c(intersect(input_out_cols, names(dt_cid)), result_cols, ".order_id")
    out <- dt_cid[, ..out_cols]
    result_list[[cid]] <- out
    logger::log_info(paste0("[CleanLabValues] Completed concept_id ", cid, "."))
  }
  result <- data.table::rbindlist(result_list, fill = TRUE)
  # restore original ordering and remove temporary order column (drop explicitly)
  if (".order_id" %in% names(result)) {
    data.table::setorder(result, .order_id)
    result <- result[, setdiff(names(result), ".order_id"), with = FALSE]
  }

  # ensure columns are in a stable, expected order similar to the ground-truth:
  # prefer person_id, concept_id, value_origin, unit_origin, included, value, unit_target, conversion, rule_applied
  desired_order <- c("person_id", "concept_id", "value_origin", "unit_origin", "included", "value", "unit_target", "conversion", "rule_applied")
  final_cols <- c(intersect(desired_order, names(result)), setdiff(names(result), desired_order))
  result <- result[, ..final_cols]

  return(result)
}
