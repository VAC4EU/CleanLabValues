# Main cleaning function for lab values
clean_lab_main <- function(dataset, list_analyses = c(), lab_target_units, lab_unit_conversion, lab_thresholds, datasource = "") {
  check_dataset_model(dataset)
  check_lab_target_units(lab_target_units)
  check_lab_unit_conversion(lab_unit_conversion, datasource, list_analyses, list())
  check_lab_thresholds(lab_thresholds, dataset)

  # Load metadata
  meta_target_units <- data.table::fread(lab_target_units)
  meta_unit_conv <- data.table::fread(lab_unit_conversion)
  meta_thresholds <- data.table::fread(lab_thresholds)

  # If list_analyses is empty, use all concept_ids from target units
  if (length(list_analyses) == 0) {
    list_analyses <- unique(meta_target_units$concept_id)
  }

  # Assign target_unit for each concept_id
  target_unit <- setNames(meta_target_units$unit_target, meta_target_units$concept_id)

  # Filter dataset to only relevant analyses
  dt <- data.table::copy(dataset[concept_id %in% list_analyses])
  # Preserve original value and unit columns for later use
  dt[, value_origin := value]
  dt[, unit_origin := unit]

  # Step 1: Fill missing units
  dt <- fill_missing_unit(dt, meta_unit_conv, target_unit)

  # Prepare result list
  result_list <- list()
  for (cid in unique(dt$concept_id)) {
    dt_cid <- dt[concept_id == cid]
    meta_cid <- meta_unit_conv[concept_id == cid]
    target_unit_cid <- target_unit[[cid]]
    # If no metadata, skip
    if (nrow(dt_cid) == 0) next
    # Step 2: Prepare unit_matched (unit_filled if present, else unit)
    dt_cid[, unit_matched := unit_filled]
    # Step 3: Prepare metadata for mo_convert
    meta_cid <- data.table::copy(meta_cid)
    # Always ensure unit_matched column exists in meta_cid
    if (!"unit_matched" %in% names(meta_cid)) {
      meta_cid[, unit_matched := unit_origin]
    }
    if (!is.null(target_unit_cid) && !any(meta_cid$unit_target == target_unit_cid & meta_cid$unit_matched == target_unit_cid)) {
      # Add identity conversion row if missing
      meta_cid <- rbind(meta_cid, data.table::data.table(
        concept_id = cid,
        datasource = NA_character_,
        unit_origin = target_unit_cid,
        unit_target = target_unit_cid,
        multiplication_factor_from_origin_to_target = 1,
        conversion_rate = 1,
        condition_on_value = NA,
        assumed_unit_if_missing = NA,
        next_attempt = 0,
        unit_matched = target_unit_cid
      ), fill = TRUE)
    }
    # Step 4: mo_convert
    dt_cid <- mo_convert(dt_cid, meta_cid)
    # Prepare output columns needed for conversion logic
    dt_cid[, value_origin := value]
    dt_cid[, unit_origin := unit]
    # Step 5: Apply thresholds (after conversion)
    meta_thresholds_cid <- meta_thresholds[concept_id == cid]
    dt_cid <- apply_thresholds(dt_cid, meta_thresholds_cid, target_unit)
    # conversion: 0 = no conversion, 1 = from nonmissing unit, 2 = from OTHER, 3 = from MISSING
    unit_pool <- unique(na.omit(meta_cid$unit_origin))
    # Use unit_origin for conversion logic, handle NA/empty as in original
    dt_cid[, conversion := fifelse(
      (!is.na(unit_origin) & unit_origin == target_unit_cid) |
        (is.na(unit_matched) & (is.na(target_unit_cid) | target_unit_cid == "NA") & !is.na(value_origin)),
      0,
      fifelse(
        !is.na(unit_origin) & unit_origin != "" & !is.na(unit_matched) & unit_origin != target_unit_cid & unit_origin %in% unit_pool,
        1,
        fifelse(
          is.na(unit_origin) | unit_origin == "",
          3,
          fifelse(
            !is.na(unit_origin) & !is.na(unit_matched) & unit_origin != target_unit_cid & !unit_origin %in% unit_pool,
            2,
            3
          )
        )
      )
    )]
    # included: set by apply_thresholds only, do not overwrite here
    # value: value_converted if included==1, else NA
    dt_cid[, value_final := fifelse(included == 1, value_converted, NA_real_)]
    # rule_applied logic
    dt_cid[, rule_applied := fifelse(
      conversion == 0 & included == 1, 0,
      fifelse(
        conversion == 0 & included == 0, 90,
        fifelse(
          conversion == 3 & included == 0, 90,
          fifelse(
            conversion > 0 & next_attempt %in% c(0, 1, 99) & included == 1, 1,
            fifelse(
              conversion > 0 & next_attempt %in% c(0, 1, 99) & included == 0, 91,
              fifelse(
                conversion > 0 & next_attempt == 2 & included == 1, 2,
                fifelse(
                  conversion > 0 & next_attempt == 2 & included == 0, 92,
                  fifelse(included == 0 & is.na(value), 99, NA_integer_)
                )
              )
            )
          )
        )
      )
    )]
    # Prepare output columns
    dt_cid[, value_origin := value]
    dt_cid[, unit_origin := unit]
    dt_cid[, unit_target := target_unit_cid]
    dt_cid[, value := round(value_final, 2)]
    # Select and order columns, include 'date' if present
    output_cols <- c("person_id", "concept_id", "value_origin", "unit_origin", "included", "value", "unit_target", "conversion", "rule_applied")
    if ("date" %in% names(dt_cid)) {
      output_cols <- c(output_cols, "date")
    }
    out <- dt_cid[, ..output_cols]
    result_list[[cid]] <- out
  }
  result <- data.table::rbindlist(result_list, fill = TRUE)
  return(result)
}
