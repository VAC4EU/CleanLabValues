# Fill missing units in the dataset based on metadata
fill_missing_unit <- function(dt, meta_unit_conv, target_unit, concept_id_col = "concept_id", unit_col = "unit") {
  dt[, unit_filled := get(unit_col)]
  for (cid in unique(dt[[concept_id_col]])) {
    assumed_unit <- meta_unit_conv[concept_id == cid & unit_origin == "MISSING" & is.na(condition_on_value), assumed_unit_if_missing]
    idx <- which(dt[[concept_id_col]] == cid & (dt[[unit_col]] == "" | is.na(dt[[unit_col]])))
    if (length(assumed_unit) > 0 && !is.na(assumed_unit[1])) {
      dt[idx, unit_filled := assumed_unit[1]]
    } else {
      dt[idx, unit_filled := target_unit[[cid]]]
    }
  }
  dt
}
