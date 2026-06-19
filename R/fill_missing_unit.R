## Fill missing units in the dataset based on metadata
#' Fill missing unit values based on metadata rules
#'
#' Use `meta_unit_conv` and `target_unit` to populate a `unit_filled`
#' column in `dt`. If an assumed unit is specified in the metadata it is
#' used; otherwise the `target_unit` mapping is applied.
#'
#' @param dt A `data.table` with the measurements. Operates by reference and
#'   returns the modified `data.table`.
#' @param meta_unit_conv A `data.table` providing conversion/assumption rules.
#' @param target_unit A named character vector mapping `concept_id` to `unit_target`.
#' @param concept_id_col Name of the concept id column in `dt` (default: "concept_id").
#' @param unit_col Name of the unit column in `dt` (default: "unit").
#' @return The input `data.table` with a new `unit_filled` column.
#' @keywords internal
fill_missing_unit <- function(dt, meta_unit_conv, target_unit, concept_id_col = "concept_id", unit_col = "unit") {
  dt[, unit_filled := get(unit_col)]
  for (cid in unique(dt[[concept_id_col]])) {
    assumed_unit <- meta_unit_conv[concept_id == cid & unit_origin == "MISSING" & is.na(condition_on_value), assumed_unit_if_missing]
    idx <- which(dt[[concept_id_col]] == cid & (dt[[unit_col]] == "" | is.na(dt[[unit_col]])))
    if (length(idx) == 0) {
      next
    }
    if (length(assumed_unit) > 0 && !is.na(assumed_unit[1])) {
      logger::log_info(paste0("[CleanLabValues] Filling missing units for concept_id ", cid, " using assumed unit '", assumed_unit[1], "' on ", length(idx), " row(s)."))
      dt[idx, unit_filled := assumed_unit[1]]
    } else {
      logger::log_info(paste0("[CleanLabValues] Filling missing units for concept_id ", cid, " using target unit '", target_unit[[cid]], "' on ", length(idx), " row(s)."))
      dt[idx, unit_filled := target_unit[[cid]]]
    }
  }
  dt
}
