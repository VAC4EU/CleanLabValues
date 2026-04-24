# Convert units in the dataset based on metadata (stub)
convert_unit <- function(dt, meta_unit_conv, target_unit) {
  # Use unit_filled as unit_matched for conversion
  dt[, unit_matched := unit_filled]
  out <- list()
  for (cid in unique(dt$concept_id)) {
    dt_cid <- dt[concept_id == cid]
    meta_cid <- meta_unit_conv[concept_id == cid]
    if (nrow(dt_cid) > 0 && nrow(meta_cid) > 0) {
      out[[cid]] <- mo_convert(dt_cid, meta_cid)
    } else {
      out[[cid]] <- dt_cid
    }
  }
  data.table::rbindlist(out, fill = TRUE)
}
