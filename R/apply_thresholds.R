# Apply thresholds to the dataset based on metadata
apply_thresholds <- function(dt, meta_thresholds, target_unit) {
  dt[, included := 1]
  for (cid in unique(dt$concept_id)) {
    dt_cid <- dt[concept_id == cid]
    meta_cid <- meta_thresholds[concept_id == cid]
    if (nrow(meta_cid) == 0) next
    # If variable-dependent thresholds
    if (any(!is.na(meta_cid$variable) & meta_cid$variable != "")) {
      vars <- unique(na.omit(meta_cid$variable))
      for (v in vars) {
        conds <- meta_cid[variable == v]
        for (i in seq_len(nrow(conds))) {
          cond <- conds[i]
          # Evaluate condition_on_variable for each row
          idx <- which(eval(parse(text = cond$condition_on_variable), envir = dt_cid))
          if (length(idx) > 0) {
            dt[concept_id == cid][
              idx,
              included := as.numeric(
                value_converted >= as.numeric(cond$Min) & value_converted <= as.numeric(cond$Max)
              )
            ]
          }
        }
      }
    } else {
      # Simple Min/Max threshold
      minv <- as.numeric(meta_cid$Min[1])
      maxv <- as.numeric(meta_cid$Max[1])
      dt[
        concept_id == cid,
        included := as.numeric(value_converted >= minv & value_converted <= maxv)
      ]
    }
  }
  dt
}
