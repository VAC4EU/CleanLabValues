# Apply thresholds to the dataset based on metadata
apply_thresholds <- function(dt, meta_thresholds, target_unit) {
  dt[, included := 0]
  for (cid in unique(dt$concept_id)) {
    dt_cid <- dt[concept_id == cid]
    meta_cid <- meta_thresholds[concept_id == cid]
    if (nrow(meta_cid) == 0) next
    # If variable-dependent thresholds
    if (any(!is.na(meta_cid$variable) & meta_cid$variable != "")) {
      for (i in seq_len(nrow(dt_cid))) {
        row <- dt_cid[i]
        row_env <- as.list(row)
        # Find the first threshold row where the condition matches
        threshold_row <- NULL
        for (j in seq_len(nrow(meta_cid))) {
          cond <- meta_cid[j]
          if (is.na(cond$condition_on_variable) || cond$condition_on_variable == "" ||
            eval(parse(text = cond$condition_on_variable), envir = row_env)) {
            threshold_row <- cond
            break
          }
        }
        # Find the row index in dt for this row
        idx <- dt[concept_id == cid][i, which = TRUE]
        if (!is.null(threshold_row)) {
          minv <- as.numeric(threshold_row$Min)
          maxv <- as.numeric(threshold_row$Max)
          val <- row$value
          if (!is.na(val) && val >= minv && val <= maxv) {
            dt[idx, included := 1]
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
