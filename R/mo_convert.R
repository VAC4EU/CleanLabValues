# Convert lab values to target units and flag inclusion, using data.table
mo_convert <- function(dat_unit_matched, metadata_convert) {
  # Step 1: Join multiplication factor
  meta_mult <- unique(metadata_convert[, .(unit_matched, multiplication_factor_from_origin_to_target, unit_target)])
  dat <- merge(dat_unit_matched, meta_mult, by = "unit_matched", all.x = TRUE)
  dat[, value_converted := value * as.numeric(multiplication_factor_from_origin_to_target)]
  dat[is.na(value_converted), value_converted := value]
  dat[, unit_matched := ifelse(value == value_converted | is.na(value), unit_matched, unit_target)]
  dat[, c("multiplication_factor_from_origin_to_target", "unit_target") := NULL]

  # Step 2: Join outlier/threshold metadata (first attempt)
  meta_outlier <- metadata_convert[next_attempt %in% c(0, 1, 99)]
  # If Min/Max not present, add as NA
  if (!"Min" %in% names(meta_outlier)) meta_outlier[, Min := NA]
  if (!"Max" %in% names(meta_outlier)) meta_outlier[, Max := NA]
  dat <- merge(dat, meta_outlier, by = "unit_matched", all.x = TRUE, suffixes = c("", ".meta"))
  dat[, value_in_range := value_converted >= as.numeric(Min) & value_converted <= as.numeric(Max)]
  dat[, next_attempt := as.numeric(next_attempt)]

  # Step 3: Handle next_attempt logic (simplified for data.table)
  if (any(dat$next_attempt == 99, na.rm = TRUE)) {
    dat[!value_in_range & next_attempt == 99, value_converted := value]
    dat[!value_in_range & next_attempt == 99, value_in_range := value_converted >= as.numeric(Min) & value_converted <= as.numeric(Max)]
  }
  if (any(dat$next_attempt == 1, na.rm = TRUE)) {
    # For next_attempt == 1, try a second conversion if out of range
    meta_second <- metadata_convert[next_attempt == 2]
    if (nrow(meta_second) > 0) {
      dat[!value_in_range & next_attempt == 1, unit_matched := meta_second$unit_matched[1]]
      dat[!value_in_range & next_attempt == 1, value_converted := value_converted * as.numeric(meta_second$multiplication_factor_from_origin_to_target[1])]
      dat[!value_in_range & next_attempt == 1, value_in_range := value_converted >= as.numeric(meta_second$Min[1]) & value_converted <= as.numeric(meta_second$Max[1])]
      dat[!value_in_range & next_attempt == 1, next_attempt := 2]
    }
  }
  return(dat)
}
