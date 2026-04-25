# Post-processing: ensure rule_applied=1 for included==1 and conversion==3 (assumed unit for missing)
# Place this after all row logic so it is not overwritten
# Convert lab values to target units and flag inclusion, using data.table

# Refactored mo_convert to incrementally try next_attempt (1, 2, ...) for each concept_id/unit_target, discarding if max next_attempt is reached
mo_convert <- function(dat_unit_matched, metadata_convert) {
  # Clean, deterministic implementation of the conversion logic described in README.
  # For each row: try direct unit matches, then follow next_attempt order for missing units
  # and for fallback conversions. Preserve rule codes and conversion semantics.
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table required")
  dat <- copy(dat_unit_matched)
  meta <- copy(metadata_convert)

  # Ensure key columns exist
  if (!"assumed_unit_if_missing" %in% names(meta)) meta[, assumed_unit_if_missing := NA_character_]
  if (!"unit_matched" %in% names(meta)) meta[, unit_matched := NA_character_]
  if (!"next_attempt" %in% names(meta)) meta[, next_attempt := NA_integer_]
  if (!"Min" %in% names(meta)) meta[, Min := NA_real_]
  if (!"Max" %in% names(meta)) meta[, Max := NA_real_]

  # Normalize helper
  norm <- function(x) tolower(trimws(as.character(x)))

  dat[, `:=`(
    included = as.integer(NA),
    value_converted = as.numeric(NA),
    conversion = as.integer(NA),
    rule_applied = as.integer(NA),
    n_conversion_attempts = 0L
  )]

  get_var_value <- function(row, varname) {
    if (is.null(varname) || is.na(varname) || varname == "") {
      return(NA)
    }
    if (varname %in% names(dat)) {
      return(dat[[varname]][row])
    }
    NA
  }

  for (i in seq_len(nrow(dat))) {
    row <- dat[i]
    cid <- row$concept_id
    target <- row$unit_target
    val_raw <- suppressWarnings(as.numeric(row$value))
    unit_origin <- ifelse(is.na(row$unit_origin) || row$unit_origin == "", NA_character_, norm(row$unit_origin))
    unit_missing_flag <- if ("unit_missing" %in% names(dat)) isTRUE(row$unit_missing) else is.na(unit_origin)

    attempts <- meta[concept_id == cid & unit_target == target]
    if (nrow(attempts) > 0) attempts[, unit_matched := norm(unit_matched)]

    # Treat OTHER: if origin unit present and not in attempts -> include as-is, conversion=2
    if (!is.na(unit_origin) && nrow(attempts) > 0 && !(unit_origin %in% attempts$unit_matched)) {
      dat[i, `:=`(included = 1L, value_converted = val_raw, conversion = 2L, rule_applied = 0L)]
      next
    }

    # Helper to evaluate a single attempt row
    eval_attempt <- function(attempt_row, assumed_unit = NA_character_) {
      # Returns list(success=TRUE/FALSE, val_conv=?, attempted=TRUE/FALSE)
      factor <- suppressWarnings(as.numeric(attempt_row$multiplication_factor_from_origin_to_target))
      if (is.na(factor) || is.na(val_raw)) {
        return(list(success = FALSE, attempted = FALSE))
      }
      val_conv <- val_raw * factor
      minv <- suppressWarnings(as.numeric(attempt_row$Min))
      maxv <- suppressWarnings(as.numeric(attempt_row$Max))
      # Evaluate condition_on_value first (explicit value-based conditions)
      cond_val_expr <- attempt_row$condition_on_value
      if (!is.null(cond_val_expr) && !is.na(cond_val_expr) && cond_val_expr != "") {
        expr <- gsub("value", as.character(val_raw), cond_val_expr)
        cond_ok <- tryCatch(isTRUE(eval(parse(text = expr))), error = function(e) FALSE)
        # If condition references value and is FALSE, this attempt is not applicable
        if (!isTRUE(cond_ok)) return(list(success = FALSE, attempted = FALSE))
      } else {
        # Otherwise consider condition_on_variable if present
        cond_var <- attempt_row$condition_on_variable
        if (!is.null(cond_var) && !is.na(cond_var) && cond_var != "") {
          varname <- if (!is.null(attempt_row$variable) && !is.na(attempt_row$variable) && attempt_row$variable != "") attempt_row$variable else NULL
          cond_val <- get_var_value(i, varname)
          if (is.na(cond_val)) {
            # cannot evaluate variable-based condition -> treat as not applicable
            return(list(success = FALSE, attempted = FALSE))
          }
          expr <- gsub(varname, as.character(cond_val), cond_var)
          cond_ok <- tryCatch(isTRUE(eval(parse(text = expr))), error = function(e) FALSE)
          if (!isTRUE(cond_ok)) return(list(success = FALSE, attempted = FALSE))
        }
      }
      # If we reach here, the attempt is applicable (attempted)
      attempted <- TRUE
      ok <- !is.na(minv) && !is.na(maxv) && !is.na(val_conv) && val_conv >= minv && val_conv <= maxv
      if (ok) {
        return(list(success = TRUE, val_conv = val_conv, attempted = attempted))
      }
      list(success = FALSE, attempted = attempted)
    }

    # 1) Try direct matches (unit_origin equals unit_matched)
    if (nrow(attempts) > 0 && !is.na(unit_origin)) {
      direct <- attempts[unit_matched == unit_origin]
      if (nrow(direct) > 0) {
        # try each direct attempt until one succeeds
        for (r in seq_len(nrow(direct))) {
          dat[i, n_conversion_attempts := n_conversion_attempts + 1L]
          res <- eval_attempt(direct[r])
          if (isTRUE(res$success)) {
            dat[i, `:=`(
              included = 1L,
              value_converted = res$val_conv,
              conversion = ifelse(is.na(unit_origin) || unit_origin == target, 0L, ifelse(unit_origin == target, 0L, 1L)),
              rule_applied = ifelse(is.na(unit_origin) || unit_origin == target, 0L, 1L)
            )]
            break
          }
        }
        if (!is.na(dat$included[i]) && dat$included[i] == 1L) next
      }
    }

    # If unit is missing but unit_matched was pre-filled (from fill_missing_unit),
    # try the prefilled assumed unit first (count as one attempt), then fall back to other missing attempts.
    missing_attempts_tried <- 0L
    if (unit_missing_flag) {
      row_unit_matched <- NA_character_
      if ("unit_matched" %in% names(dat)) row_unit_matched <- ifelse(is.na(dat$unit_matched[i]) || dat$unit_matched[i] == "", NA_character_, norm(dat$unit_matched[i]))
      if (!is.na(row_unit_matched) && nrow(attempts) > 0) {
        # only consider assumed_unit_if_missing coming from explicit MISSING rows
        # and only those without a condition_on_value (these are used by fill_missing_unit)
        assumed_list <- unique(na.omit(tolower(trimws(as.character(attempts[unit_matched == 'missing' & (is.na(condition_on_value) | condition_on_value == '')]$assumed_unit_if_missing)))))
        # Only treat prefilled unit_matched as an assumed-unit attempt if it matches an assumed unit from metadata
        if (row_unit_matched %in% assumed_list) {
          assumed_rows <- attempts[unit_matched == row_unit_matched]
        } else {
          assumed_rows <- data.table::data.table()
        }
        if (nrow(assumed_rows) > 0) {
          # try the prefilled assumed unit first
          dat[i, n_conversion_attempts := n_conversion_attempts + 1L]
          missing_attempts_tried <- missing_attempts_tried + 1L
          res <- eval_attempt(assumed_rows[1])
          if (isTRUE(res$success)) {
            factor_try <- suppressWarnings(as.numeric(assumed_rows[1]$multiplication_factor_from_origin_to_target))
            if (!is.na(factor_try) && factor_try == 1) {
              rp <- 0L
            } else {
              rp <- if (!is.na(assumed_rows[1]$next_attempt) && assumed_rows[1]$next_attempt > 0) as.integer(assumed_rows[1]$next_attempt) else 1L
            }
            dat[i, `:=`(included = 1L, value_converted = res$val_conv, conversion = 3L, rule_applied = rp)]
            next
          }
          # if that prefilled assumed unit failed, count as 1 attempt and then continue to try other missing attempts
        }
      }
    }

    # 2) If unit missing (or flagged missing), try attempts ordered by next_attempt (for assumed units)
    if (unit_missing_flag && nrow(attempts) > 0) {
      # consider explicit next_attempt chain (>0) first, then any rows that encode MISSING regardless of next_attempt
      miss_attempts <- attempts[!is.na(next_attempt) & next_attempt > 0][order(next_attempt)]
      if (nrow(miss_attempts) > 0) {
        for (r in seq_len(nrow(miss_attempts))) {
          attempt_row <- miss_attempts[r]
          # choose assumed unit if provided, else use unit_matched
          assumed_unit <- ifelse(!is.na(attempt_row$assumed_unit_if_missing) && attempt_row$assumed_unit_if_missing != "", attempt_row$assumed_unit_if_missing, attempt_row$unit_matched)
          res <- eval_attempt(attempt_row)
          if (isTRUE(res$attempted)) {
            dat[i, n_conversion_attempts := n_conversion_attempts + 1L]
            missing_attempts_tried <- missing_attempts_tried + 1L
          }
          if (isTRUE(res$success)) {
            factor_try <- suppressWarnings(as.numeric(attempt_row$multiplication_factor_from_origin_to_target))
            if (!is.na(factor_try) && factor_try == 1) {
              rp <- 0L
            } else {
              rp <- if (!is.na(attempt_row$next_attempt) && attempt_row$next_attempt > 0) as.integer(attempt_row$next_attempt) else 1L
            }
            dat[i, `:=`(included = 1L, value_converted = res$val_conv, conversion = 3L, rule_applied = rp)]
            break
          }
        }
        if (!is.na(dat$included[i]) && dat$included[i] == 1L) next
      }
      # Next, try any attempts explicitly labelled as MISSING (unit_matched == "missing")
      missing_rows <- attempts[unit_matched == "missing"]
      if (nrow(missing_rows) > 0) {
        for (r in seq_len(nrow(missing_rows))) {
          attempt_row <- missing_rows[r]
          res <- eval_attempt(attempt_row)
          if (isTRUE(res$attempted)) {
            dat[i, n_conversion_attempts := n_conversion_attempts + 1L]
            missing_attempts_tried <- missing_attempts_tried + 1L
          }
          if (isTRUE(res$success)) {
            factor_try <- suppressWarnings(as.numeric(attempt_row$multiplication_factor_from_origin_to_target))
            if (!is.na(factor_try) && factor_try == 1) {
              rp <- 0L
            } else {
              rp <- if (!is.na(attempt_row$next_attempt) && attempt_row$next_attempt > 0) as.integer(attempt_row$next_attempt) else 1L
            }
            dat[i, `:=`(included = 1L, value_converted = res$val_conv, conversion = 3L, rule_applied = rp)]
            break
          }
        }
        if (!is.na(dat$included[i]) && dat$included[i] == 1L) next
      }
    }

    # 3) Fallback: try all attempts ordered by next_attempt (1,2,...) as fallbacks for conversion
    if (nrow(attempts) > 0) {
      # fallback chain only considers positive next_attempt codes
      fallback_order <- unique(attempts$next_attempt[!is.na(attempts$next_attempt) & attempts$next_attempt > 0])
      fallback_order <- sort(fallback_order)
      attempts_made <- 0L
      for (code in fallback_order) {
        rows_idx <- which(attempts$next_attempt == code)
        for (j in rows_idx) {
          dat[i, n_conversion_attempts := n_conversion_attempts + 1L]
          attempt_row <- attempts[j]
          res <- eval_attempt(attempt_row)
          attempts_made <- attempts_made + 1L
          if (isTRUE(res$success)) {
            dat[i, `:=`(included = 1L, value_converted = res$val_conv, conversion = 1L, rule_applied = ifelse(attempts_made == 1L, 1L, 2L))]
            break
          }
        }
        if (!is.na(dat$included[i]) && dat$included[i] == 1L) break
      }
      if (!is.na(dat$included[i]) && dat$included[i] == 1L) next
    }

    # 4) If we get here, conversion failed or no applicable attempts
    # Decide conversion & rule codes per README semantics
    if (is.na(val_raw) || !is.numeric(val_raw)) {
      dat[i, `:=`(included = 0L, value_converted = NA_real_, conversion = 3L, rule_applied = 99L)]
    } else if (unit_missing_flag) {
      # Use only the missing-unit attempts count to decide 90/91/92
      attempts_made_final <- missing_attempts_tried
      rp_fail <- ifelse(is.na(attempts_made_final) || attempts_made_final == 0L, 90L, ifelse(attempts_made_final == 1L, 91L, 92L))
      dat[i, `:=`(included = 0L, value_converted = NA_real_, conversion = 3L, rule_applied = rp_fail)]
    } else if (!is.na(unit_origin) && nrow(attempts) == 0) {
      # unit present but no conversion metadata -> treat as OTHER
      dat[i, `:=`(included = 1L, value_converted = val_raw, conversion = 2L, rule_applied = 0L)]
    } else if (!is.na(unit_origin) && unit_origin != target) {
      # Tried conversions but none accepted
      # Use actual number of attempts made on this row to determine rule_applied (91 if one try, 92 if multiple)
      attempts_made <- dat$n_conversion_attempts[i]
      dat[i, `:=`(included = 0L, value_converted = NA_real_, conversion = 1L, rule_applied = ifelse(attempts_made <= 1L, 91L, 92L))]
    } else {
      dat[i, `:=`(included = 0L, value_converted = NA_real_, conversion = 0L, rule_applied = 90L)]
    }
  }

  # Clean helper columns
  dat[, n_conversion_attempts := NULL]
  return(dat)
}
