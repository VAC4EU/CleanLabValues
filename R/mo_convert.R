#' Unit conversion helpers
#'
#' Internal helpers and the `mo_convert()` implementation used to convert
#' lab values to target units and compute inclusion/conversion codes.
#'
#' These functions are internal to the package and are not exported.
#'
#' @keywords internal
NULL
.mo_norm <- function(x) tolower(trimws(as.character(x)))

.mo_get_var_value <- function(dat, row_idx, varname) {
  if (is.null(varname) || is.na(varname) || varname == "") {
    return(NA)
  }
  if (varname %in% names(dat)) {
    return(dat[[varname]][row_idx])
  }
  NA
}

.mo_parse_optional_expr <- function(expr_text, default_expr = quote(NA_real_)) {
  if (is.null(expr_text) || is.na(expr_text) || expr_text == "") {
    return(default_expr)
  }
  parse(text = expr_text)[[1]]
}

.mo_prepare_attempt_bundle <- function(attempts) {
  n_attempts <- nrow(attempts)
  if (n_attempts == 0) {
    return(list(
      n = 0L,
      unit_matched = character(0),
      next_attempt = integer(0),
      factor_num = numeric(0),
      expr_parsed = list(),
      minv = numeric(0),
      maxv = numeric(0),
      condition_on_value = character(0),
      cond_value_parsed = list(),
      condition_on_variable = character(0),
      cond_var_parsed = list(),
      variable = character(0),
      assumed_unit_if_missing = character(0)
    ))
  }

  list(
    n = n_attempts,
    unit_matched = attempts$unit_matched,
    next_attempt = attempts$next_attempt,
    factor_num = suppressWarnings(as.numeric(attempts$multiplication_factor_from_origin_to_target)),
    expr_parsed = lapply(attempts$conversion_not_multiplication, .mo_parse_optional_expr),
    minv = suppressWarnings(as.numeric(attempts$Min)),
    maxv = suppressWarnings(as.numeric(attempts$Max)),
    condition_on_value = attempts$condition_on_value,
    cond_value_parsed = lapply(attempts$condition_on_value, .mo_parse_optional_expr, default_expr = quote(TRUE)),
    condition_on_variable = attempts$condition_on_variable,
    cond_var_parsed = lapply(attempts$condition_on_variable, .mo_parse_optional_expr, default_expr = quote(TRUE)),
    variable = if ("variable" %in% names(attempts)) as.character(attempts$variable) else rep(NA_character_, n_attempts),
    assumed_unit_if_missing = if ("assumed_unit_if_missing" %in% names(attempts)) as.character(attempts$assumed_unit_if_missing) else rep(NA_character_, n_attempts)
  )
}

.mo_eval_vectorized_values <- function(values, meta_rows, meta_pos = NULL) {
  out <- rep(NA_real_, length(values))
  factor_num <- meta_rows$factor_num
  idx_factor <- which(!is.na(factor_num))
  if (length(idx_factor) > 0) {
    out[idx_factor] <- values[idx_factor] * factor_num[idx_factor]
  }
  idx_expr <- which(is.na(factor_num))
  if (length(idx_expr) > 0) {
    if (is.null(meta_pos)) {
      meta_pos <- seq_len(nrow(meta_rows))
    }
    expr_groups <- split(idx_expr, meta_pos[idx_expr])
    for (group_idx in expr_groups) {
      expr_parsed <- meta_rows$expr_parsed[[group_idx[1]]]
      out[group_idx] <- eval(expr_parsed, envir = list(value = values[group_idx]))
    }
  }
  out
}

.mo_eval_attempt <- function(attempt_bundle, attempt_idx, val_raw, dat, row_idx) {
  # Returns list(success=TRUE/FALSE, val_conv=?, attempted=TRUE/FALSE)
  factor <- attempt_bundle$factor_num[attempt_idx]
  if (!is.na(factor)) {
    val_conv <- val_raw * factor
  } else {
    val_conv <- eval(attempt_bundle$expr_parsed[[attempt_idx]], envir = list(value = val_raw))
  }
  minv <- attempt_bundle$minv[attempt_idx]
  maxv <- attempt_bundle$maxv[attempt_idx]
  cond_val_expr <- attempt_bundle$condition_on_value[attempt_idx]
  if (!is.null(cond_val_expr) && !is.na(cond_val_expr) && cond_val_expr != "") {
    cond_ok <- tryCatch(isTRUE(eval(attempt_bundle$cond_value_parsed[[attempt_idx]], envir = list(value = val_raw))), error = function(e) FALSE)
    if (!isTRUE(cond_ok)) {
      return(list(success = FALSE, attempted = FALSE))
    }
  } else {
    cond_var <- attempt_bundle$condition_on_variable[attempt_idx]
    if (!is.null(cond_var) && !is.na(cond_var) && cond_var != "") {
      varname <- attempt_bundle$variable[attempt_idx]
      if (!is.null(varname) && !is.na(varname) && varname == "") varname <- NULL
      cond_val <- .mo_get_var_value(dat, row_idx, varname)
      if (is.na(cond_val)) {
        return(list(success = FALSE, attempted = FALSE))
      }
      cond_env <- setNames(list(cond_val), varname)
      cond_ok <- tryCatch(isTRUE(eval(attempt_bundle$cond_var_parsed[[attempt_idx]], envir = cond_env)), error = function(e) FALSE)
      if (!isTRUE(cond_ok)) {
        return(list(success = FALSE, attempted = FALSE))
      }
    }
  }
  attempted <- TRUE
  ok <- !is.na(minv) && !is.na(maxv) && !is.na(val_conv) && val_conv >= minv && val_conv <= maxv
  if (ok) {
    return(list(success = TRUE, val_conv = val_conv, attempted = attempted))
  }
  list(success = FALSE, attempted = attempted)
}

# Small, top-level helpers to keep `mo_convert()` concise. They operate by
# modifying `dat` by reference (using `i`) and return success flags and
# attempt counts where appropriate.
.mo_try_direct_matches <- function(dat, i, attempt_bundle, attempt_unit_matched, unit_origin, target, val_raw) {
  if (attempt_bundle$n > 0 && !is.na(unit_origin)) {
    direct_idx <- which(attempt_unit_matched == unit_origin)
    if (length(direct_idx) > 0) {
      for (r in direct_idx) {
        dat[i, n_conversion_attempts := n_conversion_attempts + 1L]
        res <- .mo_eval_attempt(attempt_bundle, r, val_raw, dat, i)
        if (isTRUE(res$success)) {
          dat[i, `:=`(
            included = 1L,
            value_converted = res$val_conv,
            conversion = ifelse(is.na(unit_origin) || unit_origin == target, 0L, ifelse(unit_origin == target, 0L, 1L)),
            rule_applied = ifelse(is.na(unit_origin) || unit_origin == target, 0L, 1L)
          )]
          return(TRUE)
        }
      }
    }
  }
  FALSE
}

.mo_try_prefilled_assumed <- function(dat, i, attempt_bundle, attempt_unit_matched, row_unit_matched, val_raw, conv_success = 3L, other_flow = FALSE) {
  # Returns list(success=TRUE/FALSE, tried=0/1)
  if (is.na(row_unit_matched) || attempt_bundle$n == 0) {
    return(list(success = FALSE, tried = 0L))
  }
  assumed_idx <- which(attempt_unit_matched == "missing" & (is.na(attempt_bundle$condition_on_value) | attempt_bundle$condition_on_value == ""))
  assumed_list <- unique(na.omit(tolower(trimws(as.character(attempt_bundle$assumed_unit_if_missing[assumed_idx])))))
  if (!(row_unit_matched %in% assumed_list)) {
    return(list(success = FALSE, tried = 0L))
  }
  assumed_rows_idx <- which(attempt_unit_matched == row_unit_matched)
  if (length(assumed_rows_idx) == 0) {
    return(list(success = FALSE, tried = 0L))
  }
  dat[i, n_conversion_attempts := n_conversion_attempts + 1L]
  assumed_idx_first <- assumed_rows_idx[1]
  res <- .mo_eval_attempt(attempt_bundle, assumed_idx_first, val_raw, dat, i)
  if (isTRUE(res$success)) {
    factor_try <- attempt_bundle$factor_num[assumed_idx_first]
    if (!is.na(factor_try) && factor_try == 1) {
      rp <- 0L
    } else {
      rp <- if (!is.na(attempt_bundle$next_attempt[assumed_idx_first]) && attempt_bundle$next_attempt[assumed_idx_first] > 0) as.integer(attempt_bundle$next_attempt[assumed_idx_first]) else 1L
    }
    conv_code <- conv_success
    if (!is.na(factor_try) && factor_try == 1 && conv_success == 1L && isTRUE(other_flow)) conv_code <- 2L
    dat[i, `:=`(included = 1L, value_converted = res$val_conv, conversion = conv_code, rule_applied = rp)]
    return(list(success = TRUE, tried = 1L))
  }
  list(success = FALSE, tried = 1L)
}

.mo_try_missing_chain <- function(dat, i, attempt_bundle, attempt_unit_matched, val_raw, conv_success = 3L, other_flow = FALSE, skip_assumed_unit = NA_character_) {
  # Returns list(success=TRUE/FALSE, tried=number_of_attempts_tried)
  tried <- 0L
  # explicit next_attempt chain (>0)
  miss_attempts_idx <- which(!is.na(attempt_bundle$next_attempt) & attempt_bundle$next_attempt > 0)
  if (length(miss_attempts_idx) > 0) {
    miss_attempts_idx <- miss_attempts_idx[order(attempt_bundle$next_attempt[miss_attempts_idx])]
    for (r in miss_attempts_idx) {
      res <- .mo_eval_attempt(attempt_bundle, r, val_raw, dat, i)
      if (isTRUE(res$attempted)) {
        dat[i, n_conversion_attempts := n_conversion_attempts + 1L]
        tried <- tried + 1L
      }
      if (isTRUE(res$success)) {
        factor_try <- attempt_bundle$factor_num[r]
        if (!is.na(factor_try) && factor_try == 1) {
          rp <- 0L
        } else {
          rp <- if (!is.na(attempt_bundle$next_attempt[r]) && attempt_bundle$next_attempt[r] > 0) as.integer(attempt_bundle$next_attempt[r]) else 1L
        }
        conv_code <- conv_success
        if (!is.na(factor_try) && factor_try == 1 && conv_success == 1L && isTRUE(other_flow)) conv_code <- 2L
        dat[i, `:=`(included = 1L, value_converted = res$val_conv, conversion = conv_code, rule_applied = rp)]
        return(list(success = TRUE, tried = tried))
      }
    }
  }
  # rows labelled as missing
  missing_idx <- which(attempt_unit_matched == "missing")
  if (!is.na(skip_assumed_unit) && length(missing_idx) > 0) {
    missing_idx <- missing_idx[
      is.na(attempt_bundle$next_attempt[missing_idx]) |
        attempt_bundle$next_attempt[missing_idx] != 0 |
        .mo_norm(attempt_bundle$assumed_unit_if_missing[missing_idx]) != skip_assumed_unit
    ]
  }
  if (length(missing_idx) > 0) {
    for (r in missing_idx) {
      res <- .mo_eval_attempt(attempt_bundle, r, val_raw, dat, i)
      if (isTRUE(res$attempted)) {
        dat[i, n_conversion_attempts := n_conversion_attempts + 1L]
        tried <- tried + 1L
      }
      if (isTRUE(res$success)) {
        factor_try <- attempt_bundle$factor_num[r]
        if (!is.na(factor_try) && factor_try == 1) {
          rp <- 0L
        } else {
          rp <- if (!is.na(attempt_bundle$next_attempt[r]) && attempt_bundle$next_attempt[r] > 0) as.integer(attempt_bundle$next_attempt[r]) else 1L
        }
        conv_code <- conv_success
        if (!is.na(factor_try) && factor_try == 1 && conv_success == 1L && isTRUE(other_flow)) conv_code <- 2L
        dat[i, `:=`(included = 1L, value_converted = res$val_conv, conversion = conv_code, rule_applied = rp)]
        return(list(success = TRUE, tried = tried))
      }
    }
  }
  list(success = FALSE, tried = tried)
}

.mo_try_fallbacks <- function(dat, i, attempt_bundle, val_raw, conv_success = 1L, other_flow = FALSE) {
  if (attempt_bundle$n <= 0) {
    return(FALSE)
  }
  fallback_order <- unique(attempt_bundle$next_attempt[!is.na(attempt_bundle$next_attempt) & attempt_bundle$next_attempt > 0])
  fallback_order <- sort(fallback_order)
  attempts_made <- 0L
  for (code in fallback_order) {
    rows_idx <- which(attempt_bundle$next_attempt == code)
    for (j in rows_idx) {
      dat[i, n_conversion_attempts := n_conversion_attempts + 1L]
      res <- .mo_eval_attempt(attempt_bundle, j, val_raw, dat, i)
      attempts_made <- attempts_made + 1L
      if (isTRUE(res$success)) {
        factor_try <- attempt_bundle$factor_num[j]
        conv_code <- conv_success
        if (!is.na(factor_try) && factor_try == 1 && conv_success == 1L && isTRUE(other_flow)) conv_code <- 2L
        dat[i, `:=`(included = 1L, value_converted = res$val_conv, conversion = conv_code, rule_applied = ifelse(attempts_made == 1L, 1L, 2L))]
        return(TRUE)
      }
    }
  }
  FALSE
}

##' Convert values using conversion metadata
#'
#' Apply conversion rules described in `metadata_convert` to the rows of
#' `dat_unit_matched`. This returns a copy of the input with `included`,
#' `value_converted`, `conversion`, and `rule_applied` columns populated.
#'
#' @param dat_unit_matched A `data.table` containing rows to convert. Must
#'   include `concept_id`, `value`, `unit_origin`, and `unit_target`.
#' @param metadata_convert A `data.table` specifying conversion attempts and thresholds.
#' @return A `data.table` with conversion result columns added.
#' @keywords internal
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
  if (!"conversion_not_multiplication" %in% names(meta)) meta[, conversion_not_multiplication := NA_character_]
  if (!"condition_on_value" %in% names(meta)) meta[, condition_on_value := NA_character_]
  if (!"condition_on_variable" %in% names(meta)) meta[, condition_on_variable := NA_character_]
  meta[, unit_matched := ifelse(is.na(unit_matched) | unit_matched == "", NA_character_, .mo_norm(unit_matched))]
  meta[, .meta_key := paste(concept_id, unit_target, sep = "\r")]
  meta_by_key <- split(meta, by = ".meta_key", keep.by = FALSE, sorted = FALSE)
  meta_bundle_by_key <- lapply(meta_by_key, .mo_prepare_attempt_bundle)
  empty_attempts <- meta[0]
  empty_attempt_bundle <- .mo_prepare_attempt_bundle(empty_attempts)
  simple_direct_meta <- meta[
    !is.na(unit_matched) &
      unit_matched != "missing" &
      (is.na(conversion_not_multiplication) | conversion_not_multiplication == "") &
      (is.na(condition_on_value) | condition_on_value == "") &
      (is.na(condition_on_variable) | condition_on_variable == "")
  ]
  if (nrow(simple_direct_meta) > 0) {
    simple_direct_meta[, factor_num := suppressWarnings(as.numeric(multiplication_factor_from_origin_to_target))]
    simple_direct_meta[, has_expr := !is.na(conversion_not_multiplication) & conversion_not_multiplication != ""]
    simple_direct_meta[, expr_parsed := lapply(conversion_not_multiplication, function(expr_text) {
      if (is.na(expr_text) || expr_text == "") {
        quote(NA_real_)
      } else {
        parse(text = expr_text)[[1]]
      }
    })]
    simple_direct_meta <- simple_direct_meta[
      (!is.na(factor_num) | has_expr) &
        !is.na(Min) &
        !is.na(Max)
    ]
    if (nrow(simple_direct_meta) > 0) {
      simple_direct_meta[, .direct_key := paste(.meta_key, unit_matched, sep = "\r")]
      simple_direct_meta <- simple_direct_meta[, if (.N == 1L) .SD, by = .direct_key]
    }
  }

  assumed_prefill_keys <- character(0)
  if (nrow(meta) > 0) {
    assumed_prefill_rows <- meta[
      unit_matched == "missing" &
        (is.na(condition_on_value) | condition_on_value == "") &
        !is.na(assumed_unit_if_missing) &
        assumed_unit_if_missing != ""
    ]
    if (nrow(assumed_prefill_rows) > 0) {
      assumed_prefill_keys <- unique(paste(
        assumed_prefill_rows$.meta_key,
        .mo_norm(assumed_prefill_rows$assumed_unit_if_missing),
        sep = "\r"
      ))
    }
  }

  # Use internal helpers

  dat[, `:=`(
    included = as.integer(NA),
    value_converted = as.numeric(NA),
    conversion = as.integer(NA),
    rule_applied = as.integer(NA),
    n_conversion_attempts = 0L
  )]

  # local alias helpers for readability
  norm <- .mo_norm
  cid_vec <- dat$concept_id
  target_vec <- dat$unit_target
  val_raw_vec <- suppressWarnings(as.numeric(dat$value))
  unit_origin_vec <- ifelse(is.na(dat$unit_origin) | dat$unit_origin == "", NA_character_, norm(dat$unit_origin))
  unit_missing_vec <- if ("unit_missing" %in% names(dat)) {
    !is.na(dat$unit_missing) & dat$unit_missing
  } else {
    is.na(unit_origin_vec)
  }
  row_unit_matched_vec <- if ("unit_matched" %in% names(dat)) {
    ifelse(is.na(dat$unit_matched) | dat$unit_matched == "", NA_character_, norm(dat$unit_matched))
  } else {
    rep(NA_character_, nrow(dat))
  }
  meta_key_vec <- paste(cid_vec, target_vec, sep = "\r")
  direct_attempt_done <- rep(FALSE, nrow(dat))
  prefilled_attempt_done <- rep(FALSE, nrow(dat))
  prefilled_attempt_tried <- integer(nrow(dat))

  if (exists("simple_direct_meta") && nrow(simple_direct_meta) > 0) {
    direct_key_vec <- paste(meta_key_vec, unit_origin_vec, sep = "\r")
    direct_meta_pos <- match(direct_key_vec, simple_direct_meta$.direct_key)
    fast_direct_idx <- which(!is.na(direct_meta_pos) & !is.na(unit_origin_vec))
    if (length(fast_direct_idx) > 0) {
      direct_attempt_done[fast_direct_idx] <- TRUE
      dat[fast_direct_idx, n_conversion_attempts := n_conversion_attempts + 1L]
      meta_rows <- simple_direct_meta[direct_meta_pos[fast_direct_idx]]
      val_conv_fast <- .mo_eval_vectorized_values(
        values = val_raw_vec[fast_direct_idx],
        meta_rows = meta_rows,
        meta_pos = direct_meta_pos[fast_direct_idx]
      )
      ok_fast <- !is.na(val_conv_fast) &
        val_conv_fast >= meta_rows$Min &
        val_conv_fast <= meta_rows$Max
      success_idx <- fast_direct_idx[ok_fast]
      if (length(success_idx) > 0) {
        success_conv <- val_conv_fast[ok_fast]
        success_is_same_unit <- unit_origin_vec[success_idx] == target_vec[success_idx]
        dat[success_idx, `:=`(
          included = 1L,
          value_converted = success_conv,
          conversion = ifelse(success_is_same_unit, 0L, 1L),
          rule_applied = ifelse(success_is_same_unit, 0L, 1L)
        )]
      }
    }

    prefill_key_vec <- paste(meta_key_vec, row_unit_matched_vec, sep = "\r")
    prefill_meta_pos <- match(prefill_key_vec, simple_direct_meta$.direct_key)
    fast_prefill_idx <- which(
      is.na(dat$included) &
        unit_missing_vec &
        !is.na(row_unit_matched_vec) &
        !is.na(prefill_meta_pos) &
        prefill_key_vec %in% assumed_prefill_keys
    )
    if (length(fast_prefill_idx) > 0) {
      prefilled_attempt_done[fast_prefill_idx] <- TRUE
      prefilled_attempt_tried[fast_prefill_idx] <- 1L
      dat[fast_prefill_idx, n_conversion_attempts := n_conversion_attempts + 1L]
      meta_rows <- simple_direct_meta[prefill_meta_pos[fast_prefill_idx]]
      val_conv_fast <- .mo_eval_vectorized_values(
        values = val_raw_vec[fast_prefill_idx],
        meta_rows = meta_rows,
        meta_pos = prefill_meta_pos[fast_prefill_idx]
      )
      ok_fast <- !is.na(val_conv_fast) &
        val_conv_fast >= meta_rows$Min &
        val_conv_fast <= meta_rows$Max
      success_idx <- fast_prefill_idx[ok_fast]
      if (length(success_idx) > 0) {
        success_meta_rows <- meta_rows[ok_fast]
        success_conv <- val_conv_fast[ok_fast]
        factor_try <- success_meta_rows$factor_num
        rp <- ifelse(
          !is.na(factor_try) & factor_try == 1,
          0L,
          ifelse(!is.na(success_meta_rows$next_attempt) & success_meta_rows$next_attempt > 0, as.integer(success_meta_rows$next_attempt), 1L)
        )
        dat[success_idx, `:=`(
          included = 1L,
          value_converted = success_conv,
          conversion = 3L,
          rule_applied = rp
        )]
      }
    }
  }

  pending_idx <- which(is.na(dat$included))
  for (i in pending_idx) {
    cid <- cid_vec[i]
    target <- target_vec[i]
    val_raw <- val_raw_vec[i]
    unit_origin <- unit_origin_vec[i]
    unit_missing_flag <- unit_missing_vec[i]

    attempt_bundle <- meta_bundle_by_key[[meta_key_vec[i]]]
    if (is.null(attempt_bundle)) attempt_bundle <- empty_attempt_bundle
    attempt_unit_matched <- if (attempt_bundle$n > 0) ifelse(is.na(attempt_bundle$unit_matched), unit_origin, attempt_bundle$unit_matched) else character(0)

    # Identify 'OTHER' origin: unit present but not listed among attempts
    origin_other <- !is.na(unit_origin) && attempt_bundle$n > 0 && !(unit_origin %in% attempt_unit_matched)

    # 1) Try direct matches (unit_origin equals unit_matched)
    if (!direct_attempt_done[i] && .mo_try_direct_matches(dat, i, attempt_bundle, attempt_unit_matched, unit_origin, target, val_raw)) next

    # If origin is 'OTHER' (present but not listed), treat it like MISSING for
    # conversion attempts (but use conversion=1 on success). Otherwise, for
    # genuinely missing units use conversion=3 on success.
    missing_attempts_tried <- prefilled_attempt_tried[i]
    if (origin_other) {
      row_unit_matched <- row_unit_matched_vec[i]
      pref_res <- list(success = FALSE, tried = prefilled_attempt_tried[i])
      if (!prefilled_attempt_done[i] && !is.na(row_unit_matched) && attempt_bundle$n > 0) {
        pref_res <- .mo_try_prefilled_assumed(dat, i, attempt_bundle, attempt_unit_matched, row_unit_matched, val_raw, conv_success = 1L, other_flow = TRUE)
        missing_attempts_tried <- missing_attempts_tried + pref_res$tried
        if (isTRUE(pref_res$success)) next
      }
      skip_assumed_unit <- if (prefilled_attempt_done[i] || pref_res$tried > 0L) row_unit_matched else NA_character_
      miss_res <- .mo_try_missing_chain(dat, i, attempt_bundle, attempt_unit_matched, val_raw, conv_success = 1L, other_flow = TRUE, skip_assumed_unit = skip_assumed_unit)
      missing_attempts_tried <- missing_attempts_tried + miss_res$tried
      if (isTRUE(miss_res$success)) next
    } else {
      if (unit_missing_flag) {
        row_unit_matched <- row_unit_matched_vec[i]
        pref_res <- list(success = FALSE, tried = prefilled_attempt_tried[i])
        if (!prefilled_attempt_done[i] && !is.na(row_unit_matched) && attempt_bundle$n > 0) {
          pref_res <- .mo_try_prefilled_assumed(dat, i, attempt_bundle, attempt_unit_matched, row_unit_matched, val_raw, conv_success = 3L, other_flow = FALSE)
          missing_attempts_tried <- missing_attempts_tried + pref_res$tried
          if (isTRUE(pref_res$success)) next
        }
        # Only call the missing chain if prefill did not already cover the single attempt
        # (i.e., prefill did not run, or there are chained attempts with next_attempt > 0 to pursue).
        if (attempt_bundle$n > 0 && (pref_res$tried == 0L || any(!is.na(attempt_bundle$next_attempt) & attempt_bundle$next_attempt > 0L))) {
          skip_assumed_unit <- if (prefilled_attempt_done[i] || pref_res$tried > 0L) row_unit_matched else NA_character_
          miss_res <- .mo_try_missing_chain(dat, i, attempt_bundle, attempt_unit_matched, val_raw, conv_success = 3L, other_flow = FALSE, skip_assumed_unit = skip_assumed_unit)
          missing_attempts_tried <- missing_attempts_tried + miss_res$tried
          if (isTRUE(miss_res$success)) next
        }
      }
    }

    # 3) Fallback: try all attempts ordered by next_attempt (1,2,...) as fallbacks for conversion
    conv_success_current <- if (origin_other) 1L else if (unit_missing_flag) 3L else 1L
    other_flow_current <- origin_other
    if (.mo_try_fallbacks(dat, i, attempt_bundle, val_raw, conv_success_current, other_flow_current)) next

    # 4) If we get here, conversion failed or no applicable attempts
    # Decide conversion & rule codes per README semantics
    if (is.na(val_raw) || !is.numeric(val_raw)) {
      dat[i, `:=`(included = 0L, value_converted = NA_real_, conversion = 3L, rule_applied = 99L)]
    } else if (unit_missing_flag) {
      # Use only the missing-unit attempts count to decide 90/91/92
      attempts_made_final <- missing_attempts_tried
      rp_fail <- ifelse(is.na(attempts_made_final) || attempts_made_final == 0L, 90L, ifelse(attempts_made_final == 1L, 91L, 92L))
      dat[i, `:=`(included = 0L, value_converted = NA_real_, conversion = 3L, rule_applied = rp_fail)]
    } else if (!is.na(unit_origin) && attempt_bundle$n == 0) {
      # unit present but no conversion metadata -> treat as OTHER
      dat[i, `:=`(included = 1L, value_converted = val_raw, conversion = 2L, rule_applied = 0L)]
    } else if (!is.na(unit_origin) && !is.na(target) && unit_origin != target) {
      # Tried conversions but none accepted
      # If no conversion attempts were actually made for this row, treat as OTHER accepted-as-is
      attempts_made <- dat$n_conversion_attempts[i]
      if (is.na(attempts_made) || attempts_made == 0L) {
        dat[i, `:=`(included = 1L, value_converted = val_raw, conversion = 2L, rule_applied = 0L)]
      } else {
        # Use actual number of attempts made on this row to determine rule_applied (91 if one try, 92 if multiple)
        final_conv <- if (origin_other) 2L else 1L
        dat[i, `:=`(included = 0L, value_converted = NA_real_, conversion = final_conv, rule_applied = ifelse(attempts_made <= 1L, 91L, 92L))]
      }
    } else {
      dat[i, `:=`(included = 0L, value_converted = NA_real_, conversion = 0L, rule_applied = 90L)]
    }
  }

  # Clean helper columns
  dat[, n_conversion_attempts := NULL]
  return(dat)
}
