#' Unit conversion helpers
#'
#' Internal helpers and the `mo_convert()` implementation used to convert
#' lab values to target units and compute inclusion/conversion codes.
#'
#' These functions are internal to the package and are not exported.
#'
#' @name mo_convert_helpers
#' @keywords internal
NULL
.mo_norm <- function(x) tolower(trimws(as.character(x)))

.mo_get_cached_var_value <- function(var_cache, row_idx, varname) {
  if (is.null(varname) || is.na(varname) || varname == "") {
    return(NA)
  }
  values <- var_cache[[varname]]
  if (is.null(values)) {
    return(NA)
  }
  values[row_idx]
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

.mo_finalize_attempt_bundle <- function(attempt_bundle) {
  if (attempt_bundle$n == 0L) {
    attempt_bundle$next_positive_idx <- integer(0)
    attempt_bundle$missing_idx <- integer(0)
    attempt_bundle$fallback_idx_by_code <- list()
    attempt_bundle$assumed_units <- character(0)
    return(attempt_bundle)
  }

  next_pos <- which(!is.na(attempt_bundle$next_attempt) & attempt_bundle$next_attempt > 0)
  attempt_bundle$next_positive_idx <- if (length(next_pos) > 0) {
    next_pos[order(attempt_bundle$next_attempt[next_pos])]
  } else {
    integer(0)
  }
  attempt_bundle$missing_idx <- which(attempt_bundle$unit_matched == "missing")
  fallback_order <- sort(unique(attempt_bundle$next_attempt[next_pos]))
  attempt_bundle$fallback_idx_by_code <- lapply(fallback_order, function(code) {
    which(attempt_bundle$next_attempt == code)
  })
  names(attempt_bundle$fallback_idx_by_code) <- as.character(fallback_order)

  assumed_idx <- which(
    attempt_bundle$unit_matched == "missing" &
      (is.na(attempt_bundle$condition_on_value) | attempt_bundle$condition_on_value == "")
  )
  attempt_bundle$assumed_units <- unique(na.omit(.mo_norm(attempt_bundle$assumed_unit_if_missing[assumed_idx])))
  attempt_bundle
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

.mo_eval_attempt_prepared <- function(attempt_bundle, attempt_idx, val_raw, row_idx, var_cache) {
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
      cond_val <- .mo_get_cached_var_value(var_cache, row_idx, varname)
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

.mo_success_codes <- function(attempt_bundle, attempt_idx, conv_success, other_flow = FALSE, fallback_attempts_made = NA_integer_) {
  factor_try <- attempt_bundle$factor_num[attempt_idx]
  conv_code <- conv_success
  if (!is.na(factor_try) && factor_try == 1 && conv_success == 1L && isTRUE(other_flow)) {
    conv_code <- 2L
  }
  if (!is.na(fallback_attempts_made)) {
    rp <- ifelse(fallback_attempts_made == 1L, 1L, 2L)
  } else if (!is.na(factor_try) && factor_try == 1) {
    rp <- 0L
  } else {
    rp <- if (!is.na(attempt_bundle$next_attempt[attempt_idx]) && attempt_bundle$next_attempt[attempt_idx] > 0) {
      as.integer(attempt_bundle$next_attempt[attempt_idx])
    } else {
      1L
    }
  }
  list(conversion = as.integer(conv_code), rule_applied = as.integer(rp))
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
  meta_bundle_by_key <- lapply(meta_by_key, function(x) .mo_finalize_attempt_bundle(.mo_prepare_attempt_bundle(x)))
  empty_attempts <- meta[0]
  empty_attempt_bundle <- .mo_finalize_attempt_bundle(.mo_prepare_attempt_bundle(empty_attempts))
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

  # local alias helpers for readability
  norm <- .mo_norm
  n_dat <- nrow(dat)
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
    rep(NA_character_, n_dat)
  }
  meta_key_vec <- paste(cid_vec, target_vec, sep = "\r")
  direct_attempt_done <- rep(FALSE, n_dat)
  prefilled_attempt_done <- rep(FALSE, n_dat)
  prefilled_attempt_tried <- integer(n_dat)
  n_conversion_attempts <- integer(n_dat)
  included <- rep(NA_integer_, n_dat)
  value_converted <- rep(NA_real_, n_dat)
  conversion <- rep(NA_integer_, n_dat)
  rule_applied <- rep(NA_integer_, n_dat)

  condition_variables <- unique(unlist(lapply(meta_bundle_by_key, function(bundle) bundle$variable), use.names = FALSE))
  condition_variables <- condition_variables[!is.na(condition_variables) & condition_variables != "" & condition_variables %in% names(dat)]
  var_cache <- if (length(condition_variables) > 0) {
    dat[, ..condition_variables]
  } else {
    list()
  }

  if (exists("simple_direct_meta") && nrow(simple_direct_meta) > 0) {
    direct_key_vec <- paste(meta_key_vec, unit_origin_vec, sep = "\r")
    direct_meta_pos <- match(direct_key_vec, simple_direct_meta$.direct_key)
    fast_direct_idx <- which(!is.na(direct_meta_pos) & !is.na(unit_origin_vec))
    if (length(fast_direct_idx) > 0) {
      direct_attempt_done[fast_direct_idx] <- TRUE
      n_conversion_attempts[fast_direct_idx] <- n_conversion_attempts[fast_direct_idx] + 1L
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
        included[success_idx] <- 1L
        value_converted[success_idx] <- success_conv
        conversion[success_idx] <- ifelse(success_is_same_unit, 0L, 1L)
        rule_applied[success_idx] <- ifelse(success_is_same_unit, 0L, 1L)
      }
    }

    prefill_key_vec <- paste(meta_key_vec, row_unit_matched_vec, sep = "\r")
    prefill_meta_pos <- match(prefill_key_vec, simple_direct_meta$.direct_key)
    fast_prefill_idx <- which(
      is.na(included) &
        unit_missing_vec &
        !is.na(row_unit_matched_vec) &
        !is.na(prefill_meta_pos) &
        prefill_key_vec %in% assumed_prefill_keys
    )
    if (length(fast_prefill_idx) > 0) {
      prefilled_attempt_done[fast_prefill_idx] <- TRUE
      prefilled_attempt_tried[fast_prefill_idx] <- 1L
      n_conversion_attempts[fast_prefill_idx] <- n_conversion_attempts[fast_prefill_idx] + 1L
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
        included[success_idx] <- 1L
        value_converted[success_idx] <- success_conv
        conversion[success_idx] <- 3L
        rule_applied[success_idx] <- rp
      }
    }
  }

  pending_idx <- which(is.na(included))
  for (i in pending_idx) {
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
    if (!direct_attempt_done[i] && attempt_bundle$n > 0 && !is.na(unit_origin)) {
      direct_idx <- which(attempt_unit_matched == unit_origin)
      if (length(direct_idx) > 0) {
        direct_success <- FALSE
        for (r in direct_idx) {
          n_conversion_attempts[i] <- n_conversion_attempts[i] + 1L
          res <- .mo_eval_attempt_prepared(attempt_bundle, r, val_raw, i, var_cache)
          if (isTRUE(res$success)) {
            included[i] <- 1L
            value_converted[i] <- res$val_conv
            conversion[i] <- ifelse(is.na(unit_origin) || unit_origin == target, 0L, ifelse(unit_origin == target, 0L, 1L))
            rule_applied[i] <- ifelse(is.na(unit_origin) || unit_origin == target, 0L, 1L)
            direct_success <- TRUE
            break
          }
        }
        if (direct_success) next
      }
    }

    # If origin is 'OTHER' (present but not listed), treat it like MISSING for
    # conversion attempts (but use conversion=1 on success). Otherwise, for
    # genuinely missing units use conversion=3 on success.
    missing_attempts_tried <- prefilled_attempt_tried[i]
    if (origin_other) {
      row_unit_matched <- row_unit_matched_vec[i]
      pref_res <- list(success = FALSE, tried = prefilled_attempt_tried[i])
      if (!prefilled_attempt_done[i] && !is.na(row_unit_matched) && attempt_bundle$n > 0) {
        if (row_unit_matched %in% attempt_bundle$assumed_units) {
          assumed_rows_idx <- which(attempt_unit_matched == row_unit_matched)
          if (length(assumed_rows_idx) > 0) {
            n_conversion_attempts[i] <- n_conversion_attempts[i] + 1L
            assumed_idx_first <- assumed_rows_idx[1]
            res <- .mo_eval_attempt_prepared(attempt_bundle, assumed_idx_first, val_raw, i, var_cache)
            pref_res <- list(success = isTRUE(res$success), tried = 1L)
            if (isTRUE(res$success)) {
              codes <- .mo_success_codes(attempt_bundle, assumed_idx_first, conv_success = 1L, other_flow = TRUE)
              included[i] <- 1L
              value_converted[i] <- res$val_conv
              conversion[i] <- codes$conversion
              rule_applied[i] <- codes$rule_applied
            }
          }
        }
        missing_attempts_tried <- missing_attempts_tried + pref_res$tried
        if (isTRUE(pref_res$success)) next
      }
      skip_assumed_unit <- if (prefilled_attempt_done[i] || pref_res$tried > 0L) row_unit_matched else NA_character_
      miss_res <- list(success = FALSE, tried = 0L)
      if (length(attempt_bundle$next_positive_idx) > 0) {
        for (r in attempt_bundle$next_positive_idx) {
          res <- .mo_eval_attempt_prepared(attempt_bundle, r, val_raw, i, var_cache)
          if (isTRUE(res$attempted)) {
            n_conversion_attempts[i] <- n_conversion_attempts[i] + 1L
            miss_res$tried <- miss_res$tried + 1L
          }
          if (isTRUE(res$success)) {
            codes <- .mo_success_codes(attempt_bundle, r, conv_success = 1L, other_flow = TRUE)
            included[i] <- 1L
            value_converted[i] <- res$val_conv
            conversion[i] <- codes$conversion
            rule_applied[i] <- codes$rule_applied
            miss_res$success <- TRUE
            break
          }
        }
      }
      if (!isTRUE(miss_res$success)) {
        missing_idx <- attempt_bundle$missing_idx
        if (!is.na(skip_assumed_unit) && length(missing_idx) > 0) {
          missing_idx <- missing_idx[
            is.na(attempt_bundle$next_attempt[missing_idx]) |
              attempt_bundle$next_attempt[missing_idx] != 0 |
              .mo_norm(attempt_bundle$assumed_unit_if_missing[missing_idx]) != skip_assumed_unit
          ]
        }
        if (length(missing_idx) > 0) {
          for (r in missing_idx) {
            res <- .mo_eval_attempt_prepared(attempt_bundle, r, val_raw, i, var_cache)
            if (isTRUE(res$attempted)) {
              n_conversion_attempts[i] <- n_conversion_attempts[i] + 1L
              miss_res$tried <- miss_res$tried + 1L
            }
            if (isTRUE(res$success)) {
              codes <- .mo_success_codes(attempt_bundle, r, conv_success = 1L, other_flow = TRUE)
              included[i] <- 1L
              value_converted[i] <- res$val_conv
              conversion[i] <- codes$conversion
              rule_applied[i] <- codes$rule_applied
              miss_res$success <- TRUE
              break
            }
          }
        }
      }
      missing_attempts_tried <- missing_attempts_tried + miss_res$tried
      if (isTRUE(miss_res$success)) next
    } else {
      if (unit_missing_flag) {
        row_unit_matched <- row_unit_matched_vec[i]
        pref_res <- list(success = FALSE, tried = prefilled_attempt_tried[i])
        if (!prefilled_attempt_done[i] && !is.na(row_unit_matched) && attempt_bundle$n > 0) {
          if (row_unit_matched %in% attempt_bundle$assumed_units) {
            assumed_rows_idx <- which(attempt_unit_matched == row_unit_matched)
            if (length(assumed_rows_idx) > 0) {
              n_conversion_attempts[i] <- n_conversion_attempts[i] + 1L
              assumed_idx_first <- assumed_rows_idx[1]
              res <- .mo_eval_attempt_prepared(attempt_bundle, assumed_idx_first, val_raw, i, var_cache)
              pref_res <- list(success = isTRUE(res$success), tried = 1L)
              if (isTRUE(res$success)) {
                codes <- .mo_success_codes(attempt_bundle, assumed_idx_first, conv_success = 3L, other_flow = FALSE)
                included[i] <- 1L
                value_converted[i] <- res$val_conv
                conversion[i] <- codes$conversion
                rule_applied[i] <- codes$rule_applied
              }
            }
          }
          missing_attempts_tried <- missing_attempts_tried + pref_res$tried
          if (isTRUE(pref_res$success)) next
        }
        # Only call the missing chain if prefill did not already cover the single attempt
        # (i.e., prefill did not run, or there are chained attempts with next_attempt > 0 to pursue).
        if (attempt_bundle$n > 0 && (pref_res$tried == 0L || any(!is.na(attempt_bundle$next_attempt) & attempt_bundle$next_attempt > 0L))) {
          skip_assumed_unit <- if (prefilled_attempt_done[i] || pref_res$tried > 0L) row_unit_matched else NA_character_
          miss_res <- list(success = FALSE, tried = 0L)
          if (length(attempt_bundle$next_positive_idx) > 0) {
            for (r in attempt_bundle$next_positive_idx) {
              res <- .mo_eval_attempt_prepared(attempt_bundle, r, val_raw, i, var_cache)
              if (isTRUE(res$attempted)) {
                n_conversion_attempts[i] <- n_conversion_attempts[i] + 1L
                miss_res$tried <- miss_res$tried + 1L
              }
              if (isTRUE(res$success)) {
                codes <- .mo_success_codes(attempt_bundle, r, conv_success = 3L, other_flow = FALSE)
                included[i] <- 1L
                value_converted[i] <- res$val_conv
                conversion[i] <- codes$conversion
                rule_applied[i] <- codes$rule_applied
                miss_res$success <- TRUE
                break
              }
            }
          }
          if (!isTRUE(miss_res$success)) {
            missing_idx <- attempt_bundle$missing_idx
            if (!is.na(skip_assumed_unit) && length(missing_idx) > 0) {
              missing_idx <- missing_idx[
                is.na(attempt_bundle$next_attempt[missing_idx]) |
                  attempt_bundle$next_attempt[missing_idx] != 0 |
                  .mo_norm(attempt_bundle$assumed_unit_if_missing[missing_idx]) != skip_assumed_unit
              ]
            }
            if (length(missing_idx) > 0) {
              for (r in missing_idx) {
                res <- .mo_eval_attempt_prepared(attempt_bundle, r, val_raw, i, var_cache)
                if (isTRUE(res$attempted)) {
                  n_conversion_attempts[i] <- n_conversion_attempts[i] + 1L
                  miss_res$tried <- miss_res$tried + 1L
                }
                if (isTRUE(res$success)) {
                  codes <- .mo_success_codes(attempt_bundle, r, conv_success = 3L, other_flow = FALSE)
                  included[i] <- 1L
                  value_converted[i] <- res$val_conv
                  conversion[i] <- codes$conversion
                  rule_applied[i] <- codes$rule_applied
                  miss_res$success <- TRUE
                  break
                }
              }
            }
          }
          missing_attempts_tried <- missing_attempts_tried + miss_res$tried
          if (isTRUE(miss_res$success)) next
        }
      }
    }

    # 3) Fallback: try all attempts ordered by next_attempt (1,2,...) as fallbacks for conversion
    conv_success_current <- if (origin_other) 1L else if (unit_missing_flag) 3L else 1L
    other_flow_current <- origin_other
    fallback_success <- FALSE
    if (attempt_bundle$n > 0 && length(attempt_bundle$fallback_idx_by_code) > 0) {
      attempts_made <- 0L
      for (rows_idx in attempt_bundle$fallback_idx_by_code) {
        for (j in rows_idx) {
          n_conversion_attempts[i] <- n_conversion_attempts[i] + 1L
          res <- .mo_eval_attempt_prepared(attempt_bundle, j, val_raw, i, var_cache)
          attempts_made <- attempts_made + 1L
          if (isTRUE(res$success)) {
            codes <- .mo_success_codes(
              attempt_bundle,
              j,
              conv_success = conv_success_current,
              other_flow = other_flow_current,
              fallback_attempts_made = attempts_made
            )
            included[i] <- 1L
            value_converted[i] <- res$val_conv
            conversion[i] <- codes$conversion
            rule_applied[i] <- codes$rule_applied
            fallback_success <- TRUE
            break
          }
        }
        if (fallback_success) break
      }
    }
    if (fallback_success) next

    # 4) If we get here, conversion failed or no applicable attempts
    # Decide conversion & rule codes per README semantics
    if (is.na(val_raw) || !is.numeric(val_raw)) {
      included[i] <- 0L
      value_converted[i] <- NA_real_
      conversion[i] <- 3L
      rule_applied[i] <- 99L
    } else if (unit_missing_flag) {
      # Use only the missing-unit attempts count to decide 90/91/92
      attempts_made_final <- missing_attempts_tried
      rp_fail <- ifelse(is.na(attempts_made_final) || attempts_made_final == 0L, 90L, ifelse(attempts_made_final == 1L, 91L, 92L))
      included[i] <- 0L
      value_converted[i] <- NA_real_
      conversion[i] <- 3L
      rule_applied[i] <- rp_fail
    } else if (!is.na(unit_origin) && attempt_bundle$n == 0) {
      # unit present but no conversion metadata -> treat as OTHER
      included[i] <- 1L
      value_converted[i] <- val_raw
      conversion[i] <- 2L
      rule_applied[i] <- 0L
    } else if (!is.na(unit_origin) && !is.na(target) && unit_origin != target) {
      # Tried conversions but none accepted
      # If no conversion attempts were actually made for this row, treat as OTHER accepted-as-is
      attempts_made <- n_conversion_attempts[i]
      if (is.na(attempts_made) || attempts_made == 0L) {
        included[i] <- 1L
        value_converted[i] <- val_raw
        conversion[i] <- 2L
        rule_applied[i] <- 0L
      } else {
        # Use actual number of attempts made on this row to determine rule_applied (91 if one try, 92 if multiple)
        final_conv <- if (origin_other) 2L else 1L
        included[i] <- 0L
        value_converted[i] <- NA_real_
        conversion[i] <- final_conv
        rule_applied[i] <- ifelse(attempts_made <= 1L, 91L, 92L)
      }
    } else {
      included[i] <- 0L
      value_converted[i] <- NA_real_
      conversion[i] <- 0L
      rule_applied[i] <- 90L
    }
  }

  dat[, `:=`(
    included = included,
    value_converted = value_converted,
    conversion = conversion,
    rule_applied = rule_applied
  )]
  return(dat)
}
