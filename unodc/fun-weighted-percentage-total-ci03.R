#' @import data.table
NULL

# ---------------------------------------------------------------------------
# calc_percentage.R
#
# Design: every internal helper below does exactly one job, and most of them
# are pure functions (vector/scalar in, vector/scalar out) rather than
# table-mutators, so each one can be unit-tested on its own without needing
# a data.table fixture. The two exported functions at the bottom are just
# orchestration -- they call the helpers in sequence and contain almost no
# arithmetic of their own.
#
# Why this still avoids the original branching problem:
#   - `by = group_vars` accepts NULL, so grouped and overall calculations
#     use the same aggregation helper.
#   - A constant weight vector of 1 stands in for "unweighted", so the
#     weighted and unweighted cases share the same arithmetic everywhere
#     (numerator, denominator, n_eff, and the CI design effect all reduce
#     correctly when every weight is 1).
# ---------------------------------------------------------------------------


# --- Validation --------------------------------------------------------

#' Check that a data.table contains all required columns
#'
#' @param dt A data.table
#' @param required_cols Character vector of column names that must be present
#' @return Invisibly TRUE. Throws an error listing any missing columns.
#' @keywords internal
.assert_columns_exist <- function(dt, required_cols) {
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate the inputs to calc_percentage_total_ci()
#'
#' Groups every "is this call even well-formed" check in one place so the
#' main function body doesn't have to interleave validation with logic.
#'
#' @param dt A data.table
#' @param outcome_var,group_vars,weight_var,denominator_var Column-name arguments to check
#' @return Invisibly TRUE. Throws an error on the first problem found.
#' @keywords internal
.validate_pct_inputs <- function(dt, outcome_var, group_vars, weight_var, denominator_var) {
  if (!data.table::is.data.table(dt)) {
    stop("Input 'dt' must be a data.table", call. = FALSE)
  }
  required_cols <- c(outcome_var, group_vars, weight_var, denominator_var)
  .assert_columns_exist(dt, required_cols)
  invisible(TRUE)
}


# --- Row filtering -------------------------------------------------------

#' Restrict a data.table to the denominator population
#'
#' When `denominator_var` is supplied, keeps only rows where that column
#' equals `denominator_value`. Warns (rather than errors) if nothing
#' matches, since an empty result is a valid -- if surprising -- outcome for
#' a single group in a larger loop.
#'
#' @param dt_work A data.table (already a private copy, safe to filter)
#' @param denominator_var Character or NULL. Column defining the population
#' @param denominator_value Value that `denominator_var` must equal
#' @return The filtered data.table (same object if `denominator_var` is NULL)
#' @keywords internal
.filter_to_denominator <- function(dt_work, denominator_var, denominator_value) {
  if (is.null(denominator_var)) {
    return(dt_work)
  }
  filtered <- dt_work[get(denominator_var) == denominator_value]
  if (nrow(filtered) == 0) {
    warning("No rows found where ", denominator_var, " == ", denominator_value, call. = FALSE)
  }
  filtered
}


# --- Outcome indicator ----------------------------------------------------

#' Convert a raw outcome vector into a clean 0/1/NA indicator
#'
#' This is the single place that interprets `na_treatment`, so the rest of
#' the pipeline never needs to think about missingness again.
#'
#' @param outcome A numeric or logical vector, expected to hold 0/1 values
#' @param na_treatment "exclude" (missing values stay NA and are dropped
#'   downstream via na.rm / !is.na) or "as_zero" (missing values become 0
#'   and are counted in the denominator)
#' @return A numeric vector of 0, 1, or NA, the same length as `outcome`
#' @keywords internal
.make_outcome_indicator <- function(outcome, na_treatment = c("exclude", "as_zero")) {
  na_treatment <- match.arg(na_treatment)

  # 1 where outcome == 1, 0 where outcome == 0 (including 0/FALSE), NA where missing
  indicator <- data.table::fifelse(is.na(outcome), NA_real_,
                                   data.table::fifelse(outcome == 1, 1, 0))

  if (na_treatment == "as_zero") {
    indicator[is.na(indicator)] <- 0
  }
  indicator
}


# --- Weight handling -------------------------------------------------------

#' Produce a numeric weight vector, defaulting to a constant 1
#'
#' Centralizes the "no weight_var means unweighted" rule in one function so
#' every downstream calculation (numerator, denominator, n_eff, CI) can
#' treat weighted and unweighted data identically.
#'
#' @param dt_work A data.table
#' @param weight_var Character or NULL. Name of the weight column
#' @return A numeric vector of length `nrow(dt_work)`
#' @keywords internal
.make_weight_vector <- function(dt_work, weight_var) {
  if (is.null(weight_var)) {
    return(rep(1, nrow(dt_work)))
  }
  as.numeric(dt_work[[weight_var]])
}


# --- Core aggregation -------------------------------------------------------

#' Attach the normalized indicator/weight helper columns used internally
#'
#' Keeps `.pct_ind_` / `.pct_wt_` naming and mutation in exactly one place,
#' so no other function needs to know these column names exist.
#'
#' @param dt_work A data.table, modified in place
#' @param indicator Numeric vector from [.make_outcome_indicator()]
#' @param weight Numeric vector from [.make_weight_vector()]
#' @return `dt_work`, invisibly, with `.pct_ind_` and `.pct_wt_` columns added
#' @keywords internal
.attach_helper_columns <- function(dt_work, indicator, weight) {
  dt_work[, .pct_ind_ := indicator]
  dt_work[, .pct_wt_ := weight]
  invisible(dt_work)
}

#' Aggregate the raw sums needed for a percentage, grouped or not
#'
#' The only aggregation call in the whole file. `by = group_vars` is passed
#' straight through to data.table, which treats `NULL` as "no grouping" --
#' this is what lets the same call serve both the per-group results and the
#' overall total row.
#'
#' @param dt_work A data.table already prepared by [.attach_helper_columns()]
#' @param outcome_var Character. Name of the raw outcome column (used only
#'   for diagnostic counts, not for the percentage itself)
#' @param group_vars Character vector or NULL
#' @return A data.table with one row per group (or one row overall),
#'   containing `numerator`, `denominator`, `sum_weights`,
#'   `sum_weights_squared`, and raw diagnostic counts
#' @keywords internal
.aggregate_pct_components <- function(dt_work, outcome_var, group_vars) {
  dt_work[, .(
    # Weighted count of outcome == 1. na.rm = TRUE drops NA-indicator rows,
    # which is exactly "exclude" behaviour; under "as_zero" there are no
    # NAs left to drop, so this is unaffected.
    numerator = sum(.pct_wt_ * .pct_ind_, na.rm = TRUE),

    # Weight summed only over rows with a non-missing indicator, i.e. the
    # population actually used as the denominator.
    denominator = sum(.pct_wt_[!is.na(.pct_ind_)]),

    # Needed for both n_eff and the weighted CI design effect.
    sum_weights_squared = sum(.pct_wt_[!is.na(.pct_ind_)]^2),

    # Diagnostics computed on the raw outcome column, independent of
    # na_treatment, so they always describe the true group composition.
    sum_weights  = sum(.pct_wt_),
    n_total      = .N,
    n_outcome_1  = sum(get(outcome_var) == 1, na.rm = TRUE),
    n_outcome_0  = sum(get(outcome_var) == 0, na.rm = TRUE),
    n_outcome_na = sum(is.na(get(outcome_var))),
    n_valid      = sum(!is.na(get(outcome_var)))
  ), by = group_vars]
}


# --- Percentage / effective sample size -------------------------------------

#' Add a `proportion` column (numerator / denominator)
#'
#' @param agg A data.table with `numerator` and `denominator` columns
#' @return `agg`, with a `proportion` column added
#' @keywords internal
.add_proportion <- function(agg) {
  agg[, proportion := numerator / denominator]
  agg[]
}

#' Add a rounded `percentage` column derived from `proportion`
#'
#' @param agg A data.table with a `proportion` column
#' @param round_digits Integer decimal places to round to
#' @return `agg`, with a `percentage` column added
#' @keywords internal
.add_percentage <- function(agg, round_digits) {
  agg[, percentage := round(proportion * 100, round_digits)]
  agg[]
}

#' Add the effective sample size `n_eff`
#'
#' `n_eff = sum(w)^2 / sum(w^2)` over the denominator population. When every
#' weight is 1 (the unweighted case) this reduces exactly to the valid
#' observation count, so the same formula is correct in both cases.
#'
#' @param agg A data.table with `sum_weights` and `sum_weights_squared` columns
#' @return `agg`, with an `n_eff` column added
#' @keywords internal
.add_n_eff <- function(agg) {
  agg[, n_eff := (sum_weights^2) / sum_weights_squared]
  agg[]
}


# --- Confidence intervals ----------------------------------------------------

#' Wilson score confidence interval for an unweighted proportion
#'
#' More reliable than the Wald interval for small samples or proportions
#' near 0 or 1.
#'
#' @param p Numeric vector of proportions (0-1)
#' @param n Numeric vector of sample sizes
#' @return A list with numeric elements `lower`, `upper` (proportions, 0-1)
#' @keywords internal
.ci_wilson <- function(p, n) {
  z <- qnorm(0.975)
  center <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  margin <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / (1 + z^2 / n)
  list(lower = pmax(0, center - margin), upper = pmin(1, center + margin))
}

#' Wald (normal approximation) confidence interval for a proportion
#'
#' @param p Numeric vector of proportions (0-1)
#' @param n Numeric vector of sample sizes
#' @return A list with numeric elements `lower`, `upper` (proportions, 0-1)
#' @keywords internal
.ci_wald <- function(p, n) {
  z <- qnorm(0.975)
  se <- sqrt(p * (1 - p) / n)
  list(lower = pmax(0, p - z * se), upper = pmin(1, p + z * se))
}

#' Design-adjusted normal confidence interval for a weighted proportion
#'
#' Uses SE = sqrt(p(1-p) * sum(w^2) / sum(w)^2), the standard adjustment
#' for unequal survey weights.
#'
#' @param p Numeric vector of proportions (0-1)
#' @param sum_w Numeric vector. Sum of weights (the weighted denominator)
#' @param sum_w2 Numeric vector. Sum of squared weights
#' @return A list with numeric elements `lower`, `upper` (proportions, 0-1)
#' @keywords internal
.ci_weighted_normal <- function(p, sum_w, sum_w2) {
  z <- qnorm(0.975)
  se <- sqrt(p * (1 - p) * sum_w2 / (sum_w^2))
  list(lower = pmax(0, p - z * se), upper = pmin(1, p + z * se))
}

#' Attach ci_lower / ci_upper columns to an aggregated result
#'
#' Single dispatch point: picks the correct CI formula based on whether the
#' calculation is weighted, and (if unweighted) which `ci_method` was
#' requested. Every caller gets the same two output columns regardless of
#' which formula was used underneath.
#'
#' @param agg A data.table with `proportion`, `denominator`, and
#'   `sum_weights_squared` columns
#' @param weighted Logical. Whether this is a weighted calculation
#' @param ci_method "wilson" or "normal" (only used when `weighted` is FALSE)
#' @param round_digits Integer decimal places for the output CI bounds
#' @return `agg`, with `ci_lower` and `ci_upper` columns added (percentage scale)
#' @keywords internal
.add_ci <- function(agg, weighted, ci_method = c("wilson", "normal"), round_digits) {
  ci_method <- match.arg(ci_method)

  ci <- if (weighted) {
    .ci_weighted_normal(agg$proportion, agg$denominator, agg$sum_weights_squared)
  } else if (ci_method == "wilson") {
    .ci_wilson(agg$proportion, agg$denominator)
  } else {
    .ci_wald(agg$proportion, agg$denominator)
  }

  agg[, ci_lower := round(ci$lower * 100, round_digits)]
  agg[, ci_upper := round(ci$upper * 100, round_digits)]
  agg[]
}


# --- Group / column bookkeeping ---------------------------------------------

#' Drop result rows where any grouping variable is NA
#'
#' A group level of NA is generally noise (missing data in a grouping
#' column), not a group worth reporting.
#'
#' @param results A data.table
#' @param group_vars Character vector or NULL
#' @return `results`, with NA-group rows removed (unchanged if `group_vars` is NULL)
#' @keywords internal
.drop_na_group_rows <- function(results, group_vars) {
  if (is.null(group_vars)) {
    return(results)
  }
  for (v in group_vars) {
    results <- results[!is.na(get(v))]
  }
  results
}

#' Choose and order the columns to return to the user
#'
#' Keeps the "which columns does the user actually see" decision separate
#' from the arithmetic that produced them.
#'
#' @param results A data.table containing (at least) the candidate columns
#' @param group_vars Character vector or NULL
#' @param weighted Logical. Whether `sum_weights` should be included
#' @param include_diagnostics Logical. Whether to include n_total/n_valid/etc.
#' @param include_ci Logical. Whether to include ci_lower/ci_upper
#' @return `results` subset to just the selected columns, in order
#' @keywords internal
.select_output_columns <- function(results, group_vars, weighted, include_diagnostics, include_ci) {
  base_cols <- c("percentage", "calculation_type")

  if (include_diagnostics) {
    base_cols <- c(base_cols, "n_total", "n_valid", "n_outcome_1",
                   "n_outcome_0", "n_outcome_na", "n_eff")
    if (weighted) base_cols <- c(base_cols, "sum_weights")
  }
  if (include_ci) {
    base_cols <- c(base_cols, "ci_lower", "ci_upper")
  }

  final_cols <- c(group_vars, base_cols)
  results[, ..final_cols]
}


# --- One-shot result builder (used for both grouped results and the total row) --

#' Run the full aggregate -> proportion -> percentage -> n_eff -> CI pipeline
#'
#' This is the one place that stitches the small helpers together into a
#' complete result table. It is called twice by the exported function: once
#' with the user's `group_vars`, and once with `group_vars = NULL` to build
#' the total row -- guaranteeing the total is computed with exactly the same
#' logic as every other row.
#'
#' @param dt_work A data.table already prepared by [.attach_helper_columns()]
#' @param outcome_var Character
#' @param group_vars Character vector or NULL
#' @param weighted Logical
#' @param include_ci Logical
#' @param ci_method "wilson" or "normal"
#' @param round_digits Integer
#' @return A data.table: one row per group (or one row overall), fully computed
#' @keywords internal
.build_pct_result <- function(dt_work, outcome_var, group_vars, weighted,
                              include_ci, ci_method, round_digits) {
  agg <- .aggregate_pct_components(dt_work, outcome_var, group_vars)
  agg[, calculation_type := if (weighted) "weighted" else "unweighted"]

  agg <- .add_proportion(agg)
  agg <- .add_percentage(agg, round_digits)
  agg <- .add_n_eff(agg)
  if (include_ci) agg <- .add_ci(agg, weighted, ci_method, round_digits)

  agg
}

#' Build the "Total" summary row for a grouped result
#'
#' Reuses [.build_pct_result()] with `group_vars = NULL`, then overwrites
#' the (absent) group columns with `total_label` so the row can be
#' `rbindlist`-ed onto the grouped results.
#'
#' @inheritParams .build_pct_result
#' @param total_label Character. Value to use for each group column in the total row
#' @return A one-row data.table matching the shape of the grouped result
#' @keywords internal
.build_total_row <- function(dt_work, outcome_var, group_vars, weighted,
                             include_ci, ci_method, round_digits, total_label) {
  total_row <- .build_pct_result(dt_work, outcome_var, group_vars = NULL,
                                 weighted = weighted, include_ci = include_ci,
                                 ci_method = ci_method, round_digits = round_digits)
  for (v in group_vars) {
    total_row[, (v) := total_label]
  }
  total_row
}


# --- Public API ---------------------------------------------------------

#' Calculate Weighted or Unweighted Percentages by Group with 95% CIs
#'
#' Calculates the percentage of a binary outcome, overall or by one or more
#' grouping variables, with optional survey weighting and 95% confidence
#' intervals. Internally this is a thin orchestrator: it validates inputs,
#' prepares helper columns, then delegates to a handful of small
#' single-purpose functions (see the other roxygen blocks in this file) for
#' every calculation.
#'
#' @param dt A data.table containing the data
#' @param outcome_var Character string. Name of the binary outcome variable (0/1 or logical)
#' @param group_vars Character vector. Grouping variable names (optional). NULL calculates an overall percentage
#' @param weight_var Character string. Name of the weight variable (optional). NULL gives unweighted percentages
#' @param denominator_var Character string. Restricts the calculation to rows where this variable equals `denominator_value` (optional)
#' @param denominator_value Numeric. Value of `denominator_var` to include (default: 1)
#' @param na_treatment Character. "exclude" (default) drops missing outcomes from numerator and denominator; "as_zero" treats them as 0
#' @param round_digits Integer. Decimal places for percentage and CI bounds (default: 2)
#' @param include_diagnostics Logical. Include n_total/n_valid/n_eff etc. columns (default: TRUE)
#' @param include_ci Logical. Include 95% confidence interval columns (default: TRUE)
#' @param ci_method Character. "wilson" (default) or "normal", used only for unweighted CIs -- weighted CIs always use a design-adjusted normal approximation
#' @param include_total Logical. Append an overall "Total" row when `group_vars` is specified (default: FALSE)
#' @param total_label Character. Label used for the total row (default: "Total")
#'
#' @return A data.table with grouping variables, `percentage`,
#'   `calculation_type`, optional diagnostic columns (including effective
#'   sample size `n_eff`), and optional `ci_lower` / `ci_upper` columns.
#'
#' @details
#' ## Weighted percentage (weight_var supplied)
#' \deqn{Weighted \% = \frac{\sum(w \times I(outcome = 1))}{\sum(w)} \times 100}
#' Weighted CI: \deqn{SE = \sqrt{p(1-p) \sum(w^2) / (\sum w)^2}}
#'
#' ## Unweighted percentage (weight_var is NULL)
#' \deqn{Unweighted \% = \frac{Count(outcome = 1)}{Count(total)} \times 100}
#' Unweighted CI uses the Wilson score interval (default) or a normal approximation.
#'
#' `n_eff` is `sum(w)^2 / sum(w^2)` over the denominator population, which
#' reduces to the valid observation count when unweighted.
#'
#' @examples
#' calc_percentage_total_ci(dt, "ltp_cannabis", weight_var = "VEKT", denominator_var = "canpop")
#'
#' calc_percentage_total_ci(dt, "ltp_cannabis", "gender", weight_var = "VEKT",
#'                           denominator_var = "canpop", include_total = TRUE)
#'
#' @export
calc_percentage_total_ci <- function(dt,
                                     outcome_var,
                                     group_vars = NULL,
                                     weight_var = NULL,
                                     denominator_var = NULL,
                                     denominator_value = 1,
                                     na_treatment = c("exclude", "as_zero"),
                                     round_digits = 2,
                                     include_diagnostics = TRUE,
                                     include_ci = TRUE,
                                     ci_method = c("wilson", "normal"),
                                     include_total = FALSE,
                                     total_label = "Total") {

  na_treatment <- match.arg(na_treatment)
  ci_method <- match.arg(ci_method)

  .validate_pct_inputs(dt, outcome_var, group_vars, weight_var, denominator_var)

  dt_work <- copy(dt)
  dt_work <- .filter_to_denominator(dt_work, denominator_var, denominator_value)
  if (nrow(dt_work) == 0) {
    return(data.table())
  }

  weighted <- !is.null(weight_var)

  # Build and attach the normalized helper columns once; every downstream
  # calculation reads from these instead of re-deriving them.
  indicator <- .make_outcome_indicator(dt_work[[outcome_var]], na_treatment)
  weight <- .make_weight_vector(dt_work, weight_var)
  .attach_helper_columns(dt_work, indicator, weight)

  results <- .build_pct_result(dt_work, outcome_var, group_vars, weighted,
                               include_ci, ci_method, round_digits)
  results <- .drop_na_group_rows(results, group_vars)

  if (include_total && !is.null(group_vars)) {
    total_row <- .build_total_row(dt_work, outcome_var, group_vars, weighted,
                                  include_ci, ci_method, round_digits, total_label)
    results <- rbindlist(list(results, total_row), use.names = TRUE, fill = TRUE)
  }

  .select_output_columns(results, group_vars, weighted, include_diagnostics, include_ci)
}

#' Decide whether a weight variable is usable
#'
#' A weight variable is only usable if it exists, is numeric, and isn't
#' entirely NA. Kept separate from [calc_percentage_total_ci_auto()] so the
#' detection rule can be tested independently of any messaging/calculation.
#'
#' @param dt A data.table
#' @param weight_var Character or NULL
#' @return Logical, length 1
#' @keywords internal
.is_weight_usable <- function(dt, weight_var) {
  !is.null(weight_var) &&
    weight_var %in% names(dt) &&
     is.numeric(dt[[weight_var]]) &&
     !all(is.na(dt[[weight_var]]))
}

#' Calculate Percentage with Automatic Weight Detection
#'
#' Thin convenience wrapper around [calc_percentage_total_ci()] that checks
#' whether `weight_var` exists in `dt` and holds usable numeric values
#' before deciding whether to run a weighted or unweighted calculation.
#'
#' @inheritParams calc_percentage_total_ci
#' @param ... Additional arguments passed to [calc_percentage_total_ci()]
#' @return A data.table, as returned by [calc_percentage_total_ci()]
#'
#' @examples
#' calc_percentage_total_ci_auto(dt, "ltp_cannabis", "agecat", "VEKT",
#'                                denominator_var = "canpop", include_total = TRUE)
#'
#' @export
calc_percentage_total_ci_auto <- function(dt, outcome_var, group_vars = NULL,
                                          weight_var = NULL, ...) {
  usable <- .is_weight_usable(dt, weight_var)

  if (!is.null(weight_var) && !usable) {
    message("Weight variable '", weight_var, "' not found or invalid. Using unweighted calculation.")
  } else if (is.null(weight_var)) {
    message("No weight variable specified. Using unweighted calculation.")
  } else {
    message("Using weighted calculation with variable: ", weight_var)
  }

  calc_percentage_total_ci(dt, outcome_var, group_vars,
                           weight_var = if (usable) weight_var else NULL, ...)
}

# ---------------------------------------------------------------------------
# n_eff note: a separate `_with_neff` wrapper is no longer needed --
# .add_n_eff() runs unconditionally inside .build_pct_result(), and its
# formula is correct for both weighted and unweighted data (see that
# function's docs). Just leave include_diagnostics = TRUE (the default).
# ---------------------------------------------------------------------------
