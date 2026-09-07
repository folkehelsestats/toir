#' @importFrom data.table data.table copy setorderv frollsum frollmean is.data.table
#' @importFrom stats qnorm
NULL

# -------------------------------------------------------------------------
# Validation helpers
# -------------------------------------------------------------------------

#' Validate inputs to calc_percentage_ci2()
#'
#' Checks that `dt` is a data.table, that all referenced columns exist, and
#' that the categorical arguments hold one of their allowed values. Stops
#' with an informative error on the first problem found.
#'
#' @param dt A data.table.
#' @param outcome_var Character. Name of the binary outcome column.
#' @param group_vars Character vector or NULL. Grouping column names.
#' @param weight_var Character or NULL. Name of the weight column.
#' @param denominator_var Character or NULL. Column used to filter the
#'   denominator population.
#' @param na_treatment Character. Either `"exclude"` or `"as_zero"`.
#' @param ci_method Character. Either `"wilson"` or `"normal"`.
#'
#' @return Invisibly `TRUE` if all checks pass; otherwise throws an error.
#' @keywords internal
.validate_ci_inputs <- function(dt, outcome_var, group_vars, weight_var,
                                 denominator_var, na_treatment, ci_method) {
  if (!data.table::is.data.table(dt)) {
    stop("Input 'dt' must be a data.table")
  }

  required_cols <- outcome_var
  if (!is.null(group_vars))      required_cols <- c(required_cols, group_vars)
  if (!is.null(weight_var))      required_cols <- c(required_cols, weight_var)
  if (!is.null(denominator_var)) required_cols <- c(required_cols, denominator_var)

  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  if (!na_treatment %in% c("exclude", "as_zero")) {
    stop("na_treatment must be 'exclude' or 'as_zero'")
  }

  if (!ci_method %in% c("wilson", "normal")) {
    stop("ci_method must be 'wilson' or 'normal'")
  }

  invisible(TRUE)
}

#' Validate rolling-window arguments
#'
#' Ensures that `rolling_by` is supplied and included in `group_vars`
#' whenever a rolling window (`rolling_n`) is requested.
#'
#' @param group_vars Character vector or NULL. Grouping column names.
#' @param rolling_by Character or NULL. Time column the rolling window is
#'   applied over (must be one of `group_vars`).
#' @param rolling_n Integer or NULL. Width of the rolling window; `NULL`
#'   disables rolling entirely.
#'
#' @return Invisibly `TRUE` if all checks pass; otherwise throws an error.
#' @keywords internal
.validate_rolling_inputs <- function(group_vars, rolling_by, rolling_n) {
  if (is.null(rolling_n)) {
    return(invisible(TRUE))
  }

  if (is.null(rolling_by)) {
    stop("When using rolling_n, please set rolling_by (e.g., 'year').")
  }

  if (is.null(group_vars) || !(rolling_by %in% group_vars)) {
    stop("rolling_by must be included in group_vars.")
  }

  invisible(TRUE)
}

# -------------------------------------------------------------------------
# Data preparation helpers
# -------------------------------------------------------------------------

#' Restrict rows to the requested denominator population
#'
#' Filters `dt` to rows where `denominator_var` equals `denominator_value`.
#' When no `denominator_var` is supplied, `dt` is returned unchanged.
#'
#' @param dt A data.table.
#' @param denominator_var Character or NULL. Column to filter on.
#' @param denominator_value Value that `denominator_var` must equal.
#'
#' @return A (possibly filtered) data.table. Emits a warning, but does not
#'   error, if the filter removes every row.
#' @keywords internal
.filter_denominator <- function(dt, denominator_var, denominator_value) {
  if (is.null(denominator_var)) {
    return(dt)
  }

  dt_filtered <- dt[get(denominator_var) == denominator_value]

  if (nrow(dt_filtered) == 0) {
    warning(paste("No rows found where", denominator_var, "==", denominator_value))
  }

  dt_filtered
}

#' Add a 0/1/NA outcome indicator column
#'
#' Derives `outcome_indicator` from `outcome_var` according to the requested
#' NA handling: `"exclude"` keeps missing outcomes as `NA` (to be excluded
#' downstream via `na.rm`), while `"as_zero"` recodes missing outcomes to 0.
#'
#' @param dt A data.table (modified in place).
#' @param outcome_var Character. Name of the binary outcome column.
#' @param na_treatment Character. Either `"exclude"` or `"as_zero"`.
#'
#' @return `dt`, with an added `outcome_indicator` column.
#' @keywords internal
.add_outcome_indicator <- function(dt, outcome_var, na_treatment) {
  if (na_treatment == "exclude") {
    dt[, outcome_indicator := ifelse(is.na(get(outcome_var)), NA,
                                      ifelse(get(outcome_var) == 1, 1, 0))]
  } else {
    dt[, outcome_indicator := ifelse(get(outcome_var) == 1, 1, 0)]
    dt[is.na(outcome_indicator), outcome_indicator := 0]
  }
  dt
}

# -------------------------------------------------------------------------
# Aggregation helpers
# -------------------------------------------------------------------------

#' Aggregate unweighted numerator/denominator counts
#'
#' Collapses `dt` (optionally by `group_vars`) into raw counts needed to
#' compute an unweighted percentage. `by = NULL` (the default when
#' `group_vars` is NULL) aggregates over the whole table.
#'
#' @param dt A data.table containing `outcome_indicator`.
#' @param group_vars Character vector or NULL. Grouping column names.
#' @param outcome_var Character. Name of the binary outcome column.
#' @param na_treatment Character. Either `"exclude"` or `"as_zero"`.
#' @param include_diagnostics Logical. Whether to add diagnostic count
#'   columns (`n_total`, `n_outcome_1`, ...) alongside numerator/denominator.
#'
#' @return A data.table with one row per group, containing `numerator`,
#'   `denominator`, `calculation_type`, and (optionally) diagnostic columns.
#' @keywords internal
.aggregate_unweighted <- function(dt, group_vars, outcome_var, na_treatment,
                                   include_diagnostics) {
  exclude <- na_treatment == "exclude"

  if (include_diagnostics) {
    dt[, .(
      numerator    = sum(outcome_indicator, na.rm = exclude),
      denominator  = if (exclude) sum(!is.na(outcome_indicator)) else .N,
      n_total      = .N,
      n_outcome_1  = sum(get(outcome_var) == 1, na.rm = TRUE),
      n_outcome_0  = sum(get(outcome_var) == 0, na.rm = TRUE),
      n_outcome_na = sum(is.na(get(outcome_var))),
      n_valid      = sum(!is.na(get(outcome_var))),
      calculation_type = "unweighted"
    ), by = group_vars]
  } else {
    dt[, .(
      numerator   = sum(outcome_indicator, na.rm = exclude),
      denominator = if (exclude) sum(!is.na(outcome_indicator)) else .N,
      calculation_type = "unweighted"
    ), by = group_vars]
  }
}

#' Aggregate weighted numerator/denominator counts
#'
#' Collapses `dt` (optionally by `group_vars`) into weighted sums needed to
#' compute a weighted percentage, including the sum of squared weights used
#' later for the weighted confidence interval.
#'
#' @param dt A data.table containing `outcome_indicator`.
#' @param group_vars Character vector or NULL. Grouping column names.
#' @param weight_var Character. Name of the weight column.
#' @param outcome_var Character. Name of the binary outcome column.
#' @param na_treatment Character. Either `"exclude"` or `"as_zero"`.
#' @param include_diagnostics Logical. Whether to add diagnostic count
#'   columns alongside numerator/denominator.
#'
#' @return A data.table with one row per group, containing `numerator`,
#'   `denominator`, `sum_weights_squared`, `calculation_type`, and
#'   (optionally) diagnostic columns including `sum_weights`.
#' @keywords internal
.aggregate_weighted <- function(dt, group_vars, weight_var, outcome_var,
                                 na_treatment, include_diagnostics) {
  exclude <- na_treatment == "exclude"

  if (include_diagnostics) {
    dt[, .(
      numerator   = sum(get(weight_var) * outcome_indicator, na.rm = exclude),
      denominator = if (exclude) sum(get(weight_var)[!is.na(outcome_indicator)]) else sum(get(weight_var)),
      sum_weights_squared = if (exclude) sum(get(weight_var)[!is.na(outcome_indicator)]^2) else sum(get(weight_var)^2),
      n_total     = .N,
      n_outcome_1 = sum(get(outcome_var) == 1, na.rm = TRUE),
      n_outcome_0 = sum(get(outcome_var) == 0, na.rm = TRUE),
      n_outcome_na= sum(is.na(get(outcome_var))),
      n_valid     = sum(!is.na(get(outcome_var))),
      sum_weights = sum(get(weight_var)),
      calculation_type = "weighted"
    ), by = group_vars]
  } else {
    dt[, .(
      numerator   = sum(get(weight_var) * outcome_indicator, na.rm = exclude),
      denominator = if (exclude) sum(get(weight_var)[!is.na(outcome_indicator)]) else sum(get(weight_var)),
      sum_weights_squared = if (exclude) sum(get(weight_var)[!is.na(outcome_indicator)]^2) else sum(get(weight_var)^2),
      calculation_type = "weighted"
    ), by = group_vars]
  }
}

#' Add percentage and proportion columns
#'
#' Derives `proportion` (numerator / denominator) and `percentage`
#' (proportion * 100) from an aggregated results table.
#'
#' @param results A data.table containing `numerator` and `denominator`.
#'
#' @return `results`, with `proportion` and `percentage` columns added.
#' @keywords internal
.add_percentage_proportion <- function(results) {
  results[, proportion := numerator / denominator]
  results[, percentage := proportion * 100]
  results
}

# -------------------------------------------------------------------------
# Effective sample size helpers
# -------------------------------------------------------------------------

#' Kish's effective sample size
#'
#' Calculates Kish's effective sample size:
#'
#' \deqn{n_{eff} = \frac{(\sum w)^2}{\sum w^2}}
#'
#' The effective sample size quantifies how much information survives
#' weighting: when all weights are equal, `n_eff` equals the raw sample
#' size; as weight variability grows, `n_eff` shrinks below it. Using
#' `n_eff` in place of the raw `n` in an ordinary (unweighted) confidence
#' interval formula is the standard "design effect" approximation for
#' weighted data when no full survey design (strata/clusters) is available.
#'
#' @param w_sum Numeric vector. Sum of weights per row.
#' @param w2_sum Numeric vector. Sum of squared weights per row.
#'
#' @return Numeric vector of effective sample sizes.
#' @keywords internal
.kish_n_eff <- function(w_sum, w2_sum) {
  w_sum^2 / w2_sum
}

#' Add effective sample size (Kish ESS) to a results table
#'
#' Adds an `n_eff` column computed from `denominator` (sum of weights) and
#' `sum_weights_squared`. See [.kish_n_eff()] for the formula and rationale.
#'
#' @param results A data.table containing `denominator` and
#'   `sum_weights_squared`.
#'
#' @return `results`, with an added `n_eff` column.
#' @keywords internal
.add_n_eff <- function(results) {
  results[, n_eff := .kish_n_eff(denominator, sum_weights_squared)]
  results
}

# -------------------------------------------------------------------------
# Confidence interval helpers
# -------------------------------------------------------------------------
#
# Each CI helper takes plain numeric vectors and returns a list(lower, upper)
# already expressed as a percentage (0-100 scale), so callers only need to
# round the result to the requested number of digits. Both helpers work for
# weighted data too: pass n_eff (see .add_n_eff()) instead of the raw n.

#' Wilson score confidence interval
#'
#' @param n Numeric vector. Effective denominator (sample size or sum of
#'   weights) per row.
#' @param p Numeric vector. Proportion (0-1) per row.
#' @param z Numeric. Critical value of the standard normal distribution
#'   (defaults to the 95\% two-sided value).
#'
#' @return A list with `lower` and `upper`, each a numeric vector on the
#'   0-100 percentage scale.
#' @keywords internal
.wilson_ci <- function(n, p, z = stats::qnorm(0.975)) {
  center <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  margin <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / (1 + z^2 / n)
  list(
    lower = pmax(0, center - margin) * 100,
    upper = pmin(1, center + margin) * 100
  )
}

#' Normal approximation confidence interval
#'
#' @inheritParams .wilson_ci
#'
#' @return A list with `lower` and `upper`, each a numeric vector on the
#'   0-100 percentage scale.
#' @keywords internal
.normal_ci <- function(n, p, z = stats::qnorm(0.975)) {
  se     <- sqrt(p * (1 - p) / n)
  margin <- z * se
  list(
    lower = pmax(0, p - margin) * 100,
    upper = pmin(1, p + margin) * 100
  )
}

#' Add confidence interval columns to a results table
#'
#' Computes `ci_lower` / `ci_upper` using the requested `ci_method`
#' (`"wilson"` or `"normal"`). For weighted data, the effective sample
#' size ([.add_n_eff()]) is used in place of the raw denominator, so that
#' weight variability is reflected in the interval width — see
#' [.add_n_eff()] for the rationale. This means weighted data can use a
#' Wilson interval too, not just the normal approximation.
#'
#' @param results A data.table containing `proportion`, `denominator`, and
#'   (for weighted data) `sum_weights_squared`.
#' @param weight_var Character or NULL. Name of the weight column; non-NULL
#'   routes the CI through the effective-sample-size adjustment.
#' @param ci_method Character. Either `"wilson"` or `"normal"`.
#' @param round_digits Integer. Number of decimal digits to round to.
#'
#' @return `results`, with `ci_lower` and `ci_upper` columns added (and,
#'   for weighted data, `n_eff`).
#' @keywords internal
.add_confidence_intervals <- function(results, weight_var, ci_method, round_digits) {
  n <- if (!is.null(weight_var)) {
    results <- .add_n_eff(results)
    results$n_eff
  } else {
    results$denominator
  }

  ci <- if (ci_method == "wilson") {
    .wilson_ci(n, results$proportion)
  } else {
    .normal_ci(n, results$proportion)
  }

  results[, ci_lower := round(ci$lower, round_digits)]
  results[, ci_upper := round(ci$upper, round_digits)]
  results
}

# -------------------------------------------------------------------------
# Rolling window helpers
# -------------------------------------------------------------------------

#' Order a results table for rolling-window calculations
#'
#' Coerces a character `rolling_by` column to integer (so that a rolling
#' window can be applied over it), then sorts the table by the non-time
#' grouping columns followed by `rolling_by`, ascending.
#'
#' @param results A data.table.
#' @param rolling_by Character. Time column the rolling window is applied
#'   over.
#' @param other_groups Character vector. Grouping columns other than
#'   `rolling_by`.
#'
#' @return `results`, coerced and sorted in place.
#' @keywords internal
.prepare_rolling_order <- function(results, rolling_by, other_groups) {
  if (is.character(results[[rolling_by]])) {
    suppressWarnings(results[, (rolling_by) := as.integer(get(rolling_by))])
  }
  data.table::setorderv(results, c(other_groups, rolling_by))
  results
}

#' Rolling window via summed numerator/denominator ("sum_then_ratio")
#'
#' Sums the numerator, denominator (and, for weighted data, the sum of
#' squared weights) over a rolling window of `rolling_n` periods per group,
#' then recomputes the rolling percentage and confidence interval from
#' those rolled sums.
#'
#' @param results A data.table, already ordered by [.prepare_rolling_order()].
#' @param other_groups Character vector. Grouping columns to roll within
#'   (i.e. excluding the time column).
#' @param rolling_n Integer. Width of the rolling window.
#' @param rolling_align Character. One of `"right"`, `"center"`, `"left"`.
#' @param weighted Logical. Whether the input data are weighted.
#' @param ci_method Character. Either `"wilson"` or `"normal"` (used only
#'   for unweighted data).
#' @param include_ci Logical. Whether to compute rolling confidence
#'   intervals.
#' @param round_digits Integer. Number of decimal digits to round to.
#'
#' @return `results`, with `numerator_roll`, `denominator_roll`,
#'   `percentage_roll` (and, if requested, `ci_lower_roll` / `ci_upper_roll`,
#'   plus `n_eff_roll` for weighted data) columns added.
#' @keywords internal
.roll_sum_then_ratio <- function(results, other_groups, rolling_n, rolling_align,
                                  weighted, ci_method, include_ci, round_digits) {
  roll_source_cols <- c("numerator", "denominator")
  if (weighted) roll_source_cols <- c(roll_source_cols, "sum_weights_squared")
  roll_target_cols <- paste0(roll_source_cols, "_roll")

  results[, (roll_target_cols) := lapply(.SD, data.table::frollsum,
                                          n = rolling_n, align = rolling_align),
           by = other_groups, .SDcols = roll_source_cols]

  results[, proportion_roll := numerator_roll / denominator_roll]
  results[, percentage_roll := round(proportion_roll * 100, round_digits)]

  if (include_ci) {
    n <- if (weighted) {
      results[, n_eff_roll := .kish_n_eff(denominator_roll, sum_weights_squared_roll)]
      results$n_eff_roll
    } else {
      results$denominator_roll
    }

    ci <- if (ci_method == "wilson") {
      .wilson_ci(n, results$proportion_roll)
    } else {
      .normal_ci(n, results$proportion_roll)
    }
    results[, ci_lower_roll := round(ci$lower, round_digits)]
    results[, ci_upper_roll := round(ci$upper, round_digits)]
  }

  results
}

#' Rolling window via averaged percentage ("mean_of_percent")
#'
#' Computes a simple rolling mean of the already-calculated `percentage`
#' column over a window of `rolling_n` periods per group. No confidence
#' interval is defined for this method.
#'
#' @param results A data.table, already ordered by [.prepare_rolling_order()],
#'   containing `percentage`.
#' @param other_groups Character vector. Grouping columns to roll within
#'   (i.e. excluding the time column).
#' @param rolling_n Integer. Width of the rolling window.
#' @param rolling_align Character. One of `"right"`, `"center"`, `"left"`.
#' @param round_digits Integer. Number of decimal digits to round to.
#'
#' @return `results`, with a `percentage_roll` column added.
#' @keywords internal
.roll_mean_of_percent <- function(results, other_groups, rolling_n, rolling_align,
                                   round_digits) {
  results[, percentage_roll := data.table::frollmean(percentage, n = rolling_n,
                                                       align = rolling_align),
           by = other_groups]
  results[, percentage_roll := round(percentage_roll, round_digits)]
  results
}

#' Apply a rolling window to an aggregated results table
#'
#' Orders the table and dispatches to the requested rolling method
#' (`"sum_then_ratio"` or `"mean_of_percent"`).
#'
#' @param results A data.table of aggregated (non-rolling) results.
#' @param group_vars Character vector. Grouping column names, including
#'   `rolling_by`.
#' @param weight_var Character or NULL. Name of the weight column.
#' @param rolling_by Character. Time column the rolling window is applied
#'   over.
#' @param rolling_n Integer. Width of the rolling window.
#' @param rolling_align Character. One of `"right"`, `"center"`, `"left"`.
#' @param rolling_method Character. Either `"sum_then_ratio"` or
#'   `"mean_of_percent"`.
#' @param ci_method Character. Either `"wilson"` or `"normal"`.
#' @param include_ci Logical. Whether to compute rolling confidence
#'   intervals.
#' @param round_digits Integer. Number of decimal digits to round to.
#'
#' @return `results`, with rolling columns added.
#' @keywords internal
.apply_rolling <- function(results, group_vars, weight_var, rolling_by, rolling_n,
                            rolling_align, rolling_method, ci_method, include_ci,
                            round_digits) {
  other_groups <- setdiff(group_vars, rolling_by)
  results <- .prepare_rolling_order(results, rolling_by, other_groups)

  if (rolling_method == "sum_then_ratio") {
    results <- .roll_sum_then_ratio(
      results, other_groups, rolling_n, rolling_align,
      weighted = !is.null(weight_var), ci_method, include_ci, round_digits
    )
  } else {
    results <- .roll_mean_of_percent(results, other_groups, rolling_n,
                                      rolling_align, round_digits)
  }

  results
}

# -------------------------------------------------------------------------
# Output helpers
# -------------------------------------------------------------------------

#' Drop rows with a missing grouping value
#'
#' Removes rows where any of `group_vars` is `NA`.
#'
#' @param results A data.table.
#' @param group_vars Character vector or NULL. Grouping column names.
#'
#' @return `results`, filtered to complete grouping values.
#' @keywords internal
.drop_na_groups <- function(results, group_vars) {
  if (is.null(group_vars)) {
    return(results)
  }
  for (var in group_vars) {
    results <- results[!is.na(get(var))]
  }
  results
}

#' Select and order the final output columns
#'
#' Builds the final column list from the requested options (diagnostics,
#' confidence intervals, rolling results) and subsets `results` to it.
#'
#' @param results A data.table containing all computed columns.
#' @param group_vars Character vector or NULL. Grouping column names.
#' @param weight_var Character or NULL. Name of the weight column.
#' @param include_diagnostics Logical. Whether diagnostic columns were
#'   requested.
#' @param include_ci Logical. Whether confidence interval columns were
#'   requested.
#' @param rolling_n Integer or NULL. Width of the rolling window; `NULL`
#'   means rolling columns are not included.
#' @param rolling_method Character. Either `"sum_then_ratio"` or
#'   `"mean_of_percent"`.
#'
#' @return A data.table subset to the final, ordered output columns.
#' @keywords internal
.select_output_columns <- function(results, group_vars, weight_var,
                                    include_diagnostics, include_ci,
                                    rolling_n, rolling_method) {
  base_cols <- c("percentage", "calculation_type")

  if (include_diagnostics) {
    base_cols <- c(base_cols, "n_total", "n_valid", "n_outcome_1",
                   "n_outcome_0", "n_outcome_na")
    if (!is.null(weight_var)) base_cols <- c(base_cols, "sum_weights")
  }

  if (include_ci) base_cols <- c(base_cols, "ci_lower", "ci_upper")

  if (!is.null(rolling_n)) {
    roll_cols <- "percentage_roll"
    if (rolling_method == "sum_then_ratio") {
      roll_cols <- c(roll_cols, "numerator_roll", "denominator_roll")
      if (!is.null(weight_var)) roll_cols <- c(roll_cols, "sum_weights_squared_roll")
      if (include_ci) roll_cols <- c(roll_cols, "ci_lower_roll", "ci_upper_roll")
    }
    base_cols <- c(base_cols, roll_cols)
  }

  final_cols <- if (is.null(group_vars)) base_cols else c(group_vars, base_cols)
  results[, ..final_cols]
}

# -------------------------------------------------------------------------
# Main function
# -------------------------------------------------------------------------

#' Calculate weighted or unweighted percentages with confidence intervals
#'
#' Computes a (optionally grouped, optionally weighted) percentage of a
#' binary outcome, together with a confidence interval and diagnostic
#' counts. Optionally also computes a rolling-window version of the same
#' percentage across a time dimension (e.g. a 3-year rolling average).
#'
#' @param dt A data.table containing the source data.
#' @param outcome_var Character. Name of the binary (0/1) outcome column.
#' @param group_vars Character vector or NULL. Column(s) to group by.
#'   Default `NULL` (no grouping).
#' @param weight_var Character or NULL. Name of a numeric weight column.
#'   When `NULL` (the default), an unweighted calculation is performed.
#' @param denominator_var Character or NULL. Column used to restrict the
#'   population before calculating the percentage (e.g. an eligibility
#'   flag). Default `NULL` (no restriction).
#' @param denominator_value Value that `denominator_var` must equal for a
#'   row to be included. Default `1`.
#' @param na_treatment Character. How missing outcomes are treated:
#'   `"exclude"` (default) drops them from both numerator and denominator;
#'   `"as_zero"` recodes them to 0.
#' @param round_digits Integer. Number of decimal digits used when rounding
#'   percentages and confidence interval bounds. Default `2`.
#' @param include_diagnostics Logical. Whether to include diagnostic count
#'   columns (`n_total`, `n_valid`, `n_outcome_1`, `n_outcome_0`,
#'   `n_outcome_na`, and, for weighted data, `sum_weights`). Default `TRUE`.
#' @param include_ci Logical. Whether to compute and include confidence
#'   interval columns (`ci_lower`, `ci_upper`). Default `TRUE`.
#' @param ci_method Character. Confidence interval formula for unweighted
#'   data: `"wilson"` (default) or `"normal"`. Weighted data always use a
#'   design-effect-based CI regardless of this setting.
#' @param rolling_by Character or NULL. Name of the time column (must be one
#'   of `group_vars`) that a rolling window is computed over. Default
#'   `NULL` (rolling disabled).
#' @param rolling_n Integer or NULL. Width of the rolling window (e.g. `3`
#'   for a 3-period window). Default `NULL` (rolling disabled).
#' @param rolling_align Character. Alignment of the rolling window:
#'   `"right"` (default), `"center"`, or `"left"`.
#' @param rolling_method Character. How the rolling percentage is derived:
#'   `"sum_then_ratio"` (default) sums numerator and denominator over the
#'   window and recomputes the ratio (and a matching rolling CI);
#'   `"mean_of_percent"` takes a simple rolling mean of the per-period
#'   percentages (no rolling CI is defined for this method).
#'
#' @return A data.table with one row per group (or one row overall, if
#'   `group_vars` is `NULL`), containing `percentage`, `calculation_type`,
#'   and, depending on the arguments above, diagnostic counts, confidence
#'   interval bounds, and rolling-window columns. Returns an empty
#'   data.table if `denominator_var` filtering removes all rows.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' dt <- data.table(
#'   year = rep(2018:2022, each = 100),
#'   region = sample(c("A", "B"), 500, replace = TRUE),
#'   outcome = sample(c(0, 1, NA), 500, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
#'   weight = runif(500, 0.5, 2)
#' )
#'
#' # Simple unweighted percentage by region
#' calc_percentage_ci2(dt, outcome_var = "outcome", group_vars = "region")
#'
#' # Weighted percentage with a 3-year rolling average by region
#' calc_percentage_ci2(
#'   dt,
#'   outcome_var = "outcome",
#'   group_vars = c("region", "year"),
#'   weight_var = "weight",
#'   rolling_by = "year",
#'   rolling_n = 3
#' )
#' }
#'
#' @export
calc_percentage_ci2 <- function(
  dt,
  outcome_var,
  group_vars = NULL,
  weight_var = NULL,
  denominator_var = NULL,
  denominator_value = 1,
  na_treatment = "exclude",
  round_digits = 2,
  include_diagnostics = TRUE,
  include_ci = TRUE,
  ci_method = "wilson",
  rolling_by = NULL,
  rolling_n = NULL,
  rolling_align = c("right", "center", "left"),
  rolling_method = c("sum_then_ratio", "mean_of_percent")
) {
  rolling_align  <- match.arg(rolling_align)
  rolling_method <- match.arg(rolling_method)

  .validate_ci_inputs(dt, outcome_var, group_vars, weight_var,
                       denominator_var, na_treatment, ci_method)
  .validate_rolling_inputs(group_vars, rolling_by, rolling_n)

  dt_work <- data.table::copy(dt)
  dt_work <- .filter_denominator(dt_work, denominator_var, denominator_value)
  if (nrow(dt_work) == 0) {
    return(data.table::data.table())
  }

  dt_work <- .add_outcome_indicator(dt_work, outcome_var, na_treatment)

  results <- if (is.null(weight_var)) {
    .aggregate_unweighted(dt_work, group_vars, outcome_var, na_treatment,
                           include_diagnostics)
  } else {
    .aggregate_weighted(dt_work, group_vars, weight_var, outcome_var,
                         na_treatment, include_diagnostics)
  }

  results <- .add_percentage_proportion(results)

  if (include_ci) {
    results <- .add_confidence_intervals(results, weight_var, ci_method, round_digits)
  }

  if (!is.null(rolling_n)) {
    results <- .apply_rolling(results, group_vars, weight_var, rolling_by,
                               rolling_n, rolling_align, rolling_method,
                               ci_method, include_ci, round_digits)
  }

  results[, percentage := round(percentage, round_digits)]
  results <- .drop_na_groups(results, group_vars)
  results <- .select_output_columns(results, group_vars, weight_var,
                                     include_diagnostics, include_ci,
                                     rolling_n, rolling_method)

  results
}
