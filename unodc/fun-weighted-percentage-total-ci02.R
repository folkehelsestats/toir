#' @import data.table
NULL

# ---------------------------------------------------------------------------
# calc_percentage.R
#
# Refactor notes (read this if you're diffing against the old version):
#
# The original function branched on FOUR independent booleans:
#   weighted vs. unweighted  x  grouped vs. overall  x  diagnostics on/off
#   x  total-row on/off
# That's up to 2^4 near-duplicate data.table calls. Two tricks collapse all
# of that into a single aggregation call that is reused for the main result
# AND the total row:
#
#   1. `by = group_vars` works transparently when group_vars is NULL, so a
#      grouped and an "overall" calculation are literally the same call.
#
#   2. When no weight_var is supplied, we add a constant helper weight
#      column of 1. sum(w * indicator) / sum(w) then produces an unweighted
#      percentage automatically -- there is no need to special-case the
#      unweighted arithmetic anywhere, including in n_eff and the CI design
#      effect (sum(w^2) with w == 1 reduces to a plain count).
#
# The na_treatment handling ("exclude" vs "as_zero") is folded into a single
# indicator column built once, rather than duplicated inside every branch.
# ---------------------------------------------------------------------------

#' Build the 0/1/NA outcome indicator and a unified weight column
#'
#' Mutates `dt_work` in place (it is expected to already be a private copy)
#' and returns the name of the weight column to use downstream. This is the
#' piece that lets every calculation below stay agnostic to whether the user
#' supplied weights.
#'
#' @param dt_work data.table, modified in place
#' @param outcome_var character, name of the binary outcome column
#' @param weight_var character or NULL
#' @param na_treatment "exclude" or "as_zero"
#' @return character. Name of the weight column to use (either `weight_var`
#'   unchanged, or the name of the constant helper column that was added)
#' @keywords internal
.prep_pct_columns <- function(dt_work, outcome_var, weight_var, na_treatment) {

  # Outcome indicator: 1 if outcome == 1, 0 if outcome == 0, NA if missing.
  # "as_zero" immediately resolves NA -> 0; "exclude" leaves NA in place and
  # relies on na.rm / !is.na downstream so missing outcomes contribute to
  # neither numerator nor denominator.
  dt_work[, .pct_ind_ := fifelse(is.na(get(outcome_var)), NA_real_,
                                  fifelse(get(outcome_var) == 1, 1, 0))]
  if (na_treatment == "as_zero") {
    dt_work[is.na(.pct_ind_), .pct_ind_ := 0]
  }

  # Unified weight column: a constant 1 stands in for "no weighting" so that
  # every sum() below can be written once, weighted or not.
  if (is.null(weight_var)) {
    dt_work[, .pct_wt_ := 1]
    weight_col <- ".pct_wt_"
  } else {
    weight_col <- weight_var
  }

  weight_col
}

#' Core percentage/diagnostics aggregation (single code path)
#'
#' One data.table `[.data.table` call handles grouped or ungrouped, weighted
#' or unweighted data alike, because of the column prep done by
#' [.prep_pct_columns()]. This same function is reused verbatim to compute
#' both the per-group results and the optional "Total" row -- just call it
#' twice, once with `group_vars` and once with `group_vars = NULL`.
#'
#' @param dt_work data.table already prepped by .prep_pct_columns()
#' @param outcome_var character
#' @param group_vars character vector or NULL
#' @param weight_col character. Column returned by .prep_pct_columns()
#' @param weighted logical. Whether this is a genuinely weighted calculation
#'   (used only to tag `calculation_type`, not to change the arithmetic)
#' @return data.table with one row per group (or one row total)
#' @keywords internal
.calc_pct_core <- function(dt_work, outcome_var, group_vars, weight_col, weighted) {

  results <- dt_work[, .(
    # Numerator: weighted count of outcome == 1. NA indicator rows drop out
    # automatically (weight * NA = NA, na.rm = TRUE skips them) -- this is
    # what implements "exclude" without a separate branch.
    numerator = sum(get(weight_col) * .pct_ind_, na.rm = TRUE),

    # Denominator: weight summed only over rows with a non-missing
    # indicator. Under "as_zero" .pct_ind_ has no NAs, so this is simply the
    # full weight sum -- again, no separate branch needed.
    denominator = sum(get(weight_col)[!is.na(.pct_ind_)]),

    # Sum of squared weights over the same population as `denominator`.
    # Needed for the weighted CI design effect AND for n_eff. With
    # weight_col all 1's this reduces to a plain valid-row count, which is
    # exactly what n_eff should be in the unweighted case.
    sum_weights_squared = sum(get(weight_col)[!is.na(.pct_ind_)]^2),

    # Diagnostics, computed on the *original* outcome_var so they reflect
    # the raw group composition regardless of na_treatment.
    sum_weights  = sum(get(weight_col)),
    n_total      = .N,
    n_outcome_1  = sum(get(outcome_var) == 1, na.rm = TRUE),
    n_outcome_0  = sum(get(outcome_var) == 0, na.rm = TRUE),
    n_outcome_na = sum(is.na(get(outcome_var))),
    n_valid      = sum(!is.na(get(outcome_var)))
  ), by = group_vars]

  results[, calculation_type := if (weighted) "weighted" else "unweighted"]
  results[]
}

#' Attach 95% confidence intervals to a percentage result table
#'
#' Dispatches on `weighted` / `ci_method` but always writes to the same two
#' columns, so callers don't need their own if/else around this.
#'
#' @param results data.table with `proportion`, `denominator`,
#'   `sum_weights_squared` columns (as produced by [.calc_pct_core()])
#' @param weighted logical
#' @param ci_method "wilson" or "normal" (ignored when weighted == TRUE,
#'   which always uses the design-adjusted normal approximation)
#' @param round_digits integer
#' @return results, with ci_lower / ci_upper columns added (in place)
#' @keywords internal
.add_pct_ci <- function(results, weighted, ci_method, round_digits) {

  z <- qnorm(0.975)
  p <- results$proportion
  n <- results$denominator

  if (weighted) {
    # Design-adjusted normal approximation:
    # SE = sqrt(p(1-p) * sum(w^2) / sum(w)^2)
    se <- sqrt(p * (1 - p) * results$sum_weights_squared / (n^2))
    lo <- p - z * se
    hi <- p + z * se

  } else if (ci_method == "wilson") {
    # Wilson score interval -- better behaved than Wald near 0/1 or small n
    center <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
    margin <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / (1 + z^2 / n)
    lo <- center - margin
    hi <- center + margin

  } else {
    # Wald / normal approximation
    se <- sqrt(p * (1 - p) / n)
    lo <- p - z * se
    hi <- p + z * se
  }

  results[, ci_lower := round(pmax(0, lo) * 100, round_digits)]
  results[, ci_upper := round(pmin(1, hi) * 100, round_digits)]
  results[]
}

#' Calculate Weighted or Unweighted Percentages by Group with 95% CIs
#'
#' Calculates the percentage of a binary outcome, overall or by one or more
#' grouping variables, with optional survey weighting and 95% confidence
#' intervals. A single unified data.table aggregation handles all
#' combinations of grouped/overall, weighted/unweighted, and total-row
#' output -- see the comments in this file for how that unification works.
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
#' Weighted CI uses a design-adjusted normal approximation:
#' \deqn{SE = \sqrt{p(1-p) \sum(w^2) / (\sum w)^2}}
#'
#' ## Unweighted percentage (weight_var is NULL)
#' \deqn{Unweighted \% = \frac{Count(outcome = 1)}{Count(total)} \times 100}
#' Unweighted CI uses the Wilson score interval (default) or a normal
#' approximation.
#'
#' `n_eff`, the effective sample size, is `sum(w)^2 / sum(w^2)` over the
#' rows used in the denominator. For unweighted data this reduces exactly
#' to the valid observation count, so it is always safe to include.
#'
#' @examples
#' # Overall weighted percentage with CI
#' calc_percentage_total_ci(dt, "ltp_cannabis", weight_var = "VEKT", denominator_var = "canpop")
#'
#' # Weighted percentage by group, with CI and a total row
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

  # --- Input validation ---------------------------------------------------
  if (!data.table::is.data.table(dt)) {
    stop("Input 'dt' must be a data.table")
  }

  required_cols <- c(outcome_var, group_vars, weight_var, denominator_var)
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }

  # --- Restrict to the denominator population, if requested --------------
  dt_work <- copy(dt)
  if (!is.null(denominator_var)) {
    dt_work <- dt_work[get(denominator_var) == denominator_value]
    if (nrow(dt_work) == 0) {
      warning("No rows found where ", denominator_var, " == ", denominator_value)
      return(data.table())
    }
  }

  weighted <- !is.null(weight_var)

  # --- Prep columns once, then reuse the same core call for the grouped --
  # --- result and (optionally) the total row ------------------------------
  weight_col <- .prep_pct_columns(dt_work, outcome_var, weight_var, na_treatment)

  results <- .calc_pct_core(dt_work, outcome_var, group_vars, weight_col, weighted)
  results[, proportion := numerator / denominator]
  results[, percentage := round(proportion * 100, round_digits)]
  results[, n_eff := (sum_weights^2) / sum_weights_squared]
  if (include_ci) results <- .add_pct_ci(results, weighted, ci_method, round_digits)

  # Drop rows with an NA group level (mirrors original behaviour)
  if (!is.null(group_vars)) {
    for (v in group_vars) results <- results[!is.na(get(v))]
  }

  # --- Total row: literally the same core call with group_vars = NULL ----
  if (include_total && !is.null(group_vars)) {
    total_row <- .calc_pct_core(dt_work, outcome_var, NULL, weight_col, weighted)
    total_row[, proportion := numerator / denominator]
    total_row[, percentage := round(proportion * 100, round_digits)]
    total_row[, n_eff := (sum_weights^2) / sum_weights_squared]
    if (include_ci) total_row <- .add_pct_ci(total_row, weighted, ci_method, round_digits)
    for (v in group_vars) total_row[, (v) := total_label]

    results <- rbindlist(list(results, total_row), use.names = TRUE, fill = TRUE)
  }

  # --- Select / order final columns ---------------------------------------
  base_cols <- c("percentage", "calculation_type")
  if (include_diagnostics) {
    base_cols <- c(base_cols, "n_total", "n_valid", "n_outcome_1",
                    "n_outcome_0", "n_outcome_na", "n_eff")
    if (weighted) base_cols <- c(base_cols, "sum_weights")
  }
  if (include_ci) base_cols <- c(base_cols, "ci_lower", "ci_upper")

  final_cols <- c(group_vars, base_cols)
  results[, ..final_cols]
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
#' # Uses weighted calculation if VEKT exists and is numeric, unweighted otherwise
#' calc_percentage_total_ci_auto(dt, "ltp_cannabis", "agecat", "VEKT",
#'                                denominator_var = "canpop", include_total = TRUE)
#'
#' @export
calc_percentage_total_ci_auto <- function(dt, outcome_var, group_vars = NULL,
                                           weight_var = NULL, ...) {

  usable_weight <- !is.null(weight_var) &&
    weight_var %in% names(dt) &&
    is.numeric(dt[[weight_var]]) &&
    !all(is.na(dt[[weight_var]]))

  if (!is.null(weight_var) && !usable_weight) {
    message("Weight variable '", weight_var, "' not found or invalid. Using unweighted calculation.")
  } else if (is.null(weight_var)) {
    message("No weight variable specified. Using unweighted calculation.")
  } else {
    message("Using weighted calculation with variable: ", weight_var)
  }

  calc_percentage_total_ci(dt, outcome_var, group_vars,
                            weight_var = if (usable_weight) weight_var else NULL, ...)
}

# ---------------------------------------------------------------------------
# Note: a separate `_with_neff` wrapper is no longer needed. n_eff is now
# computed for free inside calc_percentage_total_ci() itself (see the
# `sum_weights` / `sum_weights_squared` columns produced by .calc_pct_core()),
# since the same formula -- sum(w)^2 / sum(w^2) -- correctly reduces to a
# plain valid-observation count in the unweighted case. Just set
# `include_diagnostics = TRUE` (the default) to get it.
# ---------------------------------------------------------------------------

# Example usage:
#
# result1 <- calc_percentage_total_ci(dt, "ltp_cannabis", "gender",
#                                      weight_var = "VEKT", denominator_var = "canpop",
#                                      include_total = TRUE)
#
# result2 <- calc_percentage_total_ci(dt, "ltp_cannabis", c("agecat", "gender"),
#                                      weight_var = "VEKT", denominator_var = "canpop",
#                                      include_total = TRUE, total_label = "All")
#
# result3 <- calc_percentage_total_ci(dt, "ltp_cannabis", "gender",
#                                      denominator_var = "canpop", include_total = TRUE)
#
# result4 <- calc_percentage_total_ci_auto(dt, "ltp_cannabis", "gender", "VEKT",
#                                           denominator_var = "canpop", include_total = TRUE)
