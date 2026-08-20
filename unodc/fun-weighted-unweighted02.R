library(data.table)

#' Calculate Weighted or Unweighted Percentages by Group or Overall
#'
#' Calculates the percentage of a binary outcome variable, either overall or
#' by one or more grouping variables, with optional weighting.
#'
#' @param dt A data.table containing the data
#' @param outcome_var Character. Name of the binary outcome variable (0/1 or logical)
#' @param group_vars Character vector. Grouping variable(s) (optional; NULL = overall)
#' @param weight_var Character. Name of the weight variable (optional; NULL = unweighted)
#' @param denominator_var Character. Variable defining the denominator population (optional)
#' @param denominator_value Value of denominator_var to include (default: 1)
#' @param na_treatment "exclude" (default, drop NA outcomes from denominator) or
#'   "as_zero" (treat NA outcomes as 0)
#' @param round_digits Integer. Decimal places for the percentage (default: 2)
#' @param include_diagnostics Logical. Include n_total/n_valid/etc. columns (default: TRUE)
#'
#' @return A data.table with grouping variables, percentage, and optional diagnostics
#'
#' @details
#' Weighted %   = sum(weight * I(outcome == 1)) / sum(weight) * 100
#' Unweighted % = count(outcome == 1) / count(total) * 100
#'
#' Internally, "unweighted" is just the weighted formula with weight = 1, so both
#' cases share one code path.
#'
#' @examples
#' calc_percentage(dt, "ltp_cannabis", weight_var = "VEKT", denominator_var = "canpop")
#' calc_percentage(dt, "ltp_cannabis", c("agecat", "gender"), denominator_var = "canpop")
#'
#' @export
calc_percentage <- function(dt,
                            outcome_var,
                            group_vars = NULL,
                            weight_var = NULL,
                            denominator_var = NULL,
                            denominator_value = 1,
                            na_treatment = c("exclude", "as_zero"),
                            round_digits = 2,
                            include_diagnostics = TRUE) {

  na_treatment <- match.arg(na_treatment)

  # ---- 1. Validate inputs --------------------------------------------------
  if (!is.data.table(dt)) stop("Input 'dt' must be a data.table")

  required_cols <- c(outcome_var, group_vars, weight_var, denominator_var)
  missing_cols  <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }

  # ---- 2. Prepare working copy ----------------------------------------------
  dt_work <- data.table::copy(dt)

  # Restrict to the denominator population, if requested
  if (!is.null(denominator_var)) {
    dt_work <- dt_work[get(denominator_var) == denominator_value]
    if (nrow(dt_work) == 0) {
      warning("No rows found where ", denominator_var, " == ", denominator_value)
      return(data.table())
    }
  }

  # Unify weighted/unweighted: an unweighted calc is just weight = 1 everywhere.
  # This lets the aggregation below use one formula for both cases.
  dt_work[, .weight := if (is.null(weight_var)) 1 else get(weight_var)]

  # Outcome indicator, handling NA per na_treatment:
  #   "exclude": NA stays NA (dropped from both numerator and denominator)
  #   "as_zero": NA becomes 0 (counted in the denominator as a non-event)
  dt_work[, .outcome_ind := data.table::fifelse(get(outcome_var) == 1, 1, 0, na = NA_real_)]
  if (na_treatment == "as_zero") {
    dt_work[is.na(.outcome_ind), .outcome_ind := 0]
  }

  # Rows that count toward the denominator (all rows for "as_zero",
  # only non-NA outcome rows for "exclude")
  dt_work[, .valid := !is.na(.outcome_ind)]

  # ---- 3. Single aggregation (handles grouped/ungrouped automatically) -----
  # data.table treats by = NULL as "no grouping", so this one call covers
  # the overall case and every grouped case without duplicating code.
  results <- dt_work[, .(
    numerator     = sum(.weight * .outcome_ind, na.rm = TRUE),
    denominator   = sum(.weight * .valid),
    n_total       = .N,
    n_valid       = sum(.valid),
    n_outcome_1   = sum(get(outcome_var) == 1, na.rm = TRUE),
    n_outcome_0   = sum(get(outcome_var) == 0, na.rm = TRUE),
    n_outcome_na  = sum(is.na(get(outcome_var))),
    sum_weights   = sum(.weight)
  ), by = group_vars]

  # ---- 4. Percentage ---------------------------------------------------------
  results[, percentage := round((numerator / denominator) * 100, round_digits)]
  results[, calculation_type := if (is.null(weight_var)) "unweighted" else "weighted"]

  # Drop rows with an NA grouping value (mirrors original behaviour)
  for (var in group_vars) results <- results[!is.na(get(var))]

  # ---- 5. Select final columns ----------------------------------------------
  diag_cols <- c("n_total", "n_valid", "n_outcome_1", "n_outcome_0", "n_outcome_na")
  if (!is.null(weight_var) && include_diagnostics) diag_cols <- c(diag_cols, "sum_weights")

  final_cols <- c(group_vars, "percentage", "calculation_type",
                   if (include_diagnostics) diag_cols)

  results[, ..final_cols]
}

#' Calculate Percentage with Automatic Weight Detection
#'
#' Uses \code{weight_var} if it exists in \code{dt} and holds usable numeric
#' values; otherwise falls back to an unweighted calculation.
#'
#' @inheritParams calc_percentage
#' @param ... Additional arguments passed to \code{\link{calc_percentage}}
#' @export
calc_percentage_auto <- function(dt, outcome_var, group_vars, weight_var = NULL, ...) {

  weight_ok <- !is.null(weight_var) &&
    weight_var %in% names(dt) &&
    is.numeric(dt[[weight_var]]) &&
    !all(is.na(dt[[weight_var]]))

  message(
    if (weight_ok) paste("Using weighted calculation with variable:", weight_var)
    else if (!is.null(weight_var)) paste("Weight variable", weight_var, "not found or invalid. Using unweighted calculation.")
    else "No weight variable specified. Using unweighted calculation."
  )

  calc_percentage(dt, outcome_var, group_vars,
                   weight_var = if (weight_ok) weight_var else NULL, ...)
}
