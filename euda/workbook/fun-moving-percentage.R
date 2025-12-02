#' @title Calculate Moving Prevalence
#'
#' Computes rolling prevalence (percentage) over a specified time window
#' for binary case and denominator variables, optionally grouped by one or more variables.
#'
#' @param dt A `data.table` containing the data.
#' @param case_var Character string. Name of the binary variable representing cases (1/0).
#' @param denom_var Character string. Name of the binary variable representing the denominator (1/0).
#' @param by_var Character vector or `NULL`. Optional grouping variable(s) (e.g., "kjonn", "age_group").
#' @param window Integer. Size of the rolling window (default = 3).
#'
#' @return A `data.table` with aggregated counts, rolling sums, and rolling percentage.
#' Columns returned:
#' \itemize{
#'   \item \code{year} - Year of observation.
#'   \item \code{cases} - Sum of cases per group/year.
#'   \item \code{denom} - Sum of denominator per group/year.
#'   \item \code{cases_roll} - Rolling sum of cases over the specified window.
#'   \item \code{denom_roll} - Rolling sum of denominator over the specified window.
#'   \item \code{pct_roll} - Rolling percentage (cases_roll / denom_roll * 100).
#' }
#'
#' @examples
#' library(data.table)
#' DT <- data.table(
#'   id = 1:100,
#'   year = sample(2015:2024, 100, replace = TRUE),
#'   case = rbinom(100, 1, 0.4),
#'   eligible = rbinom(100, 1, 0.8),
#'   kjonn = sample(c("M", "F"), 100, replace = TRUE),
#'   age_group = sample(c("18-29", "30-49", "50+"), 100, replace = TRUE)
#' )
#'
#' # Overall prevalence
#' calc_moving_prevalence(DT, "case", "eligible")
#'
#' # By gender
#' calc_moving_prevalence(DT, "case", "eligible", by_var = "kjonn")
#'
#' # By gender and age group
#' calc_moving_prevalence(DT, "case", "eligible", by_var = c("kjonn", "age_group"))
#'
#' @export
calc_moving_prevalence <- function(dt, case_var, denom_var, by_var = NULL, window = 3) {
  stopifnot(data.table::is.data.table(dt))

  # Build grouping columns dynamically
  group_cols <- c("year", by_var)

  # Aggregate by year and optional grouping variables
  agg <- dt[, .(
    cases = sum(get(case_var), na.rm = TRUE),
    denom = sum(get(denom_var), na.rm = TRUE)
  ), by = group_cols][order(year)]

  # Compute rolling sums and percentage within each group
  agg[, `:=`(
    cases_roll = data.table::frollsum(cases, n = window, align = "right"),
    denom_roll = data.table::frollsum(denom, n = window, align = "right")
  ), by = by_var]

  agg[, pct_roll := fifelse(denom_roll > 0, 100 * cases_roll / denom_roll, NA_real_)]

  return(agg)
}
