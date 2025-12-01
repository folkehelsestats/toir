
#' Calculate 3-Year Rolling Cannabis Prevalence by Gender
#'
#' This function computes yearly cases and denominators for cannabis prevalence,
#' grouped by selected variable e.g. (`kjonn`) and overall, and then calculates 3-year rolling sums
#' and percentages.
#'
#' @param dt A `data.table` containing the input data.
#' @param case_var Character string specifying the column name for cases (e.g., `"ltp_cannabis"`).
#' @param denom_var Character string specifying the column name for denominator (e.g., `"canpop"`).
#' @param by_var Character string specifying the column name for grouping by (e.g., `"kjonn"`).
#'
#' @return A list of three `data.table` objects when using `kjonn` as grouping variable:
#' \describe{
#'   \item{kjonn}{Cases and denominators by year and gender.}
#'   \item{menn}{Rolling 3-year summary for men (`kjonn == 1`).}
#'   \item{kvinner}{Rolling 3-year summary for women (`kjonn == 2`).}
#'   \item{all}{Rolling 3-year summary for all genders combined.}
#' }
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   year = rep(2010:2020, each = 2),
#'   kjonn = rep(1:2, times = 11),
#'   ltp_cannabis = sample(1:100, 22),
#'   canpop = sample(100:1000, 22)
#' )
#' result <- calc_cannabis_prevalence(dt, "ltp_cannabis", "canpop", "kjonn")
#' str(result)
#'
#' @export
calc_moving_prevalence <- function(dt, case_var, denom_var, by_var) {
  stopifnot(data.table::is.data.table(dt))

  # Aggregate by year and gender
  canltpKjonn <- dt[, .(
    cases = sum(get(case_var), na.rm = TRUE),
    denom = sum(get(denom_var), na.rm = TRUE)
  ), by = .(year, kjonn = get(by_var))][order(year)]

  # Function to compute rolling sums and percentage
  compute_rolling <- function(data) {
    data[, `:=`(
      cases_3yr = data.table::frollsum(cases, n = 3, align = "right"),
      denom_3yr = data.table::frollsum(denom, n = 3, align = "right")
    )]
    data[, pct_3yr := fifelse(denom_3yr > 0, 100 * cases_3yr / denom_3yr, NA_real_)]
    data.table::setkey(data, year)
    return(data)
  }

  # Men
  canltpMenn <- compute_rolling(canltpKjonn[kjonn == 1])

  # Women
  canltpKvinner <- compute_rolling(canltpKjonn[kjonn == 2])

  # All genders combined
  canltpAll <- dt[, .(
    cases = sum(get(case_var), na.rm = TRUE),
    denom = sum(get(denom_var), na.rm = TRUE)
  ), by = .(year)][order(year)]
  canltpAll <- compute_rolling(canltpAll)
  canltpAll[, kjonn := 0]

  ## return(list(kjonn = canltpKjonn, menn = canltpMenn, kvinner = canltpKvinner, all = canltpAll))
  return(data.table::rbindlist(list(
    canltpMenn,
    canltpKvinner,
    canltpAll
  ), use.names = TRUE, fill = TRUE))
}
