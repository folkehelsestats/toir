
calc_change <- function(dt, outcome_var, group_vars, denominator, digits = 1, diag = FALSE){
  x <- calc_percentage(dt = dt,
                       outcome_var = outcome_var,
                       group_vars = group_vars,
                       weight_var = "nyvekt2",
                       denominator_var = denominator,
                       round_digits = digits,
                       na_treatment = "as_zero",
                       include_diagnostics = diag)

  pct <- ((x[year == 2024]$percentage - x[year == 2023]$percentage)/x[year == 2023]$percentage) * 100
  pct <- round(pct, digits = digits)

  list(x, paste0("Pct change:", pct, "%"))
}
