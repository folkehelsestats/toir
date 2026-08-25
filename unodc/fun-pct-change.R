calc_change <- function(dt, outcome_var, group_vars, denominator, digits = 1, diag = FALSE){
  x <- calc_percentage_total_ci(dt = dt,
                                outcome_var = outcome_var,
                                group_vars = group_vars,
                                weight_var = "nyvekt2",
                                denominator_var = denominator,
                                round_digits = digits,
                                na_treatment = "as_zero",
                                include_diagnostics = diag)

  yr2 <- dtx[, max(year)]
  yr1 <- dtx[, min(year)]
  
  new <- x[year == yr2]$percentage
  old <- x[year == yr1]$percentage

  pct <- (new - old)/old*100
  pct <- round(pct, digits = digits)

  message("Percent change from ", yr1, " to ", yr2)

  if (length(group_vars)>1){
    age <- unique(dt[["agecat"]])
    out <- paste0(age, ": ", pct, "%" )
  } else {
    out <- paste0(pct, "%")
  }

  list(x, out)
}

#Example
#-------
# calc_change(dtx, "lyp_any", "year", "anypop")
