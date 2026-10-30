add_col <- function(dt, type, col, stoff, tot = FALSE) {

  dt <- copy(dt)

  dt[, `:=`(
    type = type,
    value = get(col),
    nark = stoff
  )]

  dt[, (col) := NULL]

  if (tot) {
    dt[, kjonn := 3L]
  }

  return(dt)
}


library(data.table)

add_narko <- function(dt,
                      pop_col = "ltpPop_cocaine",
                      value_col = "ltp_cocaine",
                      stoff = "Cocaine",
                      typ = "LTP") {

  # Gender-specific prevalence
  ltp_kjo <- torr::calc_prevalence(
    dt,
    pop_col,
    "year",
    value_col,
    weight_var = "vekt2",
    by = "kjonn",
    rolling_year = 3
  )

  # Total prevalence
  ltp_tot <- torr::calc_prevalence(
    dt,
    pop_col,
    "year",
    value_col,
    weight_var = "vekt2",
    rolling_year = 3
  )

  # Add columns
  ltp_kjo <- add_col(ltp_kjo, typ, value_col, stoff)
  ltp_tot <- add_col(ltp_tot, typ, value_col, stoff, tot = TRUE)

  # Combine datasets
  return(rbindlist(list(ltp_kjo, ltp_tot), fill = TRUE))
}
