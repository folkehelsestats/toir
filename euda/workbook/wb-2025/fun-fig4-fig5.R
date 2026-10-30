
# Keep only relevant columns
keep_it <- function(dt){
  dt[, !c("ci_l", "ci_u"), with = FALSE]
}


create_dataset_fig4_fig5 <- function(ltp, lyp, lmp){
  ltp64 <- keep_it(ltp)
  lyp64 <- keep_it(lyp)
  lmp64 <- keep_it(lmp)

  # dd1664 <- ltp64[lyp64, on = "rolling_period"][lmp64, on = "rolling_period"]

  # Inner join: keep only IDs present in all tables
  dd1664 <- Reduce(
    function(x, y) merge(x, y, by = "rolling_period"),
    list(ltp64, lyp64, lmp64)
  )

  dd1664[, year := substring(rolling_period, 6)]
  dd1664[, rolling_period := NULL]
  data.table::setcolorder(dd1664, "year", before = "ltp_cannabis")

  # Convert to long format
  long_dd64 <- data.table::melt(
    dd1664,
    id.vars = "year",
    variable.name = "measure",
    value.name = "value"
  )

  # Define old-to-new labels
  labels <- c(
    ltp_cannabis = "Lifetime",
    lyp_cannabis = "Past year",
    lmp_cannabis = "Past month"
  )

  # Replace values using the lookup vector
  long_dd64[, measure := labels[as.character(measure)]]

  long_dd64[, year := as.integer(year)]

  # Keep only the last point from each series
  label_dt <- long_dd64[, .SD[which.max(year)], by = measure]
  label_dt[, x_lab := year + 0.4]

  # ---------------------------------------------------------
  # Create custom horizontal gridlines
  # ---------------------------------------------------------
  maxV <- max(long_dd64$value)+5
  grid_dt <- data.frame(
    y = seq(0, maxV, by = 5),
    x_start = min(long_dd64$year),
    x_end = max(long_dd64$year)
  )

  # Colors matching the example
  cols <- c(
    "Lifetime" = "#025169",
    "Past year" = "#ff7f0e",
    "Past month" = "#7C145C"
  )

  list(long_dt = long_dd64, label_dt = label_dt, grid_dt = grid_dt, cols = cols)
}
