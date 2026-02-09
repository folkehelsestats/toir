## How many use sentralstimulerende rusmidler 


append_total_row <- function(DT,
                             factor_col = "kjonnSTR",
                             label = "Totalt",
                             weight_col = "sum_weights",
                             pct_col = "percentage",
                             sum_cols = c("n_total", "n_level", "sum_weights"),
                             keep_outcome_level = FALSE) {
  stopifnot(is.data.table(DT))

  # Ensure label exists in factor levels if factor
  if (is.factor(DT[[factor_col]])) {
    levels(DT[[factor_col]]) <- union(levels(DT[[factor_col]]), label)
  }

  # Compute sums
  sums <- DT[, lapply(.SD, sum, na.rm = TRUE), .SDcols = sum_cols]

  # Weighted mean for percentage (safe if weights are zero)
  w_total <- DT[[weight_col]]
  p_total <- DT[[pct_col]]
  wmean_pct <- if (!is.null(w_total) && sum(w_total, na.rm = TRUE) > 0) {
    sum(p_total * w_total, na.rm = TRUE) / sum(w_total, na.rm = TRUE)
  } else {
    NA_real_
  }

  # Build total row with correct column order
  tr <- as.list(rep(NA, ncol(DT)))
  names(tr) <- names(DT)

  tr[[factor_col]] <- if (is.factor(DT[[factor_col]])) factor(label, levels = levels(DT[[factor_col]])) else label
  if (!keep_outcome_level && "outcome_level" %in% names(DT)) tr[["outcome_level"]] <- NA_real_
  tr[[pct_col]] <- wmean_pct
  for (nm in sum_cols) tr[[nm]] <- sums[[nm]]

  # Clear CI by default
  for (nm in c("ci_lower","ci_upper")) if (nm %in% names(DT)) tr[[nm]] <- NA_real_

  total_row <- data.table::as.data.table(tr)
  data.table::rbindlist(list(DT, total_row), use.names = TRUE, fill = TRUE)
}


DTT[, ltp_sentral := fcase(ltp_cocaine == 1, 1,
                         ltp_mdma == 1, 1,
                         ltp_amphetamines == 1, 1)]

DTT[, ltpPop_sentral := fcase(ltpPop_kokain == 1, 1,
                         ltpPop_mdma == 1, 1,
                         ltpPop_amfetaminer == 1, 1)]


sentralDT <- calc_percentage_total_ci(
  DTT[year == 2025],
  outcome_var = "ltp_sentral",
  group_vars = "kjonnSTR",
  weight_var = "vekt",
  denominator_var = "ltpPop_sentral",
  na_treatment = "as_zero",
  round_digits = 1,
  include_diagnostics = TRUE,
  include_total = TRUE,
  total_label = "Alle"
)

## sentralDT <- append_total_row(sentralDT)
