
gender_prop <- function(DTT, nominator, denominator,
                        yr = 2025,
                        agemax = NULL,
                        type = c("ltp", "lyp")){

  nom <- paste0(type, "_", nominator)
  dom <- paste0(type, "Pop_", denominator)

  if (is.null(agemax)){
    DTT <- DTT[year == yr]
  } else {
    DTT <- DTT[year == yr & alder <= agemax]
  }

ltp <- calc_percentage_ci(
  DTT[year == yr],
  outcome_var = nom[1],
  group_vars = "kjonnSTR",
  weight_var = "vekt",
  denominator_var = dom[1],
  na_treatment = "as_zero",
  round_digits = 1,
  include_diagnostics = TRUE
)

outLtp <- append_total_row(ltp)

lyp <- calc_percentage_ci(
  DTT[year == yr],
  outcome_var = nom[2],
  group_vars = "kjonnSTR",
  weight_var = "vekt",
  denominator_var = dom[2],
  na_treatment = "as_zero",
  round_digits = 1,
  include_diagnostics = TRUE
)

outLyp <- append_total_row(lyp)

  out <- rbindlist(list(
  outLtp[, periode := "Noen gang"],
  outLyp[, periode := "Siste 12 måneder"]
), use.names = TRUE, fill = TRUE)

out[, stuff := denominator]

return(out[])
}


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


kokainDT <- gender_prop(DTT, "cocaine", "kokain")
mdmaDT <- gender_prop(DTT, "mdma", "mdma")
ampheDT <- gender_prop(DTT, "amphetamines", "amfetaminer")

andreDT <- data.table::rbindlist(list(kokainDT, mdmaDT, ampheDT))
andreDT[, percentage := round(percentage, 1)]

### ---- Yngre <=30
kokainDTyng <- gender_prop(DTT, "cocaine", "kokain", agemax = 30)





## yr <- 2025

## kokainLtp <- calc_percentage_ci(
##   DTT[year == yr],
##   outcome_var = "ltp_cocaine",
##   group_vars = "kjonnSTR",
##   weight_var = "vekt",
##   denominator_var = "ltpPop_kokain",
##   na_treatment = "as_zero",
##   round_digits = 1,
##   include_diagnostics = TRUE
## )

## kokainLyp <- calc_percentage_ci(
##   DTT[year == yr],
##   outcome_var = "lyp_cocaine",
##   group_vars = "kjonnSTR",
##   weight_var = "vekt",
##   denominator_var = "lypPop_kokain",
##   na_treatment = "as_zero",
##   round_digits = 1,
##   include_diagnostics = TRUE
## )

## kokainDT <- rbindlist(list(
##   kokainLtp[, periode := "Noen gang"],
##   kokainLyp[, periode := "Siste 12 måneder"]
## ), use.names = TRUE, fill = TRUE)

## ## Yngre aldersgrupper
## ## -------------------------
## ageMax <- 30

## kokainLtpYng <- calc_percentage_ci(
##   DTT[year == yr & alder <= ageMax],
##   outcome_var = "ltp_cocaine",
##   group_vars = "kjonnSTR",
##   weight_var = "vekt",
##   denominator_var = "ltpPop_kokain",
##   na_treatment = "as_zero",
##   round_digits = 1,
##   include_diagnostics = TRUE
## )

## kokainLypYng <- calc_percentage_ci(
##   DTT[year == yr & alder <= ageMax],
##   outcome_var = "lyp_cocaine",
##   group_vars = "kjonnSTR",
##   weight_var = "vekt",
##   denominator_var = "lypPop_kokain",
##   na_treatment = "as_zero",
##   round_digits = 1,
##   include_diagnostics = TRUE
## )

## kokainDTYng <- rbindlist(list(
##   kokainLtpYng[, periode := "Noen gang"],
##   kokainLypYng[, periode := "Siste 12 måneder"]
## ), use.names = TRUE, fill = TRUE)
