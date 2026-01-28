
yr <- 2025

## calc_percentage_flexible(
##   DTT[year == yr],
##   outcome_var = "ltp_cannabis",
##   group_vars = "kjonnSTR",
##   weight_var = "vekt",
##   na_treatment = "as_zero",
##   denominator_var = "canpop"
## )

canLtp <- calc_percentage_ci(
  DTT[year == yr],
  outcome_var = "ltp_cannabis",
  group_vars = "kjonnSTR",
  weight_var = "vekt",
  denominator_var = "canpop",
  na_treatment = "as_zero",
  round_digits = 1,
  include_diagnostics = TRUE
)

canLyp <- calc_percentage_ci(
  DTT[year == yr],
  outcome_var = "lyp_cannabis",
  group_vars = "kjonnSTR",
  weight_var = "vekt",
  denominator_var = "canpop",
  na_treatment = "as_zero",
  round_digits = 1,
  include_diagnostics = TRUE
)

canLmp <- calc_percentage_ci(
  DTT[year == yr],
  outcome_var = "lmp_cannabis",
  group_vars = "kjonnSTR",
  weight_var = "vekt",
  denominator_var = "canpop",
  na_treatment = "as_zero",
  round_digits = 1,
  include_diagnostics = TRUE
)

canDT <- rbindlist(list(
  canLtp[, periode := "Noen gang"],
  canLyp[, periode := "Siste 12 måneder"],
  canLmp[, periode := "Siste 4 uker"]
), use.names = TRUE, fill = TRUE)

## Yngre aldersgrupper
## -------------------------
ageMax <- 30

canLtpYng <- calc_percentage_ci(
  DTT[year == yr & alder <= ageMax],
  outcome_var = "ltp_cannabis",
  group_vars = "kjonnSTR",
  weight_var = "vekt",
  denominator_var = "canpop",
  na_treatment = "as_zero",
  round_digits = 1,
  include_diagnostics = TRUE
)

canLypYng <- calc_percentage_ci(
  DTT[year == yr & alder <= ageMax],
  outcome_var = "lyp_cannabis",
  group_vars = "kjonnSTR",
  weight_var = "vekt",
  denominator_var = "canpop",
  na_treatment = "as_zero",
  round_digits = 1,
  include_diagnostics = TRUE
)

canLmpYng <- calc_percentage_ci(
  DTT[year == yr & alder <= ageMax],
  outcome_var = "lmp_cannabis",
  group_vars = "kjonnSTR",
  weight_var = "vekt",
  denominator_var = "canpop",
  na_treatment = "as_zero",
  round_digits = 1,
  include_diagnostics = TRUE
)

canDTYng <- rbindlist(list(
  canLtpYng[, periode := "Noen gang"],
  canLypYng[, periode := "Siste 12 måneder"],
  canLmpYng[, periode := "Siste 4 uker"]
), use.names = TRUE, fill = TRUE)
