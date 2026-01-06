## Hvor dataene for 2012 til 2025 ligger
## --------------------------------------------------
pathToir <- file.path(here::here("rapport", "rap2025"))
source(file.path(pathToir, "setup.R"))

## Output from setup.R
## ddt # data 2012-2024
## DT  # data 2025
## DTT # data 2012-2025


## ==================================
## Cannabis
## ----------------------------------

DTT[can1 == 1, ltp_cannabis := 1] # Lifetime prevalence
DTT[can6 == 1, lyp_cannabis := 1] # Last year prevalence
DTT[can10 == 1, lmp_cannabis := 1] # Last month prevalence

canLtpAllHist <- calc_percentage_ci(dt = DTT,
                                outcome_var = "ltp_cannabis",
                                weight_var = "vekt",
                                denominator_var = "canpop",
                                group_vars = "year",
                                na_treatment = "as_zero",
                                round_digits = 1,
                                include_diagnostics = TRUE)

canLypAllHist <- calc_percentage_ci(dt = DTT,
                                outcome_var = "lyp_cannabis",
                                weight_var = "vekt",
                                denominator_var = "canpop",
                                group_vars = "year",
                                na_treatment = "as_zero",
                                round_digits = 1,
                                include_diagnostics = TRUE)

canLmpAllHist <- calc_percentage_ci(dt = DTT,
                                outcome_var = "lmp_cannabis",
                                weight_var = "vekt",
                                denominator_var = "canpop",
                                group_vars = "year",
                                na_treatment = "as_zero",
                                round_digits = 1,
                                include_diagnostics = TRUE)

canLtpAllHist[, grp := "Noen gang"]
canLypAllHist[, grp := "Siste år"]
canLmpAllHist[, grp := "Siste måned"]

canPrev <- rbindlist(list(canLtpAllHist, canLypAllHist, canLmpAllHist), use.names = TRUE, fill = TRUE)

## create_ci_graph(data = canLtpAllHist,
##                 x_col = "year",
##                 y_col = "percentage",
##                 lower_col = "ci_lower",
##                 upper_col = "ci_upper")

make_hist(
  d = canPrev,
  x = "year",
  y = "percentage",
  n = "n_level",
  group = "grp",
  type = "line",
  title = "Cannabis prevalence 2012-2025"
)
