## Hvor dataene for 2012 til 2025 ligger
## --------------------------------------------------
pathToir <- file.path(here::here("rapport", "rap2025"))
source(file.path(pathToir, "setup.R"))

# DT - data 2025 er lastet i setup.R

## ==================================
## Cannabis
## ----------------------------------
DT[can1 == 1, ltp_cannabis := 1] # Lifetime prevalence
DT[can6 == 1, lyp_cannabis := 1] # Last year prevalence
DT[can10 == 1, lmp_cannabis := 1] # Last month prevalence

canLtpAll <- calc_percentage_ci(dt = DT,
                                outcome_var = "ltp_cannabis",
                                weight_var = "vekt",
                                denominator_var = "canpop",
                                na_treatment = "as_zero",
                                round_digits = 1,
                                include_diagnostics = TRUE)

canLtpGender <- calc_percentage_ci(dt = DT,
                                   outcome_var = "ltp_cannabis",
                                   weight_var = "vekt",
                                   group_vars = "kjonn",
                                   denominator_var = "canpop",
                                   na_treatment = "as_zero",
                                   round_digits = 1,
                                   include_diagnostics = TRUE)

canLtpAge <- calc_percentage_ci(dt = DT,
                                outcome_var = "ltp_cannabis",
                                weight_var = "vekt",
                                group_vars = "agecat",
                                denominator_var = "canpop",
                                na_treatment = "as_zero",
                                round_digits = 1,
                                include_diagnostics = TRUE)


canLtpBoth <- calc_percentage_ci(dt = DT,
                                 outcome_var = "ltp_cannabis",
                                 weight_var = "vekt",
                                 group_vars = c("kjonn","agecat"),
                                 denominator_var = "canpop",
                                 na_treatment = "as_zero",
                                 round_digits = 1,
                                 include_diagnostics = TRUE)

setorder(canLtpBoth, "kjonn", "agecat")
canLtpBoth[]
