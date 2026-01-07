
## ==================================
## Cannabis trend 2012 - 2025
## ----------------------------------

DTT[can1 == 1, ltp_cannabis := 1] # Lifetime prevalence
DTT[can6 == 1, lyp_cannabis := 1] # Last year prevalence
DTT[can10 == 1, lmp_cannabis := 1] # Last month prevalence

cannabis_trend <- function(data, group = "year") {
    canLtpAllHist <- calc_percentage_ci(
        dt = data,
        outcome_var = "ltp_cannabis",
        weight_var = "vekt",
        denominator_var = "canpop",
        group_vars = group,
        na_treatment = "as_zero",
        round_digits = 1,
        include_diagnostics = TRUE
    )

    canLypAllHist <- calc_percentage_ci(
        dt = data,
        outcome_var = "lyp_cannabis",
        weight_var = "vekt",
        denominator_var = "canpop",
        group_vars = group,
        na_treatment = "as_zero",
        round_digits = 1,
        include_diagnostics = TRUE
    )

    canLmpAllHist <- calc_percentage_ci(
        dt = data,
        outcome_var = "lmp_cannabis",
        weight_var = "vekt",
        denominator_var = "canpop",
        group_vars = group,
        na_treatment = "as_zero",
        round_digits = 1,
        include_diagnostics = TRUE
    )

    canLtpAllHist[, grp := "Noen gang"]
    canLypAllHist[, grp := "Siste 12 måneder"]
    canLmpAllHist[, grp := "Siste 4 uker"]

    dx <- data.table::rbindlist(list(canLtpAllHist, canLypAllHist, canLmpAllHist), use.names = TRUE, fill = TRUE)

    return(dx)
}

cannAll <- cannabis_trend(DTT) #16-64 år
cannAllMenn <- cannabis_trend(DTT[kjonn == 1]) # 16-64 år og menn
cannAllKvinner <- cannabis_trend(DTT[kjonn == 2]) #16-64 år og kvinner

cannYng <- cannabis_trend(DTT[alder <= 34]) #16-34
cannYngMenn <- cannabis_trend(DTT[alder <= 34 & kjonn == 1]) #16-34 og menn
cannYngKvinner <- cannabis_trend(DTT[alder <= 34 & kjonn == 2]) #16-34 og kvinner

## ==================================
## Age regoups for those below 35
## ---------------------------------

dty <- group_age(DTT[alder <= 34], var = "alder",
                breaks = c(16, 20, 25, 30, 34),
                labels = c("16-20", "21-25", "26-30", "31-34"),
                new_var = "agecat3",
                right = TRUE)

cannLypYng <- calc_percentage_ci(dt = dty,
                                outcome_var = "lyp_cannabis",
                                weight_var = "vekt",
                                denominator_var = "canpop",
                                group_vars = c("year", "agecat3"),
                                na_treatment = "as_zero",
                                round_digits = 1,
                                include_diagnostics = TRUE)

data.table::setorder(cannLypYng, year, agecat3)

## ## rolling means
## cannLypYng2 <- calc_percentage_ci2(dt = dty,
##                                  outcome_var = "lyp_cannabis",
##                                  weight_var = NULL,
##                                  denominator_var = "canpop",
##                                  group_vars = c("year", "agecat3"),
##                                  na_treatment = "as_zero",
##                                  round_digits = 1,
##                                  rolling_by  = "year",
##                                  rolling_n   = 3,                    # 3-year window
##                                  rolling_align = "right",            # trailing
##                                  rolling_method = "sum_then_ratio",   # recommended
##                                  include_diagnostics = TRUE)

cannLypYngMenn <- calc_percentage_ci(dt = dty[kjonn == 1],
                                    outcome_var = "lyp_cannabis",
                                    weight_var = "vekt",
                                    denominator_var = "canpop",
                                    group_vars = c("year", "agecat3"),
                                    na_treatment = "as_zero",
                                    round_digits = 1,
                                    include_diagnostics = TRUE)

data.table::setorder(cannLypYngMenn, year, agecat3)

cannLypYngKvinner <- calc_percentage_ci(dt = dty[kjonn == 2],
                                    outcome_var = "lyp_cannabis",
                                    weight_var = "vekt",
                                    denominator_var = "canpop",
                                    group_vars = c("year", "agecat3"),
                                    na_treatment = "as_zero",
                                    round_digits = 1,
                                    include_diagnostics = TRUE)

data.table::setorder(cannLypYngKvinner, year, agecat3)
