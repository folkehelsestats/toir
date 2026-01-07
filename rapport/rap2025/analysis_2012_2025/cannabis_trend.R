
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
    canLypAllHist[, grp := "Siste år"]
    canLmpAllHist[, grp := "Siste måned"]

    dx <- data.table::rbindlist(list(canLtpAllHist, canLypAllHist, canLmpAllHist), use.names = TRUE, fill = TRUE)

    return(dx)
}

cannAll <- cannabis_trend(DTT) #16-64 år
cannAllMenn <- cannabis_trend(DTT[kjonn == 1]) # 16-64 år og menn
cannAllKvinner <- cannabis_trend(DTT[kjonn == 2]) #16-64 år og kvinner

cannYng <- cannabis_trend(DTT[alder <= 34]) #16-34
cannYngMenn <- cannabis_trend(DTT[alder <= 34 & kjonn == 1]) #16-34 og menn
cannYngKvinner <- cannabis_trend(DTT[alder <= 34 & kjonn == 2]) #16-34 og kvinner
