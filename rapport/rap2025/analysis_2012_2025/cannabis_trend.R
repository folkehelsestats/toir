
## ==================================
## Cannabis trend 2012 - 2025
## ----------------------------------

DTT[can1 == 1, ltp_cannabis := 1] # Lifetime prevalence
DTT[can6 == 1, lyp_cannabis := 1] # Last year prevalence
DTT[can10 == 1, lmp_cannabis := 1] # Last month prevalence

cannabis_trend <- function(data) {
    canLtpAllHist <- calc_percentage_ci(
        dt = data,
        outcome_var = "ltp_cannabis",
        weight_var = "vekt",
        denominator_var = "canpop",
        group_vars = "year",
        na_treatment = "as_zero",
        round_digits = 1,
        include_diagnostics = TRUE
    )

    canLypAllHist <- calc_percentage_ci(
        dt = data,
        outcome_var = "lyp_cannabis",
        weight_var = "vekt",
        denominator_var = "canpop",
        group_vars = "year",
        na_treatment = "as_zero",
        round_digits = 1,
        include_diagnostics = TRUE
    )

    canLmpAllHist <- calc_percentage_ci(
        dt = data,
        outcome_var = "lmp_cannabis",
        weight_var = "vekt",
        denominator_var = "canpop",
        group_vars = "year",
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

cannYng <- cannabis_trend(DTT[alder <= 34]) #16-34

## create_ci_graph(data = canLtpAllHist,
##                 x_col = "year",
##                 y_col = "percentage",
##                 lower_col = "ci_lower",
##                 upper_col = "ci_upper")

## make_hist(
##   d = cannAll,
##   x = "year",
##   y = "percentage",
##   n = "n_level",
##   group = "grp",
##   type = "line",
##   title = "Andel cannabis bruk i alderen 16-64, 2012-2025 ",
##   caption = "Tall om illegale rusmidler"
## )
