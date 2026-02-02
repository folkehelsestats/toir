
## ==================================
## Cannabis trend 2012 - 2025
## ----------------------------------

## DTT[can1 == 1, ltp_cannabis := 1] # Lifetime prevalence
## DTT[can6 == 1, lyp_cannabis := 1] # Last year prevalence
## DTT[can10 == 1, lmp_cannabis := 1] # Last month prevalence

cannabis_trend <- function(data, group = "year") {
    canLtpAllHist <- calc_percentage_ci(
        dt = data,
        outcome_var = "ltp_cannabis",
        weight_var = "vekt",
        denominator_var = "ltpPop_cannabis",
        group_vars = group,
        na_treatment = "as_zero",
        round_digits = 1,
        include_diagnostics = TRUE
    )

    canLypAllHist <- calc_percentage_ci(
        dt = data,
        outcome_var = "lyp_cannabis",
        weight_var = "vekt",
        denominator_var = "lypPop_cannabis",
        group_vars = group,
        na_treatment = "as_zero",
        round_digits = 1,
        include_diagnostics = TRUE
    )

    canLmpAllHist <- calc_percentage_ci(
        dt = data,
        outcome_var = "lmp_cannabis",
        weight_var = "vekt",
        denominator_var = "lmpPop_cannabis",
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

cannYng <- cannabis_trend(DTT[alder <= 30]) #16-30
cannYngMenn <- cannabis_trend(DTT[alder <= 30 & kjonn == 1]) #16-30 og menn
cannYngKvinner <- cannabis_trend(DTT[alder <= 30 & kjonn == 2]) #16-30 og kvinner

## ==================================
## Age regoups for those below 30
## ---------------------------------

calc_narko_gender <- function(data,
                              outcome = "lyp_cannabis",
                              group = c("year", "agecat"),
                              denominator = "lypPop_cannabis") {

  cannLypYng <- calc_percentage_ci(dt = data,
                                   outcome_var = outcome,
                                   weight_var = "vekt",
                                   denominator_var = denominator,
                                   group_vars = group,
                                   na_treatment = "as_zero",
                                   round_digits = 1,
                                   include_diagnostics = TRUE)

  data.table::setorderv(cannLypYng, cols = group)


  cannLypYngMenn <- calc_percentage_ci(dt = data[kjonn == 1],
                                       outcome_var = outcome,
                                       weight_var = "vekt",
                                       denominator_var = denominator,,
                                       group_vars = group,,
                                       na_treatment = "as_zero",
                                       round_digits = 1,
                                       include_diagnostics = TRUE)

  data.table::setorderv(cannLypYngMenn, cols = group)

  cannLypYngKvinner <- calc_percentage_ci(dt = data[kjonn == 2],
                                          outcome_var = outcome,
                                          weight_var = "vekt",
                                          denominator_var = denominator,,
                                          group_vars = group,,
                                          na_treatment = "as_zero",
                                          round_digits = 1,
                                          include_diagnostics = TRUE)

  data.table::setorderv(cannLypYngKvinner, cols = group)

  return(list(all = cannLypYng,
              menn = cannLypYngMenn,
              kvinner = cannLypYngKvinner))

}


dty <- torr::group_age_standard(DTT[alder <= 30],
                                var = "alder",
                                type = "young30",
                                new_var = "ageYng")

lypDX <- calc_narko_gender(data = dty,
                           outcome = "lyp_cannabis",
                           group = c("year", "ageYng"),
                           denominator = "lypPop_cannabis")

cannLypYng <- lypDX$all
cannLypYngMenn <- lypDX$menn
cannLypYngKvinner <- lypDX$kvinner


## Rolling means
## ------------------

cannLypYng2 <- calc_percentage_ci2(dt = dty,
                                outcome_var = "lyp_cannabis",
                                weight_var = "vekt",
                                denominator_var = "lypPop_cannabis",
                                group_vars = c("year", "ageYng"),
                                na_treatment = "as_zero",
                                round_digits = 1,
                                include_diagnostics = TRUE,
                                rolling_by = "year",
                                rolling_n = 3, # 3-year window
                                rolling_align = "right", # trailing
                                rolling_method = "sum_then_ratio" # recommended
                                )


data.table::setorder(cannLypYng2, year, ageYng)

cannLypYngMenn2 <- calc_percentage_ci2(dt = dty[kjonn == 1],
                                    outcome_var = "lyp_cannabis",
                                    weight_var = "vekt",
                                    denominator_var = "lypPop_cannabis",
                                    group_vars = c("year", "ageYng"),
                                    na_treatment = "as_zero",
                                    round_digits = 1,
                                    include_diagnostics = TRUE,
                                    rolling_by = "year",
                                    rolling_n = 3,
                                    rolling_align = "right", # trailing
                                    rolling_method = "sum_then_ratio" # recommended
                                    )

data.table::setorder(cannLypYngMenn2, year, ageYng)

cannLypYngKvinner2 <- calc_percentage_ci2(dt = dty[kjonn == 2],
                                    outcome_var = "lyp_cannabis",
                                    weight_var = "vekt",
                                    denominator_var = "lypPop_cannabis",
                                    group_vars = c("year", "ageYng"),
                                    na_treatment = "as_zero",
                                    round_digits = 1,
                                    include_diagnostics = TRUE,
                                    rolling_by = "year",
                                    rolling_n = 3,
                                    rolling_align = "right", # trailing
                                    rolling_method = "sum_then_ratio" # recommended
                                    )

data.table::setorder(cannLypYngKvinner2, year, ageYng)
