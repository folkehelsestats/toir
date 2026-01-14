
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

dty <- DTT[alder <= 34]


calc_narko_gender <- function(data,
                              outcome = "lyp_cannabis",
                              group = c("year", "agecat"),
                              denominator = "canpop") {

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

lypDX <- calc_narko_gender(data = dty,
                             outcome = "lyp_cannabis",
                             group = c("year", "agecat"),
                             denominator = "canpop")

cannLypYng <- lypDX$all
cannLypYngMenn <- lypDX$menn
cannLypYngKvinner <- lypDX$kvinner

## Age groups as used by FHI, variable agecat3
dty <- group_age(DTT[alder <= 34], var = "alder",
                breaks = c(16, 20, 25, 30, 34),
                labels = c("16-20", "21-25", "26-30", "31-34"),
                new_var = "agecat3",
                right = TRUE)

lypDX3 <- calc_narko_gender(data = dty,
                             outcome = "lyp_cannabis",
                             group = c("year", "agecat3"),
                             denominator = "canpop")


## Rolling means
## ------------------

cannLypYng2 <- calc_percentage_ci2(dt = dty,
                                outcome_var = "lyp_cannabis",
                                weight_var = "vekt",
                                denominator_var = "canpop",
                                group_vars = c("year", "agecat"),
                                na_treatment = "as_zero",
                                round_digits = 1,
                                include_diagnostics = TRUE,
                                rolling_by = "year",
                                rolling_n = 3, # 3-year window
                                rolling_align = "right", # trailing
                                rolling_method = "sum_then_ratio" # recommended
                                )


data.table::setorder(cannLypYng2, year, agecat)

cannLypYngMenn2 <- calc_percentage_ci2(dt = dty[kjonn == 1],
                                    outcome_var = "lyp_cannabis",
                                    weight_var = "vekt",
                                    denominator_var = "canpop",
                                    group_vars = c("year", "agecat"),
                                    na_treatment = "as_zero",
                                    round_digits = 1,
                                    include_diagnostics = TRUE,
                                    rolling_by = "year",
                                    rolling_n = 3,
                                    rolling_align = "right", # trailing
                                    rolling_method = "sum_then_ratio" # recommended
                                    )

data.table::setorder(cannLypYngMenn2, year, agecat)

cannLypYngKvinner2 <- calc_percentage_ci2(dt = dty[kjonn == 2],
                                    outcome_var = "lyp_cannabis",
                                    weight_var = "vekt",
                                    denominator_var = "canpop",
                                    group_vars = c("year", "agecat"),
                                    na_treatment = "as_zero",
                                    round_digits = 1,
                                    include_diagnostics = TRUE,
                                    rolling_by = "year",
                                    rolling_n = 3,
                                    rolling_align = "right", # trailing
                                    rolling_method = "sum_then_ratio" # recommended
                                    )

data.table::setorder(cannLypYngKvinner2, year, agecat)

## ## rolling means
## cannLypYng2 <- calc_percentage_ci2(dt = dty,
##                                  outcome_var = "lyp_cannabis",
##                                  weight_var = NULL,
##                                  denominator_var = "canpop",
##                                  group_vars = c("year", "agecat"),
##                                  na_treatment = "as_zero",
##                                  round_digits = 1,
##                                  rolling_by  = "year",
##                                  rolling_n   = 3,                    # 3-year window
##                                  rolling_align = "right",            # trailing
##                                  rolling_method = "sum_then_ratio",   # recommended
##                                  include_diagnostics = TRUE)
