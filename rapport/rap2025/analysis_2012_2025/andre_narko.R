## Andre stoffer og narkotika
## ---------------------------------

DTT[ans2_a == 1, ltp_cocaine := 1] #Cocaine-type drugs
DTT[ans2_b == 1, ltp_mdma := 1] #"Ecstasy" type substances
DTT[ans2_c == 1, ltp_amphetamines := 1] #Amphetamine-type stimulants
DTT[ans2_e == 1, ltp_heroin := 1] #Heroin
DTT[ans2_f == 1, ltp_ghb := 1] #Other sedatives and tranquillizers
DTT[ans2_g == 1, ltp_lsd := 1] #LSD
DTT[ans2_h == 1, ltp_other := 1] #Andre stoffer noen gang


DTT[ans3_1 == 1, lyp_cocaine := 1]
DTT[ans3_2 == 1, lyp_mdma := 1]
DTT[ans3_3 == 1, lyp_amphetamines := 1]
DTT[ans3_5 == 1, lyp_heroin := 1]
DTT[ans3_6 == 1, lyp_ghb := 1]
DTT[ans3_7 == 1, lyp_lsd := 1]
DTT[ans3_8 == 1, lyp_other := 1]



narko_trend <- function(dt, outcome_var, group_vars = "year") {

  if (length(outcome_var) != 1) {
    out <- vector("list", length(outcome_var))

    for (i in seq_along(outcome_var)) {
      out[[i]] <- calc_percentage_ci(
        dt = dt,
        outcome_var = outcome_var[i],
        weight_var = "vekt",
        denominator_var = "narkpop",
        group_vars = group_vars,
        na_treatment = "as_zero",
        round_digits = 1,
        include_diagnostics = TRUE
      )

      out[[i]][, substance := outcome_var[i]]
    }
  } else {

    out <- calc_percentage_ci(dt = dt,
                              outcome_var = outcome_var,
                              weight_var = "vekt",
                              denominator_var = "narkpop",
                              group_vars = c("year"),
                              na_treatment = "as_zero",
                              round_digits = 1,
                              include_diagnostics = TRUE)
  }

  return(out)
}

## Noen gang
## ---------------------------------

ltp_narko <- narko_trend(DTT, outcome_var = c("ltp_cocaine",
                                              "ltp_mdma",
                                              "ltp_amphetamines",
                                              "ltp_heroin",
                                              "ltp_ghb",
                                              "ltp_lsd",
                                              "ltp_other"))

ltpData <- rbindlist(ltp_narko, use.names = TRUE, fill = TRUE)
ltpData[, grp := "Noen gang"]
ltpData[, substance := gsub("ltp_", "", substance)]

## Siste 12 måneder
## ---------------------------------

lyp_narko <- narko_trend(DTT, outcome_var = c("lyp_cocaine",
                                              "lyp_mdma",
                                              "lyp_amphetamines",
                                              "lyp_heroin",
                                              "lyp_ghb",
                                              "lyp_lsd",
                                              "lyp_other"))

lypData <- rbindlist(lyp_narko, use.names = TRUE, fill = TRUE)
lypData[, grp := "Siste 12 måneder"]
lypData[, substance := gsub("lyp_", "", substance)]
