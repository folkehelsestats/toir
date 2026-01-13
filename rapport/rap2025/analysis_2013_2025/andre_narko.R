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



calc_narko <- function(data, type = c("ltp", "lyp")) {
  vars <- c("cocaine", "mdma", "amphetamines", "heroin", "ghb", "lsd", "other")

  outcome_vars <- switch(type,
      "ltp" = paste0("ltp_", vars),
      "lyp" = paste0("lyp_", vars)
  )

  dx <- narko_trend(data, outcome_var = outcome_vars)
  out <- rbindlist(dx, use.names = TRUE, fill = TRUE)
  out[, grp := fifelse(type == "ltp", "Noen gang", "Siste 12 måneder")]
  out[, substance := gsub(paste0(type, "_"), "", substance)]

  return(out[])
}

## Noen gang
## ---------------------------------

ltpData <- calc_narko(DTT, type = "ltp")
ltpDataMenn <- calc_narko(DTT[kjonn == 1], type = "ltp")
ltpDataKvinner <- calc_narko(DTT[kjonn == 2], type = "ltp")

ltpDataYng <- calc_narko(DTT[agecat %in% c("16-24", "25-34")], type = "ltp")
ltpDataYngMenn <- calc_narko(DTT[agecat %in% c("16-24", "25-34") & kjonn == 1], type = "ltp")
ltpDataYngKvinner <- calc_narko(DTT[agecat %in% c("16-24", "25-34") & kjonn == 2], type = "ltp")




## Siste 12 måneder
## ---------------------------------

lypData <- calc_narko(DTT, type = "lyp")
lypDataMenn <- calc_narko(DTT[kjonn == 1], type = "lyp")
lypDataKvinner <- calc_narko(DTT[kjonn == 2], type = "lyp")

lypDataYng <- calc_narko(DTT[agecat %in% c("16-24", "25-34")], type = "lyp")
lypDataYngMenn <- calc_narko(DTT[agecat %in% c("16-24", "25-34") & kjonn == 1], type = "lyp")
lypDataYngKvinner <- calc_narko(DTT[agecat %in% c("16-24", "25-34") & kjonn == 2], type = "lyp")
