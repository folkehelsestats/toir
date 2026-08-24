
## This function is used to match the reporting style from UNODC
## Function calc_percentage() from fun-weighted-unweighted.R

no = "ltp_cannabis"
de = "canpop"
weight_var = "vekt"
group_vars = "kjonn"

## no - nominator
## de - denominator
## weight_var NULL if unweighted
get_prev <- function(dt, no, de,
                     weight_var ="vekt",
                     kjonnkb = kjonnKB,
                     diagnostic = FALSE){

  tot <- calc_percentage(dt=dt,
                         outcome_var = no,
                         weight_var = weight_var,
                         denominator_var = de,
                         na_treatment = "as_zero",
                         round_digits = 1,
                         include_diagnostics = diagnostic)

  kjonn <- calc_percentage(dt=dt,
                           outcome_var = no,
                           group_vars = "kjonn",
                           weight_var = weight_var,
                           denominator_var = de,
                           na_treatment = "as_zero",
                           round_digits = 1,
                           include_diagnostics = diagnostic)

  kjonn[kjonnkb, on = c(kjonn = "v1"), gender := i.v2]
  data.table::setcolorder(kjonn, "gender", "percentage", skip_absent =  T)

  alder <- calc_percentage(dt=dt,
                           outcome_var = no,
                           group_vars = "agecat",
                           weight_var = weight_var,
                           denominator_var = de,
                           na_treatment = "as_zero",
                           round_digits = 1,
                           include_diagnostics = diagnostic)

  data.table::setorder(alder, "agecat")

  begge <- calc_percentage(dt=dt,
                           outcome_var = no,
                           group_vars = c("kjonn", "agecat"),
                           weight_var = weight_var,
                           denominator_var = de,
                           na_treatment = "as_zero",
                           round_digits = 1,
                           include_diagnostics = diagnostic)

  begge[kjonnkb, on = c(kjonn = "v1"), gender := i.v2]
  data.table::setorder(begge, "kjonn", "agecat")
  data.table::setcolorder(begge, "gender", "agecat", skip_absent = T)

  list(total = tot, kjonn = kjonn, alder = alder, begge = begge)
}

## Naive function
## -------------
## no - nominator
## de - denominator
cal_prev <- function(dt, no, de){
  tot <- dt[, round(sum(no, na.rm = T)/sum(de, na.rm = T)*100, digits = 1), env = list(no = no, de = de)]
  kjonn <- dt[, round(sum(no, na.rm = TRUE)/sum(de, na.rm = T)*100, digits = 1), keyby = kjonn, env = list(no = no, de = de)]
  alder <- dt[, round(sum(no, na.rm = T)/sum(de, na.rm = T)*100, digits = 1), keyby = agecat, env = list(no = no, de = de)]
  begge <- dt[, round(sum(no, na.rm = T)/sum(de, na.rm = T)*100, digits = 1), keyby = .(kjonn, agecat), env = list(no = no, de = de)]

  list(total = tot, kjonn = kjonn, alder = alder, begge = begge)
}
