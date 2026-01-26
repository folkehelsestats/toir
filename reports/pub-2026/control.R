
pathToir <- file.path(here::here("rapport", "rap2025"))
source(file.path(pathToir, "setup.R"))

## Output from setup.R
## ddt # data 2012-2024
## DT  # data 2025
## DTT # data 2012-2025

## ==================================
## Lifetime prevalence
## ----------------------------------
cantot <- DT[, .N, keyby = canpop][canpop == 1, N]
canltp <- DT[, .N, keyby = can1][can1 == 1, N]
canltp / cantot * 100

## For control purposes
crude_can <- function(dt, var) {
  DT <- data.table::copy(dt)
  cantot <- DT[, .N, keyby = canpop][canpop == 1, N]
  canltp <- DT[, .N, keyby = can1][get(var) == 1, N]
  canltp / cantot * 100
}

crude_can(DT, "can1") #2025
crude_can(DTT[year == 2025], "can1")
crude_can(DTT[year == 2024], "can1")
crude_can(DTT[year == 2023], "can1")
crude_can(DTT[year == 2022], "can1")
crude_can(DTT[year == 2021], "can1")

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

cal_prev(DT, "ltp_cannabis", "canpop")
