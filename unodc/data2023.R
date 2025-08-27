## Rusundersøkelse 2023
## --------------------
source(file.path(here::here(), "setup.R"))
## readClipboard()
odrive <- "O:\\Prosjekt\\Rusdata"
rusdrive <- "Rusundersøkelsen\\Rusus historiske data\\ORG\\alkohol_rusundersokelsen"
filpath <- file.path(odrive, rusdrive)
filer <- grep("dta$", list.files(filpath), value = TRUE)

dt <- haven::read_dta(file.path(filpath, grep("2023", filer, value = T)))
setDT(dt)

grep("Ans", names(dt), value = T)

## Denominator
## ------------
dt[Can1 %in% 1:2, canpop := 1]
dt[Ans1 %in% 1:2, narkpop := 1]

## Lifetime  prevalence
## ---------------------
## Bør sjekke tekst fra Ans2sps
## dt[, .N, keyby = Ans2sps][!grep("9999", Ans2sps)]

dt[Can1 == 1, ltp_cannabis := 1]
dt[Ans1 == 1, ltp_any := 1]
dt[Ans2_a == 1, ltp_cocaine := 1]
dt[Ans2_b == 1, ltp_mdma := 1]
dt[Ans2_c == 1, ltp_amphetamines := 1]
dt[Ans2_d == 1, ltp_relevin := 1]
dt[Ans2_e == 1, ltp_heroin := 1]
dt[Ans2_f == 1, ltp_ghb := 1]
dt[Ans2_g == 1, ltp_lsd := 1]

## Last year prevalence
## --------------------

dt[Can6 == 1, lyp_cannabis := 1]
dt[Ans3_1 == 1, lyp_cocaine := 1]
dt[Ans3_2 == 1, lyp_mdma := 1]
dt[Ans3_3 == 1, lyp_amphetamines := 1]
dt[Ans3_4 == 1, lyp_relevin := 1]
dt[Ans3_5 == 1, lyp_heroin := 1]
dt[Ans3_6 == 1, lyp_ghb := 1]
dt[Ans3_7 == 1, lyp_lsd := 1]

## Last month prevalence
## -------------------------

dt[Can10 == 1, lmp_cannabis := 1]


## no - nominator
## de - denominator
cal_prev <- function(dt, no, de){
  dt[, sum(no, na.rm = T)/sum(de, na.rm = T)*100, env = list(no = no, de = de)]
}

cal_prev(dt, "ltp_cannabis", "canpop")

## dummy denominator
dt[!is.na(Can1), canpop2 := 1]
dt[, canpop3 := 0][!(Can1 %in% 8:9), canpop3 := 1] #alle

cal_prev(dt, "ltp_cannabis", "canpop2")
