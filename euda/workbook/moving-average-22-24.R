## Analysis of Moving Average from 2022 to 2024
## --------------------------------------
source(here::here("setup.R"))
source(here::here("data-source.R"))

# clean labelled attributes coz it makes troubles
DT[, (names(DT)) := lapply(.SD, \(x) { attributes(x) <- NULL; x })]

dt <- DT[year %in% 2022:2024]
nrow(dt)


## for (j in names(dt)) {
##   # Remove attributes from the column
##   col <- dt[[j]]
##   attributes(col) <- NULL

##   # Set the cleaned column back into dt
##   data.table::set(dt, j = j, value = col)
## }

## OBS! Exclude those that used Relevin Ans2_d == 1
## dt <- dt[ans2_d != 1]

dt[ans2_d == 1, .N]
ans2 <- grep("ans2_", names(dx), ignore.case = TRUE, value = TRUE)
ansTmp <- paste0(ans2, "_tmp")

dt[, (ansTmp) := lapply(.SD, function(x) {
  fcase(
    x == 1, 1,
    x %in% c(2, 8, 9), NA,
    default = NA
  )
}), .SDcols = ans2]

dt[, ansSum := rowSums(.SD, na.rm = TRUE), .SDcols = ansTmp]
dt <- dt[ansSum != 8] # Remove those who answered yes to all drugs including Relevin
dt[, (ansTmp) := NULL] # Remove temporary columns


## Exclude all missing and not answered Can1 or Ans1
## Denominator for Illigal rusmidler
## ------------
dt[, canpop := ifelse(can1 %in% 1:2, 1, 0)]
dt[, .N, keyby = canpop]
dt[, .N, keyby = can1]

dt[, narkpop := ifelse(ans1 %in% 1:2, 1, 0)]
dt[, .N, keyby = narkpop]
dt[, .N, keyby = ans1]

dt[canpop == 1 | narkpop == 1, anypop := 1][
  is.na(anypop), anypop := 0]

## Figure 6: Cannabis use all adults (16-64) and young adults (16-34), 2022-2024
## --------------------------------------------------
## Free text - Other types
## Bør sjekke tekst fra ans2sps
dt[, .N, keyby = ans2sps][!grep("9999", ans2sps)]

dt[grep("lsd", ans2sps, ignore.case = TRUE), "AndreLSD" := 1]
dt[grep("hasj", ans2sps, ignore.case = TRUE), "AndreCannabis" := 1]
dt[grep("cb", ans2sps, ignore.case = TRUE), "AndreNPS" := 1]
dt[grep("psilocybin", ans2sps, ignore.case = TRUE), "AndreSOPP" := 1]
dt[str_detect(ans2sps, regex("methamfethamin|metamfethamin", ignore_case = TRUE)), "AndreAmfetamin" := 1]
dt[grep("ketamin", ans2sps, ignore.case = TRUE), "AndreKetamin" := 1]
dt[grep("psilocybin", ans2sps, ignore.case = TRUE), "AndreSopp" := 1]

dt[, Can1_ny := can1][AndreCannabis == 1, Can1_ny := 1]
dt[, Ans2_c_ny := ans2_c][AndreAmfetamin == 1, Ans2_c_ny := 1]
dt[, Ans2_g_ny := ans2_g][AndreLSD == 1, Ans2_g_ny := 1]
## dt[, Ans2_y_ny := ans2_y][AndreSopp == 1, Ans2_y_ny := 1]

## ---------------------
## Lifetime  prevalence
## ---------------------

dt[Can1_ny == 1, ltp_cannabis := 1] #Cannabis-type drugs
dt[ans1 == 1, ltp_drugs := 1] #Other drugs than cannabis

## Any drug
dt[, ltp_any := fcase(ltp_drugs == 1, 1,
                      ltp_cannabis == 1, 1,
                      default = 0)]

dt[ans2_a == 1, ltp_cocaine := 1] #Cocaine-type drugs
dt[ans2_b == 1, ltp_mdma := 1] #"Ecstasy" type substances
dt[Ans2_c_ny == 1, ltp_amphetamines := 1] #Amphetamine-type stimulants
dt[ans2_d == 1, ltp_relevin := 1]
dt[ans2_e == 1, ltp_heroin := 1] #Heroin
dt[ans2_f == 1, ltp_ghb := 1] #Other sedatives and tranquillizers
dt[Ans2_g_ny == 1, ltp_lsd := 1] #LSD
## dt[ans2_x == 1, ltp_nps := 1] #Any new psychoactive substances (NPS)
## dt[Ans2_y_ny == 1, ltp_sopp := 1] #Sopp
dt[ans2_h == 1, ltp_other := 1]

dt[Dop1 == 1, ltp_steroid := 1] #prestasjonsfremmende midler
dt[Drukket1 == 1 | Drukk1b == 1, ltp_alcohol := 1]

dt[, ltp_tobacco := fcase(Tob1 == 1, 1,
                          Tob14 %in% 1:2, 1,
                          default = 0)]
