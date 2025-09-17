
source(file.path(here::here(), "setup.R"))
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-age.R")
source(file.path(here::here(), "unodc","fun-weighted-unweighted.R"))
source(file.path(here::here(), "unodc","fun-prevalence.R"))
source(file.path(here::here(), "unodc","fun-pct-change.R"))

## Data 2024
## --------------
mainpath <- "O:\\Prosjekt\\Rusdata"
## DT <- haven::read_dta(file.path("Rusundersøkelsen", "Rusus 2024", "nytt forsøk februar 25 rus24.dta"))
## saveRDS(DT, file.path("Rusundersøkelsen", "Rusus 2024","rus2024.rds"))
DT <- readRDS(file.path(mainpath, "Rusundersøkelsen", "Rusus 2024","rus2024.rds"))
dt <- as.data.table(DT)

## Columnames for andre narkotiske stoffer
grep("Ans", names(dt), value = T)

## Age groups
## -------------
dt <- group_age_standard(dt, var = "Alder", type = "unodc",
                         new_var = "agecat")

## Denominator
## ------------
dt[, canpop := fcase(Can1 %in% 1:2, 1,
                     default = 0)]

dt[, narkpop := fcase(Ans1 %in% 1:2, 1,
                      default = 0)]

dt[canpop == 1 | narkpop == 1, anypop := 1][
  is.na(anypop), anypop := 0]

## Exclude all missing and not answered Can1 or Ans1
## Either age was not between 16-64 yrs
dt <- dt[anypop == 1,]

## Sample size for 16-64
nrow(dt)

## Free text - Other types
## Bør sjekke tekst fra Ans2sps
dt[, .N, keyby = Ans2sps][!grep("9999", Ans2sps)]

dt[grep("lsd", Ans2sps, ignore.case = TRUE), "AndreLSD" := 1]
dt[grep("hasj", Ans2sps, ignore.case = TRUE), "AndreCannabis" := 1]
dt[grep("cb", Ans2sps, ignore.case = TRUE), "AndreNPS" := 1]
dt[grep("psilocybin", Ans2sps, ignore.case = TRUE), "AndreSOPP" := 1]
dt[str_detect(Ans2sps, regex("methamfethamin|metamfethamin", ignore_case = TRUE)), "AndreAmfetamin" := 1]
dt[grep("ketamin", Ans2sps, ignore.case = TRUE), "AndreKetamin" := 1]

dt[, Can1_ny := Can1][AndreCannabis == 1, Can1_ny := 1]
dt[, Ans2_cny := Ans2_c][AndreAmfetamin == 1, Ans2_cny := 1]
dt[, Ans2_gny := Ans2_g][AndreLSD == 1, Ans2_gny := 1]

## ---------------------
## Lifetime  prevalence
## ---------------------

dt[Can1_ny == 1, ltp_cannabis := 1] #Cannabis-type drugs
dt[Ans1 == 1, ltp_other := 1]

## Any drug
dt[, ltp_any := fcase(Ans1 == 1, 1,
                      ltp_cannabis == 1, 1,
                      default = 0)]

dt[Ans2_a == 1, ltp_cocaine := 1] #Cocaine-type drugs
dt[Ans2_b == 1, ltp_mdma := 1] #"Ecstasy" type substances
dt[Ans2_cny == 1, ltp_amphetamines := 1] #Amphetamine-type stimulants
dt[Ans2_d == 1, ltp_relevin := 1]
dt[Ans2_e == 1, ltp_heroin := 1] #Heroin
dt[Ans2_f == 1, ltp_ghb := 1] #Other sedatives and tranquillizers
dt[Ans2_gny == 1, ltp_lsd := 1] #LSD

get_prev(dt, "ltp_any", "anypop") #Anyrug
get_prev(dt, "ltp_cannabis", "canpop") #Cannabis-type drugs
get_prev(dt, "ltp_heroin", "narkpop") #Heroin
get_prev(dt, "ltp_cocaine", "narkpop") #Cocaine-type drugs
get_prev(dt, "ltp_amphetamines", "narkpop") #Amphetamine-type stimulants
get_prev(dt, "ltp_mdma", "narkpop") #"Ecstasy" type substances
get_prev(dt, "ltp_ghb", "narkpop") #Other sedatives and tranquilizers
get_prev(dt, "ltp_lsd", "narkpop") #LSD

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

ans_ans3 <- paste0("Ans3_", c(1:8, "x", "y"))
dt[, lyp_any := as.numeric(rowSums(.SD == 1, na.rm = TRUE) > 0), .SDcols = ans_ans3]

get_prev(dt, "lyp_any", "anypop") #Anyrug
get_prev(dt, "lyp_cannabis", "canpop") #Cannabis-type drugs
get_prev(dt, "lyp_heroin", "narkpop", diagnostic = FALSE) #Heroin
get_prev(dt, "lyp_cocaine", "narkpop") #Cocaine-type drugs
get_prev(dt, "lyp_amphetamines", "narkpop") #Amphetamine-type stimulants
get_prev(dt, "lyp_mdma", "narkpop") #"Ecstasy" type substances
get_prev(dt, "lyp_ghb", "narkpop") #Other sedatives and tranquilizers
get_prev(dt, "lyp_lsd", "narkpop") #LSD

## Last month prevalence
## -------------------------

dt[Can10 == 1, lmp_cannabis := 1]

get_prev(dt, "lmp_cannabis", "canpop")

## ----------------------------
## Trend dvs. data 2023 og 2024
## ----------------------------

odrive <- "O:\\Prosjekt\\Rusdata"
rusdrive <- "Rusundersøkelsen\\Rusus historiske data\\ORG\\alkohol_rusundersokelsen"
filpath <- file.path(odrive, rusdrive)

d2023 <- haven::read_dta(file.path(filpath, "Rus2023.dta"))
d2024 <- readRDS(file.path(odrive, "Rusundersøkelsen", "Rusus 2024","rus2024.rds"))

setDT(d2023)
setDT(d2024)

## Columnames for andre narkotiske stoffer
grep("Ans|Can", names(d2023), value = T)
grep("Ans|Can", names(d2024), value = T)

## need standardized weight and variablenames as in d2023
meanX <- d2024[, mean(VEKT, na.rm = T)]
d2024[, nyvekt2 := VEKT/meanX]

keepVar <- intersect(names(d2023), names(d2024))
d2023 <- d2023[, ..keepVar][, year := 2023]
d2024 <- d2024[, ..keepVar][, year := 2024]

dtx <- data.table::rbindlist(list(d2023, d2024), ignore.attr = TRUE)

## Age groups
## -------------
AgeBrk = c(16, 18, 25, Inf)
AgeLbl = c("16-17", "18-24", "25+")
dtx <- group_age(dtx, var = "Alder", breaks = AgeBrk, labels = AgeLbl, new_var = "agecat", copy = F)

## Denominator
## ------------
dtx[, canpop := fcase(Can1 %in% 1:2, 1,
                     default = 0)]

dtx[, narkpop := fcase(Ans1 %in% 1:2, 1,
                      default = 0)]

dtx[canpop == 1 | narkpop == 1, anypop := 1][
  is.na(anypop), anypop := 0]

## Exclude all missing and not answered Can1 or Ans1
## Either age was not between 16-64 yrs
dtx <- dtx[anypop == 1,]

dtx[, .N, keyby =  year]

## Last year prevalence
## --------------------
dtx[Can6 == 1, lyp_cannabis := 1]
dtx[Ans3_1 == 1, lyp_cocaine := 1]
dtx[Ans3_2 == 1, lyp_mdma := 1]
dtx[Ans3_3 == 1, lyp_amphetamines := 1]
dtx[Ans3_4 == 1, lyp_relevin := 1]
dtx[Ans3_5 == 1, lyp_heroin := 1]
dtx[Ans3_6 == 1, lyp_ghb := 1]
dtx[Ans3_7 == 1, lyp_lsd := 1]

grep("Ans3_", names(dtx), value = T)
ans_ans3 <- paste0("Ans3_", c(1:8))
dtx[, lyp_any := as.numeric(rowSums(.SD == 1, na.rm = TRUE) > 0), .SDcols = ans_ans3]

calc_change(dtx, "lyp_any", "year", "anypop")
calc_change(dtx, "lyp_cannabis", "year", "canpop")
calc_change(dtx, "lyp_heroin", "year", "canpop")
calc_change(dtx, "lyp_cocaine", "year", "canpop")
calc_change(dtx, "lyp_amphetamines", "year", "canpop")
calc_change(dtx, "lyp_mdma", "year", "canpop")
calc_change(dtx, "lyp_ghb", "year", "canpop")
calc_change(dtx, "lyp_lsd", "year", "canpop")

grp <- c("year", "agecat")
calc_change(dtx, "lyp_any", group_vars = grp, "anypop")
calc_change(dtx, "lyp_cannabis", group_vars = grp, "canpop")
calc_change(dtx, "lyp_heroin", group_vars = grp, "canpop")
calc_change(dtx, "lyp_cocaine", group_vars = grp, "canpop")
calc_change(dtx, "lyp_amphetamines", group_vars = grp, "canpop")
calc_change(dtx, "lyp_mdma", group_vars = grp, "canpop")
calc_change(dtx, "lyp_ghb", group_vars = grp, "canpop")
calc_change(dtx, "lyp_lsd", group_vars = grp, "canpop")


calc_percentage(dtx, "lyp_any", "year", weight_var = "nyvekt2", denominator_var = "anypop", include_diagnostics = F, na_treatment =  "as_zero")
calc_percentage(dtx, "lyp_cannabis", "year", weight_var = "nyvekt2", denominator_var = "canpop", include_diagnostics = F, na_treatment = "as_zero" )
