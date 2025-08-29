
source(file.path(here::here(), "setup.R"))
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-age.R")
source(file.path(here::here(), "unodc","fun-prevalence.R"))

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

## ---------------------
## Lifetime  prevalence
## ---------------------
## Bør sjekke tekst fra Ans2sps
## dt[, .N, keyby = Ans2sps][!grep("9999", Ans2sps)]

dt[Can1 == 1, ltp_cannabis := 1] #Cannabis-type drugs
dt[Ans1 == 1, ltp_other := 1]

## Any drug
dt[, ltp_any := fcase(Ans1 == 1, 1,
                      ltp_cannabis == 1, 1,
                      default = 0)]

dt[Ans2_a == 1, ltp_cocaine := 1] #Cocaine-type drugs
dt[Ans2_b == 1, ltp_mdma := 1] #"Ecstasy" type substances
dt[Ans2_c == 1, ltp_amphetamines := 1] #Amphetamine-type stimulants
dt[Ans2_d == 1, ltp_relevin := 1]
dt[Ans2_e == 1, ltp_heroin := 1] #Heroin
dt[Ans2_f == 1, ltp_ghb := 1] #Other sedatives and tranquillizers
dt[Ans2_g == 1, ltp_lsd := 1] #LSD



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


cal_prev(dt, "ltp_cannabis", "canpop")
cal_prev(dt, "ltp_heroin", "narkpop")
