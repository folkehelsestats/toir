## Data for ST1 EUDA
## --------------------

source(file.path(here::here(), "setup.R"))
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-age.R")
source(file.path(here::here(), "unodc","fun-weighted-unweighted-ci.R"))
source(file.path(here::here(), "unodc","fun-prevalence-ci.R"))
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
dt[, Ans2_c_ny := Ans2_c][AndreAmfetamin == 1, Ans2_c_ny := 1]
dt[, Ans2_g_ny := Ans2_g][AndreLSD == 1, Ans2_g_ny := 1]

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
dt[Ans2_c_ny == 1, ltp_amphetamines := 1] #Amphetamine-type stimulants
dt[Ans2_d == 1, ltp_relevin := 1]
dt[Ans2_e == 1, ltp_heroin := 1] #Heroin
dt[Ans2_f == 1, ltp_ghb := 1] #Other sedatives and tranquillizers
dt[Ans2_g_ny == 1, ltp_lsd := 1] #LSD



# With 95%CI
get_prev_ci(dt, "ltp_any", "anypop") #Anyrug
get_prev_ci(dt, "ltp_cannabis", "canpop") #Cannabis-type drugs
get_prev_ci(dt, "ltp_heroin", "narkpop") #Heroin
get_prev_ci(dt, "ltp_cocaine", "narkpop") #Cocaine-type drugs
get_prev_ci(dt, "ltp_amphetamines", "narkpop") #Amphetamine-type stimulants
get_prev_ci(dt, "ltp_mdma", "narkpop") #"Ecstasy" type substances
get_prev_ci(dt, "ltp_ghb", "narkpop") #Other sedatives and tranquilizers
get_prev_ci(dt, "ltp_lsd", "narkpop") #LSD


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
dt[Ans3_8 == 1, lyp_other := 1]

## ans_ans3 <- paste0("Ans3_", c(1:8, "x", "y"))
## dt[, lyp_any := as.numeric(rowSums(.SD == 1, na.rm = TRUE) > 0), .SDcols = ans_ans3]

anyCols <- grep("lyp_", names(dt), value = T)
dt[, anyLYP := as.numeric(rowSums(.SD == 1, na.rm = TRUE) > 0), .SDcols = anyCols]
dt[, lyp_any := fcase(anyLYP == 1, 1,
                      lyp_cannabis == 1, 1,
                      default = 0)]

get_prev_ci(dt, "lyp_any", "anypop") #Anydrugs
get_prev_ci(dt, "lyp_cannabis", "canpop") #Cannabis-type drugs
get_prev_ci(dt, "lyp_heroin", "narkpop", diagnostic = FALSE) #Heroin
get_prev_ci(dt, "lyp_cocaine", "narkpop") #Cocaine-type drugs
get_prev_ci(dt, "lyp_amphetamines", "narkpop") #Amphetamine-type stimulants
get_prev_ci(dt, "lyp_mdma", "narkpop") #"Ecstasy" type substances
get_prev_ci(dt, "lyp_ghb", "narkpop") #Other sedatives and tranquilizers
get_prev_ci(dt, "lyp_lsd", "narkpop") #LSD

## Last month prevalence
## -------------------------

dt[Can10 == 1, lmp_cannabis := 1]

get_prev_ci(dt, "lmp_cannabis", "canpop")
