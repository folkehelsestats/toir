## Data for ST1 EUDA
## --------------------

source(file.path(here::here(), "setup.R"))
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-age.R")
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-percent-weighted.R")
source(file.path(here::here(), "unodc","fun-weighted-unweighted-ci.R"))
source(file.path(here::here(), "unodc","fun-weighted-unweighted-ci-flexible.R"))
source(file.path(here::here(), "unodc","fun-prevalence-ci.R"))
source(file.path(here::here(), "unodc","fun-pct-change.R"))
source(file.path(here::here(), "euda","fun-form-style.R"))

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
euda = list(
      breaks = c(16, 25, 35, 45, 55, 65, Inf),
      labels = c("16-24", "25-34", "35-44", "45-54", "55-64", "65+"))

dt <- group_age(dt, var = "Alder", breaks = euda$breaks,
                labels = euda$labels, new_var = "agecat")


## Either age was not between 16-64 yrs
dt <- dt[agecat != "65+",]

## Exclude all missing and not answered Can1 or Ans1
## Denominator for Illigal rusmidler
## ------------
dt[, canpop := fcase(Can1 %in% 1:2, 1,
                     default = 0)]

dt[, narkpop := fcase(Ans1 %in% 1:2, 1,
                      default = 0)]

dt[canpop == 1 | narkpop == 1, anypop := 1][
  is.na(anypop), anypop := 0]

## Denominator alkohol
dt[, alkopop := fcase(Drukket1 %in% 1:2, 1,
                      default = 0)]

## Denominator Dop
dt[, doppop := fcase(Dop1 %in% 1:2, 1,
                     default = 0)]

## Denominator tobakk
dt[, tobpop := fcase(Tob1 %in% 1:2, 1,
                     default = 0)]

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
dt[grep("psilocybin", Ans2sps, ignore.case = TRUE), "AndreSopp" := 1]

dt[, Can1_ny := Can1][AndreCannabis == 1, Can1_ny := 1]
dt[, Ans2_c_ny := Ans2_c][AndreAmfetamin == 1, Ans2_c_ny := 1]
dt[, Ans2_g_ny := Ans2_g][AndreLSD == 1, Ans2_g_ny := 1]
dt[, Ans2_y_ny := Ans2_y][AndreSopp == 1, Ans2_y_ny := 1]

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
dt[Ans2_x == 1, ltp_nps := 1] #Any new psychoactive substances (NPS)
dt[Ans2_y_ny == 1, ltp_sopp := 1] #Sopp

dt[Dop1 == 1, ltp_steroid := 1] #prestasjonsfremmende midler
dt[Drukket1 == 1 | Drukk1b == 1, ltp_alcohol := 1]

dt[, ltp_tobacco := fcase(Tob1 == 1, 1,
                          Tob14 %in% 1:2, 1,
                          default = 0)]


## ----------
## Analysis
## ----------

# With 95%CI - All Adults
get_prev_ci(dt, "ltp_any", "anypop") #Anyrug
get_prev_ci(dt, "ltp_cannabis", "canpop") #Cannabis-type drugs
get_prev_ci(dt, "ltp_heroin", "narkpop") #Heroin
get_prev_ci(dt, "ltp_cocaine", "narkpop") #Cocaine-type drugs
get_prev_ci(dt, "ltp_amphetamines", "narkpop") #Amphetamine-type stimulants
get_prev_ci(dt, "ltp_mdma", "narkpop") #"Ecstasy" type substances
get_prev_ci(dt, "ltp_ghb", "narkpop") #Other sedatives and tranquilizers
get_prev_ci(dt, "ltp_lsd", "narkpop") #LSD
get_prev_ci(dt, "ltp_nps", "narkpop") #NPS
get_prev_ci(dt, "ltp_sopp", "narkpop") #Sopp
get_prev_ci(dt, "ltp_steroid", "doppop")
get_prev_ci(dt, "ltp_alcohol", "alkopop")
get_prev_ci(dt, "ltp_tobacco", "tobpop")

# Young Adults (15-34)
dty <- dt[Alder < 35]

get_prev_ci(dty, "ltp_any", "anypop") #Anyrug
get_prev_ci(dty, "ltp_cannabis", "canpop") #Cannabis-type drugs
get_prev_ci(dty, "ltp_heroin", "narkpop") #Heroin
get_prev_ci(dty, "ltp_cocaine", "narkpop") #Cocaine-type drugs
get_prev_ci(dty, "ltp_amphetamines", "narkpop") #Amphetamine-type stimulants
get_prev_ci(dty, "ltp_mdma", "narkpop") #"Ecstasy" type substances
get_prev_ci(dty, "ltp_ghb", "narkpop") #Other sedatives and tranquilizers
get_prev_ci(dty, "ltp_lsd", "narkpop") #LSD
general_form(dty, "ltp_nps", "narkpop") #NPS
general_form(dty, "ltp_sopp", "narkpop") #Sopp
get_prev_ci(dty, "ltp_steroid", "doppop")
get_prev_ci(dty, "ltp_alcohol", "alkopop")
get_prev_ci(dty, "ltp_tobacco", "tobpop")

# Broad age groups
# With 95%CI - All Adults
broad_form(dt, "ltp_any", "anypop") #Anyrug
broad_form(dt, "ltp_cannabis", "canpop") #Cannabis-type drugs
broad_form(dt, "ltp_heroin", "narkpop") #Heroin
broad_form(dt, "ltp_cocaine", "narkpop") #Cocaine-type drugs
broad_form(dt, "ltp_amphetamines", "narkpop") #Amphetamine-type stimulants
broad_form(dt, "ltp_mdma", "narkpop") #"Ecstasy" type substances
broad_form(dt, "ltp_ghb", "narkpop") #Other sedatives and tranquilizers
broad_form(dt, "ltp_lsd", "narkpop") #LSD
broad_form(dt, "ltp_nps", "narkpop") #NPS
broad_form(dt, "ltp_sopp", "narkpop") #Sopp
broad_form(dt, "ltp_steroid", "doppop")
broad_form(dt, "ltp_alcohol", "alkopop")
broad_form(dt, "ltp_tobacco", "tobpop")

## Last year prevalence
## --------------------

# Recode
dt[Can6 == 1, lyp_cannabis := 1]
dt[Ans3_1 == 1, lyp_cocaine := 1]
dt[Ans3_2 == 1, lyp_mdma := 1]
dt[Ans3_3 == 1, lyp_amphetamines := 1]
dt[Ans3_4 == 1, lyp_relevin := 1]
dt[Ans3_5 == 1, lyp_heroin := 1]
dt[Ans3_6 == 1, lyp_ghb := 1]
dt[Ans3_7 == 1, lyp_lsd := 1]
dt[Ans3_x == 1, lyp_nps := 1] #NPS. Forklaring i kodebok er "Bruk LSD siste 12 månedene" som er feil. Se LYP for forklaringen
dt[Ans3_y == 1, lyp_sopp := 1] #Sopp. Forklaring i kodebok er "Bruk LSD siste 12 månedene" som er feil. Se LYP for forklaringen
dt[Ans3_8 == 1, lyp_other := 1]

## ans_ans3 <- paste0("Ans3_", c(1:8, "x", "y"))
## dt[, lyp_any := as.numeric(rowSums(.SD == 1, na.rm = TRUE) > 0), .SDcols = ans_ans3]

anyCols <- grep("lyp_", names(dt), value = T)

# Ensure exclude these vars if exists
excVec <- c("lyp_any", "lyp_steroid", "lyp_alcohol")
if(!any(anyCols %in% excVec ))
  anyCols <- anyCols[!(anyCols %in% excVec)]


dt[, anyLYP := as.numeric(rowSums(.SD == 1, na.rm = TRUE) > 0), .SDcols = anyCols]
dt[, lyp_any := fcase(anyLYP == 1, 1,
                      lyp_cannabis == 1, 1,
                      default = 0)]

dt[Dop3_1 == 1, lyp_steroid := 1] #anabole steroider siste 12 m
dt[Drukket1 == 1, lyp_alcohol := 1]

#gen ltp_tobacco=(Tob1==1 | Tob3==1 | Tob3==2) if Tob1<=2
# Hva med Tob14?

general_form(dt, "lyp_any", "anypop") #Anydrugs
general_form(dt, "lyp_cannabis", "canpop") #Cannabis-type drugs
general_form(dt, "lyp_heroin", "narkpop") #Heroin
general_form(dt, "lyp_cocaine", "narkpop") #Cocaine-type drugs
general_form(dt, "lyp_amphetamines", "narkpop") #Amphetamine-type stimulants
general_form(dt, "lyp_mdma", "narkpop") #"Ecstasy" type substances
general_form(dt, "lyp_ghb", "narkpop") #Other sedatives and tranquilizers
general_form(dt, "lyp_lsd", "narkpop") #LSD
general_form(dt, "lyp_nps", "narkpop") #NPS
general_form(dt, "lyp_sopp", "narkpop") #Sopp
general_form(dt, "lyp_steroid", "doppop")
general_form(dt, "lyp_alcohol", "alkopop")


# Young Adults (15-34)
dty <- dt[Alder < 35]

general_form(dty, "lyp_any", "anypop") #Anydrugs
general_form(dty, "lyp_cannabis", "canpop") #Cannabis-type drugs
general_form(dty, "lyp_heroin", "narkpop") #Heroin
general_form(dty, "lyp_cocaine", "narkpop") #Cocaine-type drugs
general_form(dty, "lyp_amphetamines", "narkpop") #Amphetamine-type stimulants
general_form(dty, "lyp_mdma", "narkpop") #"Ecstasy" type substances
general_form(dty, "lyp_ghb", "narkpop") #Other sedatives and tranquilizers
general_form(dty, "lyp_lsd", "narkpop") #LSD
general_form(dty, "lyp_nps", "narkpop") #NPS
general_form(dty, "lyp_sopp", "narkpop") #Sopp
general_form(dty, "lyp_steroid", "doppop")
general_form(dty, "lyp_alcohol", "alkopop")

# Broad age groups
broad_form(dt, "lyp_any", "anypop") #Anydrugs
broad_form(dt, "lyp_cannabis", "canpop") #Cannabis-type drugs
broad_form(dt, "lyp_heroin", "narkpop") #Heroin
broad_form(dt, "lyp_cocaine", "narkpop") #Cocaine-type drugs
broad_form(dt, "lyp_amphetamines", "narkpop") #Amphetamine-type stimulants
broad_form(dt, "lyp_mdma", "narkpop") #"Ecstasy" type substances
broad_form(dt, "lyp_ghb", "narkpop") #Other sedatives and tranquilizers
broad_form(dt, "lyp_lsd", "narkpop") #LSD
broad_form(dt, "lyp_nps", "narkpop") #NPS
broad_form(dt, "lyp_sopp", "narkpop") #Sopp
broad_form(dt, "lyp_steroid", "doppop")
broad_form(dt, "lyp_alcohol", "alkopop")


## Last month prevalence
## -------------------------

dt[Can10 == 1, lmp_cannabis := 1]
dt[Drukket3 == 1, lmp_alcohol := 1]

# All adults
general_form(dt, "lmp_cannabis", "canpop")
general_form(dt, "lmp_alcohol", "alkopop")

# Young adults (15-34)
dty <- dt[Alder < 35]
general_form(dty, "lmp_cannabis", "canpop")
general_form(dty, "lmp_alcohol", "alkopop")

# Broad age groups
broad_form(dt, "lmp_cannabis", "canpop")
broad_form(dt, "lmp_alcohol", "alkopop")

## Frequency with N and %
## -----------------------

get_prev_ci(dt, "lmp_cannabis", "canpop", diagnostic = TRUE) #Cannabis-type drugs

freqVal <- c("20 dager eller mer" = 1,
             "10-19 dager" = 2,
             "4-9 dager" = 3,
             "1-3 dager" = 4)

cannabis_form <- function(dt, group_vars = NULL, dim = NULL){

  # dim - extra dimension to show in final tabel
  canFreqTot <- calc_percentage_flexible(dt = dt,
                                         outcome_var = "Can11",
                                         outcome_type = "categorical",
                                         group_vars = group_vars,
                                         weight_var = "VEKT",
                                         denominator_var = "canpop")
  canFreq <- calc_percentage_flexible(dt = dt,
                                      outcome_var = "Can11",
                                      outcome_type = "categorical",
                                      group_vars = group_vars,
                                      weight_var = "VEKT",
                                      denominator_var = "canpop")

  canKB <- data.table::data.table(v1 = as.integer(freqVal),
                                  v2 = names(freqVal))

  canFreq[kjonnKB, on = c(Kjonn = "v1"), gender := i.v2]
  canFreq[canKB, on = c(outcome_level = "v1"), canfreq := i.v2]
  canFreqTot[canKB, on = c(outcome_level = "v1"), canfreq := i.v2][, gender := "Total"]

  canCols <- c("gender","canfreq", "percentage")

  if (!is.null(dim))
    canCols <- c(canCols, dim)

    list(
      canFreqTot[, ..canCols],
      canFreq[Kjonn == 0, ..canCols],
      canFreq[Kjonn == 1, ..canCols]
    )
}

cannabis_form(dt, "Kjonn")

# Young adults (15-34)
dty <- dt[Alder < 35]
get_prev_ci(dty, "lmp_cannabis", "canpop", diagnostic = TRUE) #Cannabis-type drugs
cannabis_form(dty, "Kjonn")

# Broad age groups (number of cases)
canBroadTot <- calc_percentage_flexible(dt, "lmp_cannabis", group_vars = c("agecat"), weight_var = "VEKT", denominator_var = "canpop")
canBroadTot[order(agecat)][, .(agecat, n_level)]

canBroad <- calc_percentage_flexible(dt, "lmp_cannabis", group_vars = c("Kjonn", "agecat"), weight_var = "VEKT", denominator_var = "canpop")
canBroad[kjonnKB, on = c(Kjonn = "v1"), gender := i.v2][order(agecat, Kjonn)][, .(gender, agecat, n_level)]

# Frequency percentages
cannabis_form(dt, c("Kjonn", "agecat"), dim = "agecat")


canFreqTot <- calc_percentage_flexible(dt = dt,
                                       outcome_var = "Can11",
                                       outcome_type = "categorical",
                                       group_vars = c("agecat"),
                                       weight_var = "VEKT",
                                       denominator_var = "canpop")

canFreqTot[canKB, on = c(outcome_level = "v1"), canfreq := i.v2]
canFreqTot[, gender := "Total"]
## canFreqTot <- canFreqTot[order(agecat, outcome_level), .(agecat, canfreq, percentage)]

canFreqGender <- calc_percentage_flexible(dt = dt,
                                       outcome_var = "Can11",
                                       outcome_type = "categorical",
                                       group_vars = c("Kjonn", "agecat"),
                                       weight_var = "VEKT",
                                       denominator_var = "canpop")

canKB <- data.table::data.table(v1 = as.integer(freqVal),
                                v2 = names(freqVal))

canFreqGender[kjonnKB, on = c(Kjonn = "v1"), gender := i.v2]
canFreqGender[canKB, on = c(outcome_level = "v1"), canfreq := i.v2]
## canFreqGender <- canFreqGender[order(Kjonn, agecat, outcome_level), .(gender, agecat, canfreq, percentage)]

broad_form_cat(canFreqTot, canFreqGender)
