## pak::pkg_install("folkehelsestats/torr")


# tcltk - to display the CRAN windows when not active. Relevant for Emacs
# DescTools - for Winsorize function
pkgs <- c("tcltk","ggplot2", "here", "haven",
          "rlang", "stringr", "highcharter",
          "data.table", "cuci", "torr")

invisible(lapply(pkgs, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
}))

# Sys.setlocale("LC_ALL", "nb-NO.UTF-8")

source("https://raw.githubusercontent.com/folkehelsestats/toir/refs/heads/main/reports/pub-2026/setup.R")


# source(file.path(here::here(), "unodc","fun-weighted-percentage-total-ci03.R"))
# source(file.path(here::here(), "unodc","fun-prevalence-ci02.R"))

source(file.path(here::here(), "euda", "fun-form-style.R"))

### Data 2025 from pub-2026 setup.R file
### Only for 16-64 yrs old
### -----------------------------
dt25 <- data.table::copy(DT25)
dt25[, id := 1:.N]

## Columnames for andre narkotiske stoffer
grep("ans", names(dt25), value = T)

## Age groups
## -------------
## labels = c("16-24", "25-34", "35-44", "45-54", "55-64", "65+")
dt25 <- torr::group_age_standard(dt25, var = "alder", type = "rusund",
                                 new_var = "agecat")

## Røyker variables
## ----------------
## tob1 - Hender det at du røyker? 1,2
## tob2 - Røyker du daglig eller av og til? 1,2
## tob13 - Har du noen gang røykt daglig? 1,2 Hvis tob2 == 2
## tob14 - Har du noen gang røykt daglig eller av og til? 1,2,3 Hvis tob1 == 2

dt25[, roykstatus := fcase(
  tob1 == 1 & tob2  == 1, 1, #Daglig
  tob2 == 2 & tob13 == 1, 2, #Av og til, daglig før
  tob2 == 2 & tob13 == 2, 3, #Av og til, aldri daglig
  tob1 == 2 & tob14 == 1, 4, #Ikke nå, daglig før
  tob1 == 2 & tob14 == 2, 5, #Ikke nå, av og til før
  tob1 == 2 & tob14 == 3, 6, #Aldri røkt
  default = NA
)]

dt25[!is.na(roykstatus), rykpop := 1]

### Populasjon
### ----------
# Create cannabis, narko and any drugs population ie. canpop, narkpop and anypop
create_population <- function(dt) {
  data <- data.table::copy(dt)

  popv <- c("can1", "ans1", "drukket1", "dop1", "roykstatus")
  popx <- setdiff(popv, names(dt))

  if (length(popx) != 0){
    stop("Column(s):", popx, " not found in the dataset", call. = FALSE)
  }
  
  ## Denominator illegale rusmidler 
  data[, canpop := fifelse(can1 %in% 1:2, 1, 0)] #Cannabis
  data[, narkpop := fifelse(ans1 %in% 1:2, 1, 0)] #Other illegal drugs
  data[canpop == 1 | narkpop == 1, anypop := 1][
    is.na(anypop), anypop := 0] # Any illegal drugs

  ## Denominator alkohol
  ## ------------------
  ## drukket1 - Har drukket siste 12 måneder
  ## drukk1b - Har noen gang drukket
  ## Men alle som svarte drukk1b må først svare drukket1, derfor kan drukket1
  ## brukkes som denominator fordi alle i drukk1b er i  drukket1
  data[, alkopop := fcase(drukket1 %in% 1:2, 1,
                          default = 0)]

  ## Denominator Dop
  data[, doppop := fcase(dop1 %in% 1:2, 1,
                         default = 0)]

  ## Denominator tobakk
  data[, tobpop := fcase(roykstatus %in% 1:6, 1,
                         default = 0)]
  return(data)
}

dt <- create_population(dt25)

dx <- data.table::copy(dt)

dt[, .N, keyby = anypop]
dt[, .N, keyby = canpop] # similar to ltpPop_cannabis
dt[, .N, keyby = narkpop]
dt[, .N, keyby = alkopop]
dt[, .N, keyby = tobpop]

## Free text - Other types
## Bør sjekke tekst fra ans2sps
dt[, .N, keyby = ans2sps][!grep("9999", ans2sps)]

dt[grep("sopp", ans2sps, ignore.case = TRUE), "andreSopp" := 1]
dt[grep("mushrooms", ans2sps, ignore.case = TRUE), "andreSopp" := 1]
dt[grep("ketamin", ans2sps, ignore.case = TRUE), "andreKetamin" := 1]
dt[grep("cb", ans2sps, ignore.case = TRUE), "andreLSD" := 1]
dt[grep("2c", ans2sps, ignore.case = TRUE), "andreLSD" := 1]

dt[, ans2_y_ny := ans2_y][andreSopp == 1, ans2_y_ny := 1]
dt[, ans2_g_ny := ans2_g][andreLSD == 1, ans2_g_ny := 1]

is_case <- function(dt, varfrom, varto, val = 1) {
  d <- data.table::copy(dt)

  d[, (varto) := data.table::fcase(
    is.na(get(varfrom)), 0L,
    get(varfrom) == val, 1L,
    default = 0L
  )]

  d
}

## Cannabis
## -----------------------------------------------------------------------------
dt <- is_case(dt, "can1", "ltp_cannabis")
dt <- is_case(dt, "can6", "lyp_cannabis")
dt <- is_case(dt, "can10", "lmp_cannabis")


## LTP
## -----------------------------------------------------------------------------
dt <- is_case(dt, "ans1", "ltp_narko") #Norkotiske stoffer
dt <- is_case(dt, "ans2_a", "ltp_cocaine") #Cocaine-type drugs
dt <- is_case(dt, "ans2_b", "ltp_mdma") #Ecstasy
dt <- is_case(dt, "ans2_c", "ltp_amphetamines") #Amphetamine-type stimulants
dt <- is_case(dt, "ans2_e", "ltp_heroin") #Heroin
dt <- is_case(dt, "ans2_f", "ltp_ghb") #Other sedatives and tranquillizers
dt <- is_case(dt, "ans2_x", "ltp_nps") #Any new psychoactive substances (NPS)

dt[ans2_g == 1, ltp_lsd_x := 1] #LSD - only ans2_g
dt <- is_case(dt, "ans2_g_ny", "ltp_lsd") #LSD

dt[ans2_y == 1, ltp_sopp_x := 1] #Sopp - only ans2_y
dt <- is_case(dt, "ans2_y_ny", "ltp_sopp") #Sopp

dt <- is_case(dt, "ans2_h", "ltp_other") #Andre rusmidler noen gang

## Check freetext recode
## ---------------------
dt[, .N, keyby = ltp_lsd_x]
dt[, .N, keyby = ltp_lsd]
dt[ltp_lsd == 1 | ltp_lsd_x == 1, .(id, ltp_lsd, ltp_lsd_x, ans2_g, ans2sps)]

dt[, .N, keyby = ltp_lsd_x]
dt[, .N, keyby = ltp_lsd]
dt[ltp_sopp == 1 | ltp_sopp_x == 1, .(id, ltp_sopp, ltp_sopp_x, ans2_y, ans2sps)]

## OBS!! This should be run before alcohol, tobacco and dop else will any
## illegale rusmidler includes them as well.
## Any drugs lifetime
# excCols <- grep("alco|alcohol|tobacco|dop|steroid", names(dt), ignore.case = T, value = T)
# excColx <- grep("_x$", names(dt), ignore.case = T, value = T)
# anyltpNark <- grep("^ltp_", names(dt), value = T)
# anyltpIR <- setdiff(anyltpNark, c(excCols, excColx))

anyltpIR <- c("ltp_cannabis", "ltp_narko", "ltp_heroin", "ltp_cocaine",
              "ltp_amphetamines","ltp_mdma","ltp_ghb","ltp_lsd",
              "ltp_nps", "ltp_other")

dt[, ltp_any := as.numeric(rowSums(.SD == 1, na.rm = TRUE) > 0), .SDcols = anyltpIR]

dt[, .N, keyby = ltp_any]

dt <- is_case(dt, "dop1", "ltp_steroid") #prestasjonsfremmende midler

dt[drukket1 == 1 | drukk1b == 1, ltpAlco := 1]
dt <- is_case(dt, "ltpAlco", "ltp_alcohol")

dt[, ltp_tobacco := fcase(roykstatus %in% 1:5, 1,
                          default = 0)]


## LYP
## -----------------------------------------------------------------------------

dt <- is_case(dt, "ans3_1", "lyp_cocaine")
dt <- is_case(dt, "ans3_2", "lyp_mdma")
dt <- is_case(dt, "ans3_3", "lyp_amphetamines")
dt <- is_case(dt, "ans3_5", "lyp_heroin")
dt <- is_case(dt, "ans3_6", "lyp_ghb")
dt <- is_case(dt, "ans3_7", "lyp_lsd")
dt <- is_case(dt, "ans3_8", "lyp_other")
dt <- is_case(dt, "ans3_x", "lyp_nps")
dt <- is_case(dt, "ans3_y", "lyp_sopp")
dt <- is_case(dt, "dop3_1", "lyp_steroid")
dt <- is_case(dt, "drukket1", "lyp_alcohol")

## Any drugs last year exclude sopp
anylypIR <-
c("lyp_cannabis", "lyp_cocaine", "lyp_mdma", "lyp_amphetamines",
"lyp_heroin", "lyp_ghb", "lyp_lsd", "lyp_other", "lyp_nps")

dt[, lyp_any := as.numeric(rowSums(.SD == 1, na.rm = TRUE) > 0), .SDcols = anylypIR]

dim(dt)
dt[, .N, keyby = year]



## -----------------------------------------------------------------------------
## Denominator
## -----------------------------------------------------------------------------

CanVars = c("can1", "can6", "can10")
#Dette lager ltpPop_cannabis, lypPop_cannabis, lmpPop_cannabis
dt <- torr::create_cann_pop(dt, vars = CanVars) 

# Create LTP and LYP colums for respective stoff
dt <- torr::create_narko_pop(dt, types = "ltp", vars = "ans1", val = "narko") #ltpPop_narko
dt <- torr::create_narko_pop(dt, vars = c("ans2_a", "ans3_1"), val = "kokain") #ltpPop_kokain og lypPop_kokain
dt <- torr::create_narko_pop(dt, vars = c("ans2_b", "ans3_2"), val = "mdma") #ltpPop_mdma og lypPop_mdma
dt <- torr::create_narko_pop(dt, vars = c("ans2_c", "ans3_3"), val = "amfetaminer")
dt <- torr::create_narko_pop(dt, vars = c("ans2_e", "ans3_5"), val = "heroin")
dt <- torr::create_narko_pop(dt, vars = c("ans2_f", "ans3_6"), val = "ghb")
dt <- torr::create_narko_pop(dt, vars = c("ans2_g", "ans3_7"), val = "lsd")
dt <- torr::create_narko_pop(dt, vars = c("ans2_x", "ans3_x"), val = "nps")
dt <- torr::create_narko_pop(dt, vars = c("ans2_y", "ans3_y"), val = "sopp")
dt <- torr::create_narko_pop(dt, vars = c("ans2_h", "ans3_8"), val = "annet")

# ltpPop_any
# OBS!! Dette gjelder også som nevnen til lyp_any siden de som svarte lyp spørsmålene
# er basert på om det har svart ans1
dt[ltpPop_cannabis == 1 | ltpPop_narko == 1, popltp_any := 1]
dt <- torr::create_narko_pop(dt, types = "ltp", vars =  "popltp_any", val = "any") #ltpPop_any


# no <- "ltp_any"
# de <- "anypop"
# weight <- "vekt2"


#### ---------------------------------------------------------------------------
#### 2. Lifetime prevalence
#### ---------------------------------------------------------------------------
## 2.1 EUDA age range
## -----------------------------------------------------------------------------

# With 95%CI - All Adults
general_form(dt, "ltp_any", "ltpPop_any") #Anyrug
general_form(dt, "ltp_cannabis", "ltpPop_cannabis") #Cannabis-type drugs
general_form(dt, "ltp_heroin", "ltpPop_heroin") #Heroin
general_form(dt, "ltp_cocaine", "ltpPop_kokain") #Cocaine-type drugs
general_form(dt, "ltp_amphetamines", "ltpPop_amfetaminer") #Amphetamine-type stimulants
general_form(dt, "ltp_mdma", "ltpPop_mdma") #"Ecstasy" type substances
general_form(dt, "ltp_lsd", "ltpPop_lsd") #LSD
general_form(dt, "ltp_sopp", "ltpPop_sopp") #Sopp
general_form(dt, "ltp_ghb", "ltpPop_ghb") #Other sedatives and tranquilizers
general_form(dt, "ltp_steroid", "doppop")
general_form(dt, "ltp_alcohol", "alkopop")
general_form(dt, "ltp_tobacco", "tobpop")
general_form(dt, "ltp_nps", "ltpPop_nps") #NPS
#Dette registreres ikke i skjema siden flere under other må først renses
#fritekst fordi noen av dem kan være cannabis, mdma, lsd etc
general_form(dt, "ltp_other", "ltpPop_narko") #other

## 2.2 EUDA age range Young Adults (16-34)
dty <- dt[alder < 35]
dty[, .(min = min(alder), max = max(alder))]

general_form(dty, "ltp_any", "ltpPop_any") #Anyrug
general_form(dty, "ltp_cannabis", "ltpPop_cannabis") #Cannabis-type drugs
general_form(dty, "ltp_heroin", "ltpPop_heroin") #Heroin
general_form(dty, "ltp_cocaine", "ltpPop_kokain") #Cocaine-type drugs
general_form(dty, "ltp_amphetamines", "ltpPop_amfetaminer") #Amphetamine-type stimulants
general_form(dty, "ltp_mdma", "ltpPop_mdma") #"Ecstasy" type substances
general_form(dty, "ltp_lsd", "ltpPop_lsd") #LSD
general_form(dty, "ltp_sopp", "ltpPop_sopp") #Sopp
general_form(dty, "ltp_ghb", "ltpPop_ghb") #Other sedatives and tranquilizers
general_form(dty, "ltp_steroid", "doppop")
general_form(dty, "ltp_alcohol", "alkopop")
general_form(dty, "ltp_tobacco", "tobpop")
general_form(dty, "ltp_nps", "ltpPop_nps") #NPS
#Dette registreres ikke i skjema siden flere under other må først renses
#fritekst fordi noen av dem kan være cannabis, mdma, lsd etc
general_form(dty, "ltp_other", "ltpPop_narko") #other


## 2.3 Broad age groups
## -----------------------------------------------------------------------------

# With 95%CI - Broad age group LTP
broad_form(dt, "ltp_any", "ltpPop_any") #Anyrug
broad_form(dt, "ltp_cannabis", "ltpPop_cannabis") #Cannabis-type drugs
broad_form(dt, "ltp_heroin", "ltpPop_heroin") #Heroin
broad_form(dt, "ltp_cocaine", "ltpPop_kokain") #Cocaine-type drugs
broad_form(dt, "ltp_amphetamines", "ltpPop_amfetaminer") #Amphetamine-type stimulants
broad_form(dt, "ltp_mdma", "ltpPop_mdma") #"Ecstasy" type substances
broad_form(dt, "ltp_lsd", "ltpPop_lsd") #LSD
broad_form(dt, "ltp_sopp", "ltpPop_sopp") #Sopp
broad_form(dt, "ltp_ghb", "ltpPop_ghb") #Other sedatives and tranquilizers
broad_form(dt, "ltp_steroid", "doppop")
broad_form(dt, "ltp_alcohol", "alkopop")
broad_form(dt, "ltp_tobacco", "tobpop")
broad_form(dt, "ltp_nps", "ltpPop_nps") #NPS
#Dette registreres ikke i skjema siden flere under other må først renses
#fritekst fordi noen av dem kan være cannabis, mdma, lsd etc
broad_form(dt, "ltp_other", "ltpPop_narko") #other


#### ---------------------------------------------------------------------------
#### 3. Last 12 months prevalence
#### ---------------------------------------------------------------------------

## 3.1
# With 95%CI - All Adults
general_form(dt, "lyp_any", "ltpPop_any") #Anyrug: Use ltp pop. Read above for explanation
general_form(dt, "lyp_cannabis", "lypPop_cannabis") #Cannabis-type drugs
general_form(dt, "lyp_heroin", "lypPop_heroin") #Heroin
general_form(dt, "lyp_cocaine", "lypPop_kokain") #Cocaine-type drugs
general_form(dt, "lyp_amphetamines", "lypPop_amfetaminer") #Amphetamine-type stimulants
general_form(dt, "lyp_mdma", "lypPop_mdma") #"Ecstasy" type substances
general_form(dt, "lyp_lsd", "lypPop_lsd") #LSD
general_form(dt, "lyp_sopp", "lypPop_sopp") #Sopp
general_form(dt, "lyp_ghb", "lypPop_ghb") #Other sedatives and tranquilizers
general_form(dt, "lyp_steroid", "doppop")
general_form(dt, "lyp_alcohol", "alkopop")
general_form(dt, "lyp_nps", "lypPop_nps") #NPS
#Dette registreres ikke i skjema siden flere under other må først renses
#fritekst fordi noen av dem kan være cannabis, mdma, lsd etc
general_form(dt, "lyp_other", "lypPop_narko") #other

## 3.2 EUDA age range Young Adults (16-34)
dty <- dt[alder < 35]
dty[, .(min = min(alder), max = max(alder))]

general_form(dty, "lyp_any", "ltpPop_any") #Anydrug
general_form(dty, "lyp_cannabis", "lypPop_cannabis") #Cannabis-type drugs
general_form(dty, "lyp_heroin", "lypPop_heroin") #Heroin
general_form(dty, "lyp_cocaine", "lypPop_kokain") #Cocaine-type drugs
general_form(dty, "lyp_amphetamines", "lypPop_amfetaminer") #Amphetamine-type stimulants
general_form(dty, "lyp_mdma", "lypPop_mdma") #"Ecstasy" type substances
general_form(dty, "lyp_lsd", "lypPop_lsd") #LSD
general_form(dty, "lyp_sopp", "lypPop_sopp") #Sopp
general_form(dty, "lyp_ghb", "lypPop_ghb") #Other sedatives and tranquilizers
general_form(dty, "lyp_steroid", "doppop")
general_form(dty, "lyp_alcohol", "alkopop")
general_form(dty, "lyp_tobacco", "tobpop")
general_form(dty, "lyp_nps", "lypPop_nps") #NPS
#Dette registreres ikke i skjema siden flere under other må først renses
#fritekst fordi noen av dem kan være cannabis, mdma, lsd etc
general_form(dty, "lyp_other", "lypPop_narko") #other

## 3.3 Broad age groups - LYP
## -----------------------------------------------------------------------------

# With 95%CI - Broad age group LYP
broad_form(dt, "lyp_any", "ltpPop_any") #Anydrug
broad_form(dt, "lyp_cannabis", "lypPop_cannabis") #Cannabis-type drugs
broad_form(dt, "lyp_heroin", "lypPop_heroin") #Heroin
broad_form(dt, "lyp_cocaine", "lypPop_kokain") #Cocaine-type drugs
broad_form(dt, "lyp_amphetamines", "lypPop_amfetaminer") #Amphetamine-type stimulants
broad_form(dt, "lyp_mdma", "lypPop_mdma") #"Ecstasy" type substances
broad_form(dt, "lyp_lsd", "lypPop_lsd") #LSD
broad_form(dt, "lyp_sopp", "lypPop_sopp") #Sopp
broad_form(dt, "lyp_ghb", "lypPop_ghb") #Other sedatives and tranquilizers
broad_form(dt, "lyp_steroid", "doppop")
broad_form(dt, "lyp_alcohol", "alkopop")
broad_form(dt, "lyp_nps", "lypPop_nps") #NPS
#Dette registreres ikke i skjema siden flere under other må først renses
#fritekst fordi noen av dem kan være cannabis, mdma, lsd etc
broad_form(dt, "lyp_other", "lypPop_narko") #other

### ----------------------------------------------------------------------------
### 4 - Last 30 days prevalence
### ----------------------------------------------------------------------------

## 4.1 All adults - LMP
## -----------------------------------------------------------------------------

general_form(dt, "lmp_cannabis", "lypPop_cannabis") #Cannabis-type drugs

dt <- is_case(dt, "drukket3", "lmp_alcohol")
general_form(dt, "lmp_alcohol", "alkopop") #Cannabis-type drugs

## 4.2 EUDA age range Young Adults (16-34)
dty <- dt[alder < 35]
dty[, .(min = min(alder), max = max(alder))]

general_form(dty, "lmp_cannabis", "lmpPop_cannabis") #Cannabis-type drugs
general_form(dty, "lmp_alcohol", "alkopop") #Cannabis-type drugs

## 4.3 Broad form - LMP
## -----------------------------------------------------------------------------
broad_form(dt, "lmp_cannabis", "lmpPop_cannabis") #Cannabis-type drugs
broad_form(dt, "lmp_alcohol", "alkopop") #Cannabis-type drugs


### ----------------------------------------------------------------------------
### 5 - Quantitative info : Freq of cannabis use
### ----------------------------------------------------------------------------

source("https://raw.githubusercontent.com/folkehelsestats/toir/refs/heads/main/unodc/fun-weighted-unweighted-ci-flexible.R")

sp5 <- torr::calc_prevalence(dt, "lmpPop_cannabis",
                      year_var = "year", outcome_var = "lmp_cannabis",
                      weight_var = "vekt2")

dt[, .N, keyby = .(kjonn, lmp_cannabis)]

freqVal <- c("20 dager eller mer" = 1,
             "10-19 dager" = 2,
             "4-9 dager" = 3,
             "1-3 dager" = 4)

kjonnKB <- data.table::data.table(v1 = 1:2, v2 = c("Male", "Female"))

# Total
calc_percentage_flexible(dt = dt,
                         outcome_var = "can11",
                         outcome_type = "categorical",
                         group_vars = NULL,
                         weight_var = "vekt2",
                         denominator_var = "lmpPop_cannabis",
                         round_digits = 1)

# Form style
# ------------------------------------------------------------------------------
cannabis_form <- function(dt, group_vars = NULL, dim = NULL, keep = NULL){

  # dim - extra dimension to show in final tabel
  # keep - extra columns to keep as output

  if (!is.null(dim)){
    totVar <- dim
  } else {
    totVar <- NULL
  }

  canFreqTot <- calc_percentage_flexible(dt = dt,
                                         outcome_var = "can11",
                                         outcome_type = "categorical",
                                         group_vars = totVar,
                                         weight_var = "vekt2",
                                         denominator_var = "lmpPop_cannabis",
                                         round_digits = 1)

  canFreq <- calc_percentage_flexible(dt = dt,
                                      outcome_var = "can11",
                                      outcome_type = "categorical",
                                      group_vars = group_vars,
                                      weight_var = "vekt2",
                                      denominator_var = "lmpPop_cannabis",
                                      round_digits = 1)

  canKB <- data.table::data.table(v1 = as.integer(freqVal),
                                  v2 = names(freqVal))

  canFreq[kjonnKB, on = c(kjonn = "v1"), gender := i.v2]
  canFreq[canKB, on = c(outcome_level = "v1"), canfreq := i.v2]
  canFreqTot[canKB, on = c(outcome_level = "v1"), canfreq := i.v2][, gender := "Total"]

  canCols <- c("gender","canfreq", "percentage")

  if (!is.null(keep))
    canCols <- c(canCols, keep)

  if (!is.null(dim))
    canCols <- c(canCols, dim)

    list(
      canFreqTot[, ..canCols],
      canFreq[kjonn == 1, ..canCols],
      canFreq[kjonn == 2, ..canCols]
    )
}

cannabis_form(dt, "kjonn")



## Young adults (15-34)
## -----------------------------------------------------------------------------

dty <- dt[alder < 35]
dty[, .N, keyby = .(kjonn, lmp_cannabis)]

cannabis_form(dty, "kjonn")

## Broad age groups
## -----------------------------------------------------------------------------

# Number of cases
canBroadTot <- calc_percentage_flexible(dt, "lmp_cannabis", group_vars = "agecat", weight_var = "vekt2", denominator_var = "lmpPop_cannabis")
canBroadTot[order(agecat)][, .(agecat, n_level)]

canBroad <- calc_percentage_flexible(dt, "lmp_cannabis", group_vars = c("kjonn", "agecat"), weight_var = "vekt2", denominator_var = "lmpPop_cannabis")
canBroad[kjonnKB, on = c(kjonn = "v1"), gender := i.v2][order(agecat, kjonn)][, .(gender, agecat, n_level)]

# Frequency percentages
unsortTbl <- cannabis_form(dt, c("kjonn", "agecat"), dim = "agecat", keep = "outcome_level")

totTbl <- unsortTbl[[1]][order(agecat, outcome_level)]
malTbl <- unsortTbl[[2]][order(agecat, outcome_level)]
femTbl <- unsortTbl[[3]][order(agecat, outcome_level)]

## Inspect high percentage
dt[agecat == "55-64" & kjonn == 1 & can10 %in% 1:2 , .(can10, can11, vekt2)]
dt[agecat == "55-64" & kjonn == 1 & can11 %in% 1:4, .(can10, can11, vekt2)]
dt[agecat == "55-64" & kjonn == 2 & can11 %in% 1:4, .(can10, can11, vekt2)]


canFreqTot <- calc_percentage_flexible(dt = dt,
                                       outcome_var = "can11",
                                       outcome_type = "categorical",
                                       group_vars = c("agecat"),
                                       weight_var = "vekt2",
                                       denominator_var = "lmpPop_cannabis")

canFreqTot[canKB, on = c(outcome_level = "v1"), canfreq := i.v2]
canFreqTot[, gender := "Total"]
canFreqTot
## canFreqTot <- canFreqTot[order(agecat, outcome_level), .(agecat, canfreq, percentage)]


canFreqGender <- calc_percentage_flexible(dt = dt,
                                       outcome_var = "can11",
                                       outcome_type = "categorical",
                                       group_vars = c("Kjonn", "agecat"),
                                       weight_var = "vekt2",
                                       denominator_var = "lmpPop_cannabis")

canKB <- data.table::data.table(v1 = as.integer(freqVal),
                                v2 = names(freqVal))

canFreqGender[kjonnKB, on = c(Kjonn = "v1"), gender := i.v2]
canFreqGender[canKB, on = c(outcome_level = "v1"), canfreq := i.v2]
## canFreqGender <- canFreqGender[order(Kjonn, agecat, outcome_level), .(gender, agecat, canfreq, percentage)]

broad_form_cat(canFreqTot, canFreqGender)



#### ---------------------------------------------------------------------------
#### TESTING - NOT part of the form
#### ---------------------------------------------------------------------------

canFreqTot <- calc_percentage_flexible(dt = dt,
                                       outcome_var = "can11",
                                       outcome_type = "categorical",
                                       group_vars = NULL,
                                       weight_var = "vekt2",
                                       denominator_var = "lmpPop_cannabis")


canFreq <- calc_percentage_flexible(dt = dt,
                                    outcome_var = "can11",
                                    outcome_type = "categorical",
                                    group_vars = "kjonn",
                                    weight_var = "vekt2",
                                    denominator_var = "lmpPop_cannabis")

canKB <- data.table::data.table(v1 = as.integer(freqVal),
                                v2 = names(freqVal))

canFreq[kjonnKB, on = c(kjonn = "v1"), gender := i.v2]
canFreq[canKB, on = c(outcome_level = "v1"), canfreq := i.v2]
canFreqTot[canKB, on = c(outcome_level = "v1"), canfreq := i.v2][, gender := "Total"]

canCols <- c("gender","canfreq", "percentage")

if (!is.null(dim))
  canCols <- c(canCols, dim)

list(
  canFreqTot[, ..canCols],
  canFreq[kjonn == 0, ..canCols],
  canFreq[kjonn == 1, ..canCols]
)
