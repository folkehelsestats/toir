source("https://raw.githubusercontent.com/folkehelsestats/toir/refs/heads/main/euda/euda-setup2025.R")

#### ---------------------------------------------------------------------------
#### 2. Lifetime prevalence
#### ---------------------------------------------------------------------------
## 2.1 EUDA age range
## -----------------------------------------------------------------------------

# With 95%CI - All Adults
general_form(dt, "ltp_any", "ltpPop_any") #Anydrug
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
#Dette skal ikke registreres i skjema siden flere under other må først renses
#fritekst fordi noen av dem kan være cannabis, mdma, lsd etc
general_form(dt, "ltp_other", "ltpPop_narko") #other

## 2.2 EUDA age range Young Adults (16-34)
dty <- dt[alder < 35] #beholder bare de som er i denne aldersgruppe
dty[, .(min = min(alder), max = max(alder))]

general_form(dty, "ltp_any", "ltpPop_any") #Anydrug
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
#Dette skal ikke registreres i skjema siden flere under other må først renses
#fritekst fordi noen av dem kan være cannabis, mdma, lsd etc
general_form(dty, "ltp_other", "ltpPop_narko") #other


## 2.3 Broad age groups
## -----------------------------------------------------------------------------

# With 95%CI - Broad age group LTP
broad_form(dt, "ltp_any", "ltpPop_any") #Anydrug
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
#Dette skal ikke registreres i skjema siden flere under other må først renses
#fritekst fordi noen av dem kan være cannabis, mdma, lsd etc
broad_form(dt, "ltp_other", "ltpPop_narko") #other


#### ---------------------------------------------------------------------------
#### 3. Last 12 months prevalence
#### ---------------------------------------------------------------------------

## 3.1
# With 95%CI - All Adults
general_form(dt, "lyp_any", "ltpPop_any") #Anydrug: Use ltp pop. Read above for explanation
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
#Dette skal ikke registreres i skjema siden flere under other må først renses
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
#Dette skal ikke registreres i skjema siden flere under other må først renses
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
#Dette skal ikke registreres i skjema siden flere under other må først renses
#fritekst fordi noen av dem kan være cannabis, mdma, lsd etc
broad_form(dt, "lyp_other", "lypPop_narko") #other

### ----------------------------------------------------------------------------
### 4 - Last 30 days prevalence
### ----------------------------------------------------------------------------

## 4.1 All adults - LMP
## -----------------------------------------------------------------------------

general_form(dt, "lmp_cannabis", "lmpPop_cannabis") #Cannabis-type drugs

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
