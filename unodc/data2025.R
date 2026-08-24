# # Install the development version from GitHub
# # install.packages("pak")
# pak::pak("folkehelsestats/cuci")
library(cuci)
library(torr)
library(data.table)

source("https://raw.githubusercontent.com/folkehelsestats/toir/refs/heads/main/reports/pub-2026/setup.R")

source(file.path(here::here(), "unodc","fun-weighted-percentage-total-ci03.R"))
source(file.path(here::here(), "unodc","fun-prevalence-ci02.R"))


## Data 2024
## --------------
# mainpath <- "O:\\Prosjekt\\Rusdata\\Rusundersokelsen\\Datasets\\Rusus_2025"
# dt <- readRDS(file.path(mainpath, "rusus2025_20251126.rds"))
# setDT(dt)

### Data 2025 from pub-2026 setup.R file
### Only for 16-64 yrs old
### -----------------------------
dt25 <- data.table::copy(DT25)

## Columnames for andre narkotiske stoffer
grep("ans", names(dt25), value = T)

## Age groups
## -------------
dt25 <- torr::group_age_standard(dt25, var = "alder", type = "unodc",
                                 new_var = "agecat")


### Populasjon
### ----------
# Create cannabis, narko and any drugs population ie. canpop, narkpop and anypop
create_population <- function(dt) {
  data <- data.table::copy(dt)

  data[, canpop := fifelse(can1 %in% 1:2, 1, 0)] #Cannabis
  data[, narkpop := fifelse(ans1 %in% 1:2, 1, 0)] #Other illegal drugs
  data[canpop == 1 | narkpop == 1, anypop := 1][
    is.na(anypop), anypop := 0] # Any illegal drugs

  return(data)
}

dt <- create_population(dt25)
dt[, .N, keyby = anypop]

CanVars = c("can1", "can6", "can10")
dt <- torr::create_cann_pop(dt, vars = CanVars ) #Dette lager ltpPop_cannabis, lypPop_cannabis, lmpPop_cannabis

dt <- torr::create_narko_pop(dt, vars = c("ans2_a", "ans3_1"), val = "kokain") #ltpPop_kokain og lypPop_kokain
dt <- torr::create_narko_pop(dt, vars = c("ans2_b", "ans3_2"), val = "mdma") #ltpPop_mdma og lypPop_mdma
dt <- torr::create_narko_pop(dt, vars = c("ans2_c", "ans3_3"), val = "amfetaminer")
dt <- torr::create_narko_pop(dt, vars = c("ans2_e", "ans3_5"), val = "heroin")
dt <- torr::create_narko_pop(dt, vars = c("ans2_f", "ans3_6"), val = "ghb")
dt <- torr::create_narko_pop(dt, vars = c("ans2_g", "ans3_7"), val = "lsd")
dt <- torr::create_narko_pop(dt, vars = c("ans2_h", "ans3_8"), val = "annet")

## Illegal drugs variables
## ----------------------------------
dt[can1 == 1, ltp_cannabis := 1] # Lifetime prevalence
dt[can6 == 1, lyp_cannabis := 1] # Last year prevalence
dt[can10 == 1, lmp_cannabis := 1] # Last month prevalence

dt[ans1 == 1, ltp_narko := 1] # Lifetime narkotiske stoffer
dt[ans2_a == 1, ltp_cocaine := 1] #Cocaine-type drugs
dt[ans2_b == 1, ltp_mdma := 1] #"Ecstasy" type substances
dt[ans2_c == 1, ltp_amphetamines := 1] #Amphetamine-type stimulants
dt[ans2_e == 1, ltp_heroin := 1] #Heroin
dt[ans2_f == 1, ltp_ghb := 1] #Other sedatives and tranquillizers
dt[ans2_g == 1, ltp_lsd := 1] #LSD
dt[ans2_h == 1, ltp_other := 1] #Andre rusmidler noen gang

dt[ans3_1 == 1, lyp_cocaine := 1]
dt[ans3_2 == 1, lyp_mdma := 1]
dt[ans3_3 == 1, lyp_amphetamines := 1]
dt[ans3_5 == 1, lyp_heroin := 1]
dt[ans3_6 == 1, lyp_ghb := 1]
dt[ans3_7 == 1, lyp_lsd := 1]
dt[ans3_8 == 1, lyp_other := 1]

## Any drugs lifetime
anyltpCols <- grep("ltp_", names(dt), value = T)
dt[, ltp_any := as.numeric(rowSums(.SD == 1, na.rm = TRUE) > 0), .SDcols = anyltpCols]

## Any drugs last year
anyCols <- grep("lyp_", names(dt), value = T)
dt[, lyp_any := as.numeric(rowSums(.SD == 1, na.rm = TRUE) > 0), .SDcols = anyCols]

## Exclude all missing and not answered can1 or ans1
## Either age was not between 16-64 yrs
nrow(dt)
dt <- dt[anypop == 1,]

## Sample size for 16-64 and exclude all missing and not answered can1 or ans1
nrow(dt)


## Vekt is character - convert to numeric
## Create standardized weight variable like previous years
## ----------------------------------
# dt[, vekt := as.numeric(gsub(",", ".", vekt))]
# dt[, vekt := vekt2 / mean(vekt2, na.rm = TRUE)]


## ---------------------
## Lifetime  prevalence
## ---------------------

## Kjonn codebook
## ------------
kjonnKB <- data.table::data.table(v1 = 1:2, v2 = c("Male", "Female"))

get_prev(dt, "ltp_any", "anypop") #Anyrug
get_prev(dt, "ltp_cannabis", "canpop") #Cannabis-type drugs
get_prev(dt, "ltp_heroin", "narkpop") #Heroin
get_prev(dt, "ltp_cocaine", "narkpop") #Cocaine-type drugs
get_prev(dt, "ltp_amphetamines", "narkpop") #Amphetamine-type stimulants
get_prev(dt, "ltp_mdma", "narkpop") # "Ecstasy" type substances
get_prev(dt, "ltp_ghb", "narkpop") #Other sedatives and tranquillizers
get_prev(dt, "ltp_lsd", "narkpop") #LSD

## --------------------
## Last year prevalence
## --------------------

get_prev(dt, "lyp_any", "anypop") #Anydrugs
get_prev(dt, "lyp_cannabis", "canpop") #Cannabis-type drugs
get_prev(dt, "lyp_heroin", "narkpop", diagnostic = FALSE) #Heroin
get_prev(dt, "lyp_cocaine", "narkpop") #Cocaine-type drugs
get_prev(dt, "lyp_amphetamines", "narkpop") #Amphetamine-type stimulants
get_prev(dt, "lyp_mdma", "narkpop") #"Ecstasy" type substances
get_prev(dt, "lyp_ghb", "narkpop") #Other sedatives and tranquillizers
get_prev(dt, "lyp_lsd", "narkpop") #LSD

## -------------------------
## Last month prevalence
## -------------------------

get_prev(dt, "lmp_cannabis", "canpop")


## Item 06b - Daily or nearly-daily use (use on 20 days or more past 30 days)
## -----------------------------------------------------------------------------

dt[can11 == 1, can20more := 1]
get_prev(dt, "can20more", "canpop")

## ----------------------------
## Trend dvs. data 2023 og 2024
## ----------------------------

# odrive <- "O:\\Prosjekt\\Rusdata"
# rusdrive <- "Rusundersøkelsen\\Rusus historiske data\\ORG\\alkohol_rusundersokelsen"
# filpath <- file.path(odrive, rusdrive)

# d2023 <- haven::read_dta(file.path(filpath, "Rus2023.dta"))
# d2024 <- readRDS(file.path(odrive, "Rusundersøkelsen", "Rusus 2024","rus2024.rds"))

# setDT(d2023)
# setDT(d2024)

# ## Columnames for andre narkotiske stoffer
# grep("ans|Can", names(d2023), value = T)
# grep("ans|Can", names(d2024), value = T)

# ## need standardized weight and variablenames as in d2023
# meanX <- d2024[, mean(VEKT, na.rm = T)]
# d2024[, nyvekt2 := VEKT/meanX]

# keepVar <- intersect(names(d2023), names(d2024))
# d2023 <- d2023[, ..keepVar][, year := 2023]
# d2024 <- d2024[, ..keepVar][, year := 2024]

# dtx <- data.table::rbindlist(list(d2023, d2024), ignore.attr = TRUE)

rusfolder <- "O:\\Prosjekt\\Rusdata\\Rusundersokelsen\\Datasets"
file <- file.path(rusfolder, "Rusus_samlet/rusus_2012_2025.rds")
DT <- readRDS(file)
dtx <- subset(DT, year %in% c(2024, 2025))

library(torr)
library(data.table)

## Age groups
## -------------
dtx <- dtx[alder %between% c(16,64)]
AgeBrk = c(16, 18, 25, Inf)
AgeLbl = c("16-17", "18-24", "25+")
dtx <- torr::group_age(dtx, var = "alder", breaks = AgeBrk, labels = AgeLbl, new_var = "agecat", copy = F)

### Populasjon
### ----------
# Create cannabis, narko and any drugs population ie. canpop, narkpop and anypop
create_population <- function(dt) {
  data <- data.table::copy(dt)

  data[, canpop := fifelse(can1 %in% 1:2, 1, 0)] #Cannabis
  data[, narkpop := fifelse(ans1 %in% 1:2, 1, 0)] #Other illegal drugs
  data[canpop == 1 | narkpop == 1, anypop := 1][
    is.na(anypop), anypop := 0] # Any illegal drugs

  return(data)
}

dtt <- create_population(dtx)
dtt[, .N, keyby = anypop]

CanVars = c("can1", "can6", "can10")
dtt <- torr::create_cann_pop(dtt, vars = CanVars ) #Dette lager ltpPop_cannabis, lypPop_cannabis, lmpPop_cannabis

dtt <- torr::create_narko_pop(dtt, vars = c("ans2_a", "ans3_1"), val = "kokain") #ltpPop_kokain og lypPop_kokain
dtt <- torr::create_narko_pop(dtt, vars = c("ans2_b", "ans3_2"), val = "mdma") #ltpPop_mdma og lypPop_mdma
dtt <- torr::create_narko_pop(dtt, vars = c("ans2_c", "ans3_3"), val = "amfetaminer")
dtt <- torr::create_narko_pop(dtt, vars = c("ans2_e", "ans3_5"), val = "heroin")
dtt <- torr::create_narko_pop(dtt, vars = c("ans2_f", "ans3_6"), val = "ghb")
dtt <- torr::create_narko_pop(dtt, vars = c("ans2_g", "ans3_7"), val = "lsd")
dtt <- torr::create_narko_pop(dtt, vars = c("ans2_h", "ans3_8"), val = "annet")

## Illegal drugs variables
## ----------------------------------
dtt[can1 == 1, ltp_cannabis := 1] # Lifetime prevalence
dtt[can6 == 1, lyp_cannabis := 1] # Last year prevalence
dtt[can10 == 1, lmp_cannabis := 1] # Last month prevalence

dtt[ans1 == 1, ltp_narko := 1] # Lifetime narkotiske stoffer
dtt[ans2_a == 1, ltp_cocaine := 1] #Cocaine-type drugs
dtt[ans2_b == 1, ltp_mdma := 1] #"Ecstasy" type substances
dtt[ans2_c == 1, ltp_amphetamines := 1] #Amphetamine-type stimulants
dtt[ans2_e == 1, ltp_heroin := 1] #Heroin
dtt[ans2_f == 1, ltp_ghb := 1] #Other sedatives and tranquillizers
dtt[ans2_g == 1, ltp_lsd := 1] #LSD
dtt[ans2_h == 1, ltp_other := 1] #Andre rusmidler noen gang

dtt[ans3_1 == 1, lyp_cocaine := 1]
dtt[ans3_2 == 1, lyp_mdma := 1]
dtt[ans3_3 == 1, lyp_amphetamines := 1]
dtt[ans3_5 == 1, lyp_heroin := 1]
dtt[ans3_6 == 1, lyp_ghb := 1]
dtt[ans3_7 == 1, lyp_lsd := 1]
dtt[ans3_8 == 1, lyp_other := 1]

## Any drugs lifetime
anyltpCols <- grep("ltp_", names(dtt), value = T)
dtt[, ltp_any := as.numeric(rowSums(.SD == 1, na.rm = TRUE) > 0), .SDcols = anyltpCols]

## Any drugs last year
anyCols <- grep("lyp_", names(dtt), value = T)
dtt[, lyp_any := as.numeric(rowSums(.SD == 1, na.rm = TRUE) > 0), .SDcols = anyCols]

dim(dtt)
dtt[, .N, keyby = year]

## Exclude all missing and not answered can1 or ans1
## Either age was not between 16-64 yrs
dd <- dtt[anypop == 1,]
dd[, .N, keyby =  year]

## --------------------
## Last year prevalence
## --------------------

source(file.path(here::here(), "unodc","fun-pct-change.R"))

calc_change(dtt, "lyp_any", "year", "anypop")
calc_change(dtt, "lyp_cannabis", "year", "canpop")
calc_change(dtt, "lyp_heroin", "year", "canpop")
calc_change(dtt, "lyp_cocaine", "year", "canpop")
calc_change(dtt, "lyp_amphetamines", "year", "canpop")
calc_change(dtt, "lyp_mdma", "year", "canpop")
calc_change(dtt, "lyp_ghb", "year", "canpop")
calc_change(dtt, "lyp_lsd", "year", "canpop")

grp <- c("year", "agecat")
calc_change(dtt, "lyp_any", group_vars = grp, "anypop")
calc_change(dtt, "lyp_cannabis", group_vars = grp, "canpop")
calc_change(dtt, "lyp_heroin", group_vars = grp, "canpop")
calc_change(dtt, "lyp_cocaine", group_vars = grp, "canpop")
calc_change(dtt, "lyp_amphetamines", group_vars = grp, "canpop")
calc_change(dtt, "lyp_mdma", group_vars = grp, "canpop")
calc_change(dtt, "lyp_ghb", group_vars = grp, "canpop")
calc_change(dtt, "lyp_lsd", group_vars = grp, "canpop")


calc_percentage(dtt, "lyp_any", "year", weight_var = "nyvekt2", denominator_var = "anypop", include_diagnostics = F, na_treatment =  "as_zero")
calc_percentage(dtt, "lyp_cannabis", "year", weight_var = "nyvekt2", denominator_var = "canpop", include_diagnostics = F, na_treatment = "as_zero" )



### Populasjon
### ----------
create_population <- function(dt) {
  data <- data.table::copy(dt)

  data[, canpop := fifelse(can1 %in% 1:2, 1, 0)] #Cannabis
  data[, narkpop := fifelse(ans1 %in% 1:2, 1, 0)] #Other illegal drugs
  data[canpop == 1 | narkpop == 1, anypop := 1][
    is.na(anypop), anypop := 0] # Any illegal drugs

  return(data)
}

dt <- create_population(dtx)
dt[, .N, keyby = anypop]

CanVars = c("can1", "can6", "can10")
dt <- torr::create_cann_pop(dt, vars = CanVars ) #Dette lager ltpPop_cannabis, lypPop_cannabis, lmpPop_cannabis

dt <- torr::create_narko_pop(dt, vars = c("ans2_a", "ans3_1"), val = "kokain") #ltpPop_kokain og lypPop_kokain
dt <- torr::create_narko_pop(dt, vars = c("ans2_b", "ans3_2"), val = "mdma") #ltpPop_mdma og lypPop_mdma
dt <- torr::create_narko_pop(dt, vars = c("ans2_c", "ans3_3"), val = "amfetaminer")
dt <- torr::create_narko_pop(dt, vars = c("ans2_e", "ans3_5"), val = "heroin")
dt <- torr::create_narko_pop(dt, vars = c("ans2_f", "ans3_6"), val = "ghb")
dt <- torr::create_narko_pop(dt, vars = c("ans2_g", "ans3_7"), val = "lsd")
dt <- torr::create_narko_pop(dt, vars = c("ans2_h", "ans3_8"), val = "annet")

## Illegal drugs variables
## ----------------------------------
dt[can1 == 1, ltp_cannabis := 1] # Lifetime prevalence
dt[can6 == 1, lyp_cannabis := 1] # Last year prevalence
dt[can10 == 1, lmp_cannabis := 1] # Last month prevalence

dt[ans2_a == 1, ltp_cocaine := 1] #Cocaine-type drugs
dt[ans2_b == 1, ltp_mdma := 1] #"Ecstasy" type substances
dt[ans2_c == 1, ltp_amphetamines := 1] #Amphetamine-type stimulants
dt[ans2_e == 1, ltp_heroin := 1] #Heroin
dt[ans2_f == 1, ltp_ghb := 1] #Other sedatives and tranquillizers
dt[ans2_g == 1, ltp_lsd := 1] #LSD
dt[ans2_h == 1, ltp_other := 1] #Andre rusmidler noen gang

dt[ans3_1 == 1, lyp_cocaine := 1]
dt[ans3_2 == 1, lyp_mdma := 1]
dt[ans3_3 == 1, lyp_amphetamines := 1]
dt[ans3_5 == 1, lyp_heroin := 1]
dt[ans3_6 == 1, lyp_ghb := 1]
dt[ans3_7 == 1, lyp_lsd := 1]
dt[ans3_8 == 1, lyp_other := 1]


anyCols <- grep("lyp_", names(dt), value = T)
dt[, anyLYP := as.numeric(rowSums(.SD == 1, na.rm = TRUE) > 0), .SDcols = anyCols]
dt[, lyp_any := fcase(anyLYP == 1, 1,
                       lyp_cannabis == 1, 1,
                       default = 0)]
