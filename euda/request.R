## Her finnes forskjellige analyser som noen har bedt om
## -----------------------------------------------------

source(file.path(here::here(), "setup.R"))
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-age.R")
source(file.path(here::here(), "unodc","fun-weighted-unweighted.R"))
source(file.path(here::here(), "unodc","fun-prevalence.R"))

## Data 2024
## --------------
mainpath <- "O:\\Prosjekt\\Rusdata"
## DT <- haven::read_dta(file.path("Rusundersøkelsen", "Rusus 2024", "nytt forsøk februar 25 rus24.dta"))
## saveRDS(DT, file.path("Rusundersøkelsen", "Rusus 2024","rus2024.rds"))
DT <- readRDS(file.path(mainpath, "Rusundersøkelsen", "Rusus 2024","rus2024.rds"))
dt <- as.data.table(DT)

## Age groups
## -------------
dt <- group_age_standard(dt, var = "Alder", type = "unodc",
                         new_var = "agecat")

## Bruk Cannabis siste 4 uker
## --------------------------
canVal <- c("20 dager eller mer" = 1,
            "10-19 dager" = 2,
            "4-9 dager" = 3,
            "1-3 dager" = 4)

canKB <- data.table::data.table(v1 = as.integer(canVal),
                                v2 = names(canVal))

canTbl <- dt[, .N, keyby = Can11]
canTbl[canKB, on = c(Can11 = "v1"), val := i.v2]
canTbl[is.na(val), val := "Ingen svar eller missing"]
canTbl[, Can11 := NULL]
data.table::setcolorder(canTbl, c("val", "N"))
gtsave(gt::gt(canTbl), "can20.rtf")



ageTbl <- dt[, .N, keyby = agecat]
gtsave(gt::gt(ageTbl), "euda/agegrp.rtf")

canAge <- dt[, .N, keyby = .(agecat, Can11)]
canAge[canKB, on = c(Can11 = "v1"), val := i.v2]
canAge[is.na(val), val := "Ingen svar eller missing"]
canAge[, Can11 := NULL]
data.table::setcolorder(canAge, c("agecat", "val", "N"))
gtsave(gt::gt(canAge), "euda/canAge20.rtf")

dt[, .N]
