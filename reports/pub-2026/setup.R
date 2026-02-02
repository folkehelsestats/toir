## Path to data and standard setup
## --------------------------------------------------
options(scipen = 999) # disable scientific notation
source("https://raw.githubusercontent.com/folkehelsestats/rusus/refs/heads/main/folder-path.R")
source("https://raw.githubusercontent.com/folkehelsestats/rusus/refs/heads/main/setup.R")

## Functions
## --------------------------------------------------
## source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-age.R")
source("https://raw.githubusercontent.com/fyrtaarn/fyr/91dbf471b6454e08bdd783d0c04329b2a562e053/R/utils.R") #is_encode() and is_delete_index()
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-percent-weighted.R")
source(here::here("unodc","fun-weighted-unweighted-ci.R"))
source(here::here("unodc","fun-weighted-unweighted-ci-flexible.R"))
source(here::here("unodc","fun-weighted-unweighted-ci-rolling.R"))

source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-ci-graph.R")
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-graph.R")


## --------------------------------------------------
ddt <- readRDS(file.path(Rususdata, "rusus_2012_2024.rds"))

## Remove 2012 data for drug use questions due to error in filter from SSB
## See email from Elin https://github.com/folkehelsestats/toa/blob/main/misc/missing-etc.org
## ------------------------------
ddt <- ddt[year != 2012]

## ## Data 2024
## ## --------------------------------------------------
## DT24 <- readRDS(file.path(Rususdata, "Rusus_2024","rus2024.rds"))
## dt24 <- data.table::as.data.table(DT)

## Data 2025
## --------------------------------------------------
DT25 <- readRDS(file = file.path(Rususdata, "Rusus_2025", "rusus2025_20251126.rds"))
setDT(DT25)
setnames(DT25, names(DT25), tolower(names(DT25)))

## ==================================
## Data preparation
## ----------------------------------
# clean labelled attributes coz it makes troubles
DT25[, (names(DT25)) := lapply(.SD, \(x) { attributes(x) <- NULL; x })]

## =================================
## OBS!! Etter møte med FHI 19.jan 2026 så er det avklart at de som svarte bruk
## av Relevin skal IKKE ekskluderes likevel
## ==================================
## Relevin exclusion
## ------------------
## OBS! Relevin ie. Ans2_d == 1 is not an illegal drug, but a fake drug
## Remove ONLY those who answered yes to all drugs including Relevin
## Relevin is a non-existing drug, used to identify non-serious respondents

exclude_relevin <- function(dt) {
  data <- data.table::copy(dt)
  ans2_var <- grep("ans2_", names(data), ignore.case = TRUE, value = TRUE)
  ans2_tmp <- paste0(ans2_var, "_tmp")

  data[, (ans2_tmp) := lapply(.SD, function(x) {
    fcase(
      x == 1, 1,
      x %in% c(2, 8, 9), NA,
      default = NA
    )
  }), .SDcols = ans2_var]

  data[, ansSum := rowSums(.SD, na.rm = TRUE), .SDcols = ans2_tmp]

  data <- data[ansSum != 8]

  data[, (ans2_tmp) := NULL]
  data[, ansSum := NULL]

  return(data)
}

## ## Rusus 2012-2024 includes a fake drug "Relevin" to identify non-serious respondents.
## ddt <- exclude_relevin(ddt)

## ## Rusus 2025 includes a fake drug "Relevin" to identify non-serious respondents.
## DT25 <- exclude_relevin(DT25)


## Age groups
## -------------
## In 2025 the questions about drug use were asked to all ages 16-79.
## To be comparable with previous years, we only include ages 16-64.
DT25 <- DT25[alder <= 64]
DT25 <- torr::group_age_standard(DT25,
                                 var = "alder",
                                 type = "rusund",
                                 new_var = "agecat")

## Vekt is character - convert to numeric
## Create standardized weight variable like previous years
## ----------------------------------
DT25[, vektnr := as.numeric(gsub(",", ".", vekt))]
DT25[, vekt := vektnr / mean(vektnr, na.rm = TRUE)]


## Free text - Other types
## OBS!! Bør sjekke tekst fra Ans2sps
DT25[, ans2sps := is_encode(ans2sps)]
DT25[, .N, keyby = ans2sps][!grep("9999", ans2sps)]

DT25[grep("ketamin", ans2sps, ignore.case = TRUE), "AndreKetamin" := 1]

## ==================================
## Merge data 2012-2024 and 2025
## ----------------------------------
DT25[, year := 2025]
ddt[, vekt := nyvekt2]

commonCols <- intersect(names(ddt), names(DT25))

# clean labelled attributes coz it makes troubles
dtx <- ddt[, lapply(.SD, \(col) {
  attributes(col) <- NULL
  col
})]

## Age groups
## -------------
## only those 16-64 years old being asked about drug use 2012-2024
dtx <- dtx[alder <= 64] #Only 16-64 years old included
DD <- rbindlist(list(dtx[, ..commonCols], DT25[, ..commonCols]), use.names = TRUE, fill = TRUE)

DD <- torr::group_age_standard(DD,
                                var = "alder",
                                type = "rusund",
                                new_var = "agecat")

## Kjonn variable
## -------------
DD[, kjonnSTR := factor(kjonn, levels = c(1, 2), labels = c("Menn", "Kvinner"))]

## ==================================
## Exclude all missing and not answered Can1 or Ans1 used to create
## denominator for Illegal rusmidler for general calculation
## ----------------------------------

create_population <- function(dt) {
  data <- data.table::copy(dt)

  data[, canpop := fifelse(can1 %in% 1:2, 1, 0)] #Cannabis
  data[, narkpop := fifelse(ans1 %in% 1:2, 1, 0)] #Other illegal drugs
  data[canpop == 1 | narkpop == 1, anypop := 1][
    is.na(anypop), anypop := 0] # Any illegal drugs

  return(data)
}

DD <- create_population(DD)

CanVars = c("can1", "can6", "can10")
DTT <- torr::create_cann_pop(DD, vars = CanVars )

DTT <- torr::create_narko_pop(DTT, vars = c("ans2_a", "ans3_1"), val = "kokain")
DTT <- torr::create_narko_pop(DTT, vars = c("ans2_b", "ans3_2"), val = "mdma")
DTT <- torr::create_narko_pop(DTT, vars = c("ans2_c", "ans3_3"), val = "amfetaminer")
DTT <- torr::create_narko_pop(DTT, vars = c("ans2_e", "ans3_5"), val = "heroin")
DTT <- torr::create_narko_pop(DTT, vars = c("ans2_f", "ans3_6"), val = "ghb")
DTT <- torr::create_narko_pop(DTT, vars = c("ans2_g", "ans3_7"), val = "lsd")
DTT <- torr::create_narko_pop(DTT, vars = c("ans2_h", "ans3_8"), val = "annet")

## ## Alternative---
## vars_list <- list(
##   c("ans2_a", "ans3_1"),
##   c("ans2_b", "ans3_2"),
##   c("ans2_c", "ans3_3"),
##   c("ans2_e", "ans3_5"),
##   c("ans2_f", "ans3_6"),
##   c("ans2_g", "ans3_7"),
##   c("ans2_h", "ans3_8")
## )
## vals <- c("kokain", "mdma", "amfetaminer", "heroin", "ghb", "lsd", "annet")

## for (i in seq_along(vals)) {
##   DD <- torr::create_narko_pop(DD, vars = vars_list[[i]], val = vals[i])
## }
## Illegal drugs variables
## ----------------------------------
DTT[can1 == 1, ltp_cannabis := 1] # Lifetime prevalence
DTT[can6 == 1, lyp_cannabis := 1] # Last year prevalence
DTT[can10 == 1, lmp_cannabis := 1] # Last month prevalence

DTT[ans2_a == 1, ltp_cocaine := 1] #Cocaine-type drugs
DTT[ans2_b == 1, ltp_mdma := 1] #"Ecstasy" type substances
DTT[ans2_c == 1, ltp_amphetamines := 1] #Amphetamine-type stimulants
DTT[ans2_e == 1, ltp_heroin := 1] #Heroin
DTT[ans2_f == 1, ltp_ghb := 1] #Other sedatives and tranquillizers
DTT[ans2_g == 1, ltp_lsd := 1] #LSD
DTT[ans2_h == 1, ltp_other := 1] #Andre stoffer noen gang

DTT[ans3_1 == 1, lyp_cocaine := 1]
DTT[ans3_2 == 1, lyp_mdma := 1]
DTT[ans3_3 == 1, lyp_amphetamines := 1]
DTT[ans3_5 == 1, lyp_heroin := 1]
DTT[ans3_6 == 1, lyp_ghb := 1]
DTT[ans3_7 == 1, lyp_lsd := 1]
DTT[ans3_8 == 1, lyp_other := 1]



