## Path to data and standard setup
## --------------------------------------------------
options(scipen = 999) # disable scientific notation
source("https://raw.githubusercontent.com/folkehelsestats/rusus/refs/heads/main/folder-path.R")
source("https://raw.githubusercontent.com/folkehelsestats/rusus/refs/heads/main/setup.R")

## Functions
## --------------------------------------------------
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-age.R")
source("https://raw.githubusercontent.com/fyrtaarn/fyr/91dbf471b6454e08bdd783d0c04329b2a562e053/R/utils.R") #is_encode() and is_delete_index()
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-percent-weighted.R")
source(here::here("unodc","fun-weighted-unweighted-ci.R"))
source(here::here("unodc","fun-weighted-unweighted-ci-flexible.R"))
source(here::here("unodc","fun-weighted-unweighted-ci-rolling.R"))

source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-ci-graph.R")
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-graph.R")


## Data 2012 - 2024
## --------------------------------------------------
ddt <- readRDS(file.path(Rususdata, "rusus_2012_2024.rds"))

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

## Rusus 2012-2024 includes a fake drug "Relevin" to identify non-serious respondents.
ddt <- exclude_relevin(ddt)

## Rusus 2025 includes a fake drug "Relevin" to identify non-serious respondents.
DT25 <- exclude_relevin(DT25)

## Age groups
## -------------
## In 2025 the questions about drug use were asked to all ages 16-79.
## To be comparable with previous years, we only include ages 16-64.
DT25 <- DT25[alder <= 64]
DT25 <- group_age_standard(DT25,
                           var = "alder",
                           type = "rusund",
                           new_var = "agecat")

## Vekt is character - convert to numeric
## ----------------------------------
DT25[, vekt := as.numeric(gsub(",", ".", vekt))]


## ==================================
## Exclude all missing and not answered Can1 or Ans1
## Denominator for Illegal rusmidler
## ----------------------------------

create_population <- function(dt) {
  data <- data.table::copy(dt)

  data[, canpop := fifelse(can1 %in% 1:2, 1, 0)] #Cannabis
  data[, narkpop := fifelse(ans1 %in% 1:2, 1, 0)] #Other illegal drugs
  data[canpop == 1 | narkpop == 1, anypop := 1][
    is.na(anypop), anypop := 0] # Any illegal drugs

  return(data)
}

DT <- create_population(DT25)

## Free text - Other types
## Bør sjekke tekst fra Ans2sps
DT[, ans2sps := is_encode(ans2sps)]
DT[, .N, keyby = ans2sps][!grep("9999", ans2sps)]

DT[grep("ketamin", ans2sps, ignore.case = TRUE), "AndreKetamin" := 1]

## ==================================
## Merge data 2012-2024 and 2025
## ----------------------------------
DT[, year := 2025]
ddt[, vekt := nyvekt2]

commonCols <- intersect(names(ddt), names(DT))

# clean labelled attributes coz it makes troubles
dtx <- ddt[, lapply(.SD, \(col) {
  attributes(col) <- NULL
  col
})]


DTT <- rbindlist(list(dtx[, ..commonCols], DT[, ..commonCols]), use.names = TRUE, fill = TRUE)

DTT <- create_population(DTT)

## Age groups
## -------------
## only those 16-64 years old being asked about drug use 2012-2024
DTT <- DTT[alder <= 64]
DTT <- group_age_standard(DTT,
                          var = "alder",
                          type = "rusund",
                          new_var = "agecat")

