## Path to data and standard setup
## --------------------------------------------------
options(scipen = 999) # disable scientific notation
source("https://raw.githubusercontent.com/folkehelsestats/rusus/refs/heads/main/folder-path.R")
source("https://raw.githubusercontent.com/folkehelsestats/rusus/refs/heads/main/setup.R")

## Functions
## --------------------------------------------------
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-age.R")
source("https://raw.githubusercontent.com/fyrtaarn/fyr/91dbf471b6454e08bdd783d0c04329b2a562e053/R/utils.R") #is_encode() and is_delete_index()

## Data 2012 - 2024
## --------------------------------------------------
ddt <- readRDS(file.path(Rususdata, "Rusus_2012_2023", "data_2012_2024.rds"))

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

## Age groups
## -------------
DT25 <- group_age_standard(DT25, var = "alder", type = "rusund",
                         new_var = "agecat")

## Relevin exclusion
## ------------------
## OBS! Exclude those that used Relevin Ans2_d == 1

ans2 <- grep("ans2_", names(DT25), ignore.case = TRUE, value = TRUE)
ansTmp <- paste0(ans2, "_tmp")

DT25[, (ansTmp) := lapply(.SD, function(x) {
  fcase(
    x == 1, 1,
    x %in% c(2, 8, 9), NA,
    default = NA
  )
}), .SDcols = ans2]

DT25[, ansSum := rowSums(.SD, na.rm = TRUE), .SDcols = ansTmp]
# Remove ONLY those who answered yes to all drugs including Relevin
# Relevin is a non-existing drug, used to identify non-serious respondents
DT25 <- DT25[ansSum != 8]
DT25[, (ansTmp) := NULL]


## Exclude all missing and not answered Can1 or Ans1
## Denominator for Illigal rusmidler
## ------------
DT25[, canpop := fifelse(can1 %in% 1:2, 1, 0)] #Cannabis
DT25[, narkpop := fifelse(ans1 %in% 1:2, 1, 0)] #Other illegal drugs
DT25[canpop == 1 | narkpop == 1, anypop := 1][
  is.na(anypop), anypop := 0] # Any illegal drugs

DT <- DT25[anypop == 1,]

## Free text - Other types
## Bør sjekke tekst fra Ans2sps
DT[, ans2sps := is_encode(ans2sps)]
DT[, .N, keyby = ans2sps][!grep("9999", ans2sps)]

DT[grep("ketamin", ans2sps, ignore.case = TRUE), "AndreKetamin" := 1]
