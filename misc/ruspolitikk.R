## spørsmål fra Tryggere Ruspolitikk

## Data 2024
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
## Denominator
## ------------
dt[, canpop := fcase(Can1 %in% 1:2, 1,
                     default = 0)]

dt[, narkpop := fcase(Ans1 %in% 1:2, 1,
                      default = 0)]

dt[canpop == 1 | narkpop == 1, anypop := 1][
  is.na(anypop), anypop := 0]

## Exclude all missing and not anwered
dt <- dt[anypop == 1,]

dt[Can1 == 1, ltp_cannabis := 1] #Cannabis-type drugs
dt[Ans1 == 1, ltp_other := 1]

## Any drug
dt[, ltp_any := fcase(ltp_other == 1, 1,
                      ltp_cannabis == 1, 1,
                      default = 0)]

breaks = c(16, 25, Inf)
labels = c("16-24", "25+")

dd24 <- group_age(dt = dt, "Alder", breaks = breaks, labels = labels, new_var = "age24")
dd24[, .N, keyby = ltp_any]


calc_percentage(dt=dd24,
                outcome_var = "ltp_any",
                group_vars = "age24",
                weight_var = "VEKT",
                denominator_var = "anypop",
                na_treatment = "as_zero",
                round_digits = 1,
                include_diagnostics = FALSE)

calc_percentage(dt=dd24,
                outcome_var = "ltp_any",
                group_vars = "age24",
                weight_var = NULL,
                denominator_var = "anypop",
                na_treatment = "as_zero",
                round_digits = 1,
                include_diagnostics = FALSE)

## Rusundersøkelse 2023
## --------------------
source(file.path(here::here(), "setup.R"))
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-age.R")

## readClipboard()
odrive <- "O:\\Prosjekt\\Rusdata"
rusdrive <- "Rusundersøkelsen\\Rusus historiske data\\ORG\\alkohol_rusundersokelsen"
filpath <- file.path(odrive, rusdrive)
filer <- grep("dta$", list.files(filpath), value = TRUE)

dt23 <- haven::read_dta(file.path(filpath, grep("2023", filer, value = T)))
setDT(dt23)

## Denominator
## ------------
dt23[Can1 %in% 1:2, canpop := 1]
dt23[Ans1 %in% 1:2, narkpop := 1]
dt23[canpop == 1 | narkpop == 1, anypop := 1]

## Lifetime  prevalence
## ---------------------
## Bør sjekke tekst fra Ans2sps
## dt23[, .N, keyby = Ans2sps][!grep("9999", Ans2sps)]

dt23[Can1 == 1, ltp_cannabis := 1]
dt23[Ans1 == 1, ltp_any := 1]
dt23[Ans2_a == 1, ltp_cocaine := 1]
dt23[Ans2_b == 1, ltp_mdma := 1]
dt23[Ans2_c == 1, ltp_amphetamines := 1]
dt23[Ans2_d == 1, ltp_relevin := 1]
dt23[Ans2_e == 1, ltp_heroin := 1]
dt23[Ans2_f == 1, ltp_ghb := 1]
dt23[Ans2_g == 1, ltp_lsd := 1]

## Any illicit drugs
dt23[anypop == 1, ltp_anydrugs := 0]
dt23[ltp_cannabis == 1 | ltp_any == 1, ltp_anydrugs := 1]

dd23 <- group_age(dt23, "Alder", breaks = breaks, labels = labels, new_var = "age24")
dd23[, .N, keyby = ltp_anydrugs]


calc_percentage(dt=dd23,
                outcome_var = "ltp_anydrugs",
                group_vars = "age24",
                weight_var = "vekt2",
                denominator_var = "anypop",
                na_treatment = "as_zero",
                round_digits = 1,
                include_diagnostics = FALSE)

calc_percentage(dt=dd23,
                outcome_var = "ltp_anydrugs",
                group_vars = "age24",
                weight_var = "vekt1",
                denominator_var = "anypop",
                na_treatment = "as_zero",
                round_digits = 1,
                include_diagnostics = FALSE)

calc_percentage(dt=dd23,
                outcome_var = "ltp_anydrugs",
                group_vars = "age24",
                weight_var = "nyvekt2",
                denominator_var = "anypop",
                na_treatment = "as_zero",
                round_digits = 1,
                include_diagnostics = FALSE)

calc_percentage(dt=dd23,
                outcome_var = "ltp_anydrugs",
                group_vars = "age24",
                weight_var = NULL,
                denominator_var = "anypop",
                na_treatment = "as_zero",
                round_digits = 1,
                include_diagnostics = FALSE)
