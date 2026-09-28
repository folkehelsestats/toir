## pak::pkg_install("folkehelsestats/torr")

if (!requireNamespace("torr", quietly = T)) install.packages("torr")

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

# source(file.path(here::here(), "euda", "fun-form-style.R"))
source("https://raw.githubusercontent.com/folkehelsestats/toir/refs/heads/main/euda/fun-form-style.R")

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

