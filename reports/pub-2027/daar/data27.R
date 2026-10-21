# ------------------------------------------------------------------------------
# Datasettet er fra 2026, men publiseringen er for 2027 rapport
# ------------------------------------------------------------------------------
library(data.table)
library(rio)
library(highdir)

path <- "C:\\Users\\ykama\\OneDrive - Helsedirektoratet\\dataset\\daar\\data2025"


## Antall og rater
## ---------------
filN <- "Narkotikautløste dødsfall etter bofylke og dødsårsak, antall.csv" 
filRate <- "Narkotikautløste dødsfall etter bofylke og dødsårsak, rater.csv"

drate <- fread(file.path(path, filRate), encoding = "Latin-1")
dnum <- fread(file.path(path, filN), encoding = "Latin-1")

ratenor <- drate[Bofylke == "Hele landet", c(1,4)]
numnor <- dnum[Bofylke == "Hele landet", c(1,4)]

setnames(ratenor, names(ratenor), c("yr", "rate"))
setnames(numnor, names(numnor), c("yr", "n"))

## Annual percent change
## ---------------------
# log linear model for anual percent change
model <- stats::lm(log(rate) ~ yr, data = ratenor)

apc <- (exp(stats::coef(model)["yr"]) - 1) * 100
apc
# apc = 2.2 dvs. rate i gjennonsnitt øker 2.2% per år fra 216 til 2025

# Annual change
# Calculate annual absolute and percent change
ratenor[, annual_change := rate - data.table::shift(rate)]

ratenor[, annual_pct_change :=
      (rate - data.table::shift(rate)) /
      data.table::shift(rate) * 100]

# Calculate 3-year moving average
ratenor[, ma3 := data.table::frollmean(
  x = rate,
  n = 3,
  align = "right"
)]

# Calculate absolute change in 3-year moving average
ratenor[, ma3_change := ma3 - data.table::shift(ma3)]

# Overall change
r25 <- ratenor[yr == 2025, rate]
r16 <- ratenor[yr == 2016, rate]

(r25 - r16)/r16*100

# Fit linear trend
model <- stats::lm(rate ~ yr, data = ratenor)

# Annual change in rate per 100,000
coef(model)["yr"]
summary(model)


## ----------------------------------------------------------------------------
## Alder
## ----------------------------------------------------------------------------

# Alderskategorier
ageCat <- "Narkotikautløste dødsfall etter alder og dødsårsak, antall.csv"
dagecat <- fread(file.path(path, ageCat), encoding = "Latin-1")


# Gjennomsnitt og median
ageMean <- "Gjennomsnittsalder2021_2025.xlsx"
ageMed <- "Median alder 2021-2025.xlsx"

dmean <- setDT(rio::import(file.path(path, ageMean), encoding = "Latin-1"))
dmedian <- setDT(rio::import(file.path(path, ageMed), encoding = "Latin-1"))

dage <- cbind(dmean, dmedian[, 3])
