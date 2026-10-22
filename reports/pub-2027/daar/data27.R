# ------------------------------------------------------------------------------
# Datasettet er fra 2026, men publiseringen er for 2027 rapport
# ------------------------------------------------------------------------------
library(data.table)
library(rio)
library(highdir)

path24 <- "C:\\Users\\ykama\\OneDrive - Helsedirektoratet\\dataset\\daar\\data2024"
path <- "C:\\Users\\ykama\\OneDrive - Helsedirektoratet\\dataset\\daar\\data2025"


## Antall og rater
## ---------------
filRate24 <- "Narkotikautløste dødsfall etter bofylke og dødsårsak, rater norge fylke.csv"
filN24 <- "antall2024.csv"
filRate <- "Narkotikautløste dødsfall etter bofylke og dødsårsak, rater.csv"
filN <- "Narkotikautløste dødsfall etter bofylke og dødsårsak, antall.csv"


drate24 <- fread(file.path(path24, filRate24), encoding = "Latin-1")
dnum24 <- setDT(rio::import(file.path(path24, filN24), encoding = "Latin-1"))
drate <- fread(file.path(path, filRate), encoding = "Latin-1")
dnum <- fread(file.path(path, filN), encoding = "Latin-1")

keep_norway <- function(dt, col = c("rate", "n")) {
  data.table::copy(dt)
  dt <- dt[Bofylke == "Hele landet", c(1, 4)]
  data.table::setnames(dt, c("year", col))
  return(dt)
}

ratenor24 <- keep_norway(drate24, "rate")
numnor24  <- keep_norway(dnum24, "n")
ratenor   <- keep_norway(drate, "rate")
numnor    <- keep_norway(dnum, "n")


## Beholder den nyeste data fra 2025
yrmin <- ratenor[, min(year)]
ratenor24 <- ratenor24[year < yrmin]

ratenor <- rbindlist(list(ratenor24, ratenor), use.names = TRUE)

pred_rate <- function(dt){
  data.table::copy(dt)
  model <- stats::lm(rate ~ year, data = dt)
  dt[, trend := stats::predict(model)]
}

# Figur
make_fig <- function(dt, min, fig, mode = "dynamic"){
  d <- dt[year >= min]
  d <- pred_rate(d)
  dtrate <- melt(d, id.vars = "year", measure.vars = c("rate", "trend"))
  spec <- hd_spec(dtrate, x = "year", y = "value", group = "variable")
  opts <- hd_opts(title = "Narkotikautløste dødsfall",
                  subtitle = "Aldersstandardisert rate per 100 000 innbyggere",
                  ylab = "Rate")
  fig <- hd_make(spec, fig, opts, mode = mode)
  list(data = d, fig = fig)
}

d16 <- make_fig(ratenor, 2016, "line")
d12 <- make_fig(ratenor, 2012, "line")
d96 <- make_fig(ratenor, 1996, "line")

# Fit a linear trend model
trend_model <- stats::lm(rate ~ year, data = ratenor1625)
predict(trend_model, interval = "confidence") #for CI

# Add fitted trend values
ratenor[, trend := stats::predict(trend_model)]

# View trend statistics
summary(trend_model)

## ----------------------------------------------------------------------------
## Alder
## ----------------------------------------------------------------------------

# Alderskategorier
ageCat <- "Narkotikautløste dødsfall etter alder og dødsårsak, antall.csv"
dagecat <- fread(file.path(path, ageCat), encoding = "Latin-1")

dagecat <- dagecat[, c(1,2,4)]
setnames(dagecat, c("year", "alder", "antall"))

spec <- hd_spec(dagecat, x = "year", y = "antall", group = "alder")
opts <- hd_opts(title = "Narkotikautløste dødsfall etter alderskategorier",
                subtitle = " Antall dødsfall fordelt på ulike aldersgrupper",
                ylab = "Antall", yint = 20)

hd_make(spec, "line", opts)


# Gjennomsnitt og median alder per kjønn
# --------------------------------------
ageMean <- "Gjennomsnittsalder2021_2025.xlsx"
ageMed <- "Median alder 2021-2025.xlsx"

dmean <- setDT(rio::import(file.path(path, ageMean), encoding = "Latin-1"))
dmedian <- setDT(rio::import(file.path(path, ageMed), encoding = "Latin-1"))

dage <- cbind(dmean, dmedian[, 3])
# dage[, ARGANG := factor(ARGANG, levels = sort(unique(ARGANG)))]

spec <- hd_spec(dage, x = "ARGANG", y = "GJENNOMSNITT_ALDER", group = "KJONN")
opts <- hd_opts(title = "Gjennomsnittsalder ved narkotikautløste dødsfall",
                subtitle = "Total og etter kjønn, 2020-2024",
                ylab = "Antall")

hd_make(spec, "line", opts)

library(highcharter)

highcharter::highchart() |>
  highcharter::hc_add_series(
    data = dage,
    type = "spline",
    hcaes(x = "ARGANG", y = "GJENNOMSNITT_ALDER", group = "KJONN")
  ) |>
  highcharter::hc_xAxis(
    title = list(text = "Årgang"),
    tickInterval = 1,
    allowDecimals = FALSE
  )
