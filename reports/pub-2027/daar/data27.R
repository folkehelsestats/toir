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
ratenor1225 <- ratenor[year > 2011]
ratenor1625 <- ratenor[year > 2015]

pred_rate <- function(dt){
  data.table::copy(dt)
  model <- stats::lm(rate ~ year, data = dt)
  dt[, trend := stats::predict(model)]
}

ratenor <- pred_rate(ratenor)
ratenor1225 <- pred_rate(ratenor1225)
ratenor1625 <- pred_rate(ratenor1625)

# Fit a linear trend model
trend_model <- stats::lm(rate ~ year, data = ratenor1625)

# Add fitted trend values
ratenor[, trend := stats::predict(trend_model)]

# View trend statistics
summary(trend_model)

# Figur
make_fig <- function(dt, min, fig, mode = "dynamic"){
  d <- dt[year >= min]
  dtrate <- melt(d, id.vars = "year", measure.vars = c("rate", "trend"))
  spec <- hd_spec(dtrate, x = "year", y = "value", group = "variable")
  opts <- hd_opts(title = "Narkotikautløste dødsfall",
                  subtitle = "Aldersstandardisert rate per 100 000 innbyggere",
                  ylab = "Rate")
  hd_make(spec, fig, opts, mode = mode)
}

make_fig(ratenor, "line")
make_fig(ratenor, "line", "static")

make_fig(ratenor, 2016, "line")
make_fig(ratenor, 2012, "line")

make_fig(ratenor1225, "line")
make_fig(ratenor, "line", "static")

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
