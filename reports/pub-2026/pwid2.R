
## Skal ikke publiseres før 18.mars
## --------------------------------------------
pkgs <- c("data.table", "rio", "highdir")
sapply(pkgs, require, character.only = TRUE)

fwid <- "O:\\Prosjekt\\Rusdata\\PWID\\pwid_rolling_mean.xlsx"
dt <- rio::import(fwid, sheet = "Ark1") |> as.data.table()

# Data med N og rolling # --------------------------------------------

years <- as.character(2012:2024)

long <- data.table::melt(
  dt,
  id.vars = "col",
  measure.vars = years_present,
  variable.name = "year",
  value.name = "value",
  variable.factor = FALSE
)

long[, year := as.integer(year)]

# Endre legend -------------------------------------------------------
sproy <- c("N" = "Estimat antall sprøytebrukere",
           "roll" = "Glidende estimat antall sprøytebrukere")

long[, cols := sproy[col]]

title1 <- "Estimert antall personer som injiserer rusmidler (PWID) i Norge"
subtitle1 <- "Basert på data 2019–2024 • 3-årig glidende gjennomsnitt"
specpwid <- hd_spec(data = long, x = "year", y = "value", group = "cols")
optspwid <- hd_opts(xlab = NULL, ylab = "Antall", yint = 2000, title = title1,
                    subtitle = subtitle1)

hd_make(specpwid, "line", optspwid)


## For bare data som har CI ------------------------------------------


pwid[, var := factor(var, labels = c("dod", "sproy", "sd", "upCI", "lowCI"))]

# 1. Melt wide → long
long_dt <- melt(
  pwid[, -1],
  id.vars = "var",
  variable.name = "year",
  value.name = "value",
  variable.factor = FALSE
)

# 2. Cast long → wide: var becomes columns
wide_dt <- dcast(
  long_dt,
  year ~ var,
  value.var = "value"
)

wide_dt[, year := as.numeric(year)]
cols <- names(wide_dt)[-1]

wide_dt[, (cols) := lapply(.SD, function(x) round(x, 0)), .SDcols = cols]


title1 <- "Estimert antall personer som injiserer rusmidler (PWID) i Norge"
subtitle1 <- "Basert på data 2019–2024 • 3-årig glidende gjennomsnitt"

specs <- highdir::hd_spec(data = wide_dt,
                          x = "year",
                          y = "sproy")

opts <- highdir::hd_opts(title = title1, subtitle = subtitle1, xlab = NULL, ylab = "Antall", yint = 2000, )

pwid <- highdir::hd_make(specs, "arearange", opts, backend = "highcharter", ymin = "lowCI", ymax = "upCI") |>
    hc_legend(enabled = FALSE)

highdir::hd_save(pwid, "img/pwid.html")
