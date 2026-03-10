## remotes::install_github("folkehelsestats/highdir")
remotes::install_github("folkehelsestats/highdir@dev")

## Skal ikke publiseres før 18.mars
## --------------------------------------------
pkgs <- c("data.table", "rio", "highdir", "highcharter")
sapply(pkgs, require, character.only = TRUE)

vannFil <- "O:\\Prosjekt\\Rusdata\\illegale_rus/avlopvann_20260304.xlsx"
source("~/Git-hdir/toir/reports/pub-2026/fun-avlopvann.R")

thca <- vann_data(vannFil, "thca")

thcaUke <- thca[ukedag == "uke"]
spec <- hd_spec(thcaUke, x = "by", y = "val", group = "tid")
opts <- hd_opts(title = "THCA", ylab = "mg THCA/1000p/dag")
fg <- hd_make(spec, "column", opts, use_js = F)
fg

week <- c("Tir", "Ons", "Tor", "Fre", "Lør", "Søn", "Man", "Tir")
wDT <- thca[ukedag %chin% week]
wDT[, id := seq_len(.N), by = .(by, tid)]
wDT[, id := id - 1L] #highcart index starts from 0

specBgn <- hd_spec(wDT[by == "Bergen"], x = "id", y = "val", group ="tid")
optsBgn <- hd_opts(title = "Bergen", xtick = "ukedag", xlab = NULL, ylab = "mg THCA/1000p/dag")

hd_make(specBgn, "line", optsBgn)



kokain <- vann_data(vannFil, "kokain")

kokainUke <- kokain[ukedag == "uke"]
















## for SD linje --------------------------

kokain <- vann_data(vannFil, "kokain")
kokainUke <- kokain[ukedag == "uke"][, id := .I]
kokainUkeSD <- kokain[ukedag == "ukeSD"][, id := .I]

colSD <- c("id", "val")
sdx <- kokainUkeSD[, ..colSD]
sdx[, sd := val][, val := NULL]
kokainUke[sdx, on = "id", sd := i.sd]
kokainUke[, `:=`(low = val - sd, up = val + sd)]


spec <- hd_spec(kokainUke, x = "by", y = "val", group = "tid")
opts <- hd_opts(title = "Kokain")
fg <- hd_make(spec, "column", opts, use_js = F)
fg

fg |>
  hc_add_series(
    kokainUke
  )
