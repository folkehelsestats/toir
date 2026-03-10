## remotes::install_github("folkehelsestats/highdir")
remotes::install_github("folkehelsestats/highdir@dev")

## Skal ikke publiseres før 18.mars
## --------------------------------------------
pkgs <- c("data.table", "rio", "highdir")
sapply(pkgs, require, character.only = TRUE)

vannFil <- "O:\\Prosjekt\\Rusdata\\illegale_rus/avlopvann_20260304.xlsx"
source("~/Git-hdir/toir/reports/pub-2026/fun-avlopvann.R")

vannFil <- "O:\\Prosjekt\\Rusdata\\illegale_rus/avlopvann_20260304.xlsx"
thca <- vann_data(vannFil, "thca")

spec <- hd_spec(thca[Ukedag == "uke"], x = "by", y = "val", group = "tid")
opts <- hd_opts(title = "THCA")
fg <- hd_make(spec, "column", opts, use_js = F)
fg
