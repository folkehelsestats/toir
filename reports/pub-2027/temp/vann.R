# Avløpsvann 2026 delrapport - R-kode for å lese inn data fra Excel-ark
# ------------------------------------------------------------------------------

library(readxl)
library(data.table)

path <- "C:\\Users\\ykama\\Helsedirektoratet\\ARB-Rusmidler tobakk - analyser og rapportering-HDIR-HDIR-Unntatt offentlighet - EWS - Avløpsvann - Død.reg - Dokumenter\\Avløpsvann delrapport 2026"
file <- grep(pattern = "data_2026", list.files(path, full.names = TRUE), value = TRUE)

path <- "O:\\Prosjekt\\Rusdata\\avlopsvann\\arkiv"
file <- grep(pattern = "data_2026", list.files(path, full.names = TRUE), value = TRUE)

dt <- read_excel(file, sheet = "Ketamin", range = "D3:AV7", col_names = TRUE, skip = 1)
setDT(dt)
