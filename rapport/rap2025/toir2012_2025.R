## Hvor dataene for 2012 til 2025 ligger
## --------------------------------------------------
pathToir <- file.path(here::here("rapport", "rap2025"))
source(file.path(pathToir, "setup.R"))

## ddt # data 2012-2024 er lastet i setup.R
## DT  # data 2025 er lastet i setup.R

DT[, year := 2025]

commonCols <- intersect(names(ddt), names(DT))

# clean labelled attributes coz it makes troubles
dtx <- ddt[, lapply(.SD, \(col) {
  attributes(col) <- NULL
  col
})]


DTT <- rbindlist(list(dtx[, ..commonCols], DT[, ..commonCols]), use.names = TRUE, fill = TRUE)
## ==================================
## Cannabis
## ----------------------------------
