library(data.table)
library(rio)

vann_data <- function(vannFil, sheet = c("thca", "kokain", "amfetaminer", "metamfetamin", "mdma", "ketamin")) {

  sheet <- match.arg(sheet)

  # --- Import og grunnoppsett ----------------------------------------------
  dt <- suppressMessages(
      rio::import(vannFil, sheet = sheet, skip = 1) |>
          data.table::as.data.table()
  )

  data.table::setDT(dt)

  # --- Ekstraher tidsnavn fra rad 1 (uten Ukedag) ---------------------------
  time_labels <- unlist(dt[1, -1])

  # --- Lag nye kolonnenavn --------------------------------------------------
  newCols <- c(
    "Ukedag",
    paste0(
      rep(c("Bergen", "Oslo", "Trondheim"), each = 4),
      "_",
      time_labels
    )
  )

  data.table::setnames(dt, newCols)

  # --- Fjern første rad (med V24, H24 ...) ----------------------------------
  dt <- dt[-1]

  # --- Long-format -----------------------------------------------------------
  long <- data.table::melt(
    dt,
    id.vars = "Ukedag",
    variable.name = "by_tid",
    value.name = "value"
  )

  # --- Split by og tid -------------------------------------------------------
  long[, c("by", "tid") :=
         data.table::tstrsplit(by_tid, "_", fixed = TRUE)]

  # --- Splitt V/H + årstall --------------------------------------------------
  long[, c("s", "t") :=
         data.table::tstrsplit(
           tid,
           "(?<=[A-Za-z])(?=[0-9])",
           perl = TRUE
         )]

  long[, t2 := data.table::fifelse(s == "V", "Vår", "Høst")]
  long[, tid := paste0(t2, " 20", t)]

  # --- Rens Ukedag (fjern 'Gjennomsnitt') -----------------------------------
  long[, Ukedag := sub("^Gjennomsnitt\\s+", "", Ukedag)]

  # --- Behandle SD-rader -----------------------------------------------------
  long[, prev := data.table::shift(Ukedag)]
  long[Ukedag == "SD", Ukedag := paste0(prev, "SD")]

  # --- Numeric + avrunding ---------------------------------------------------
  long[, value := as.numeric(value)]
  long[, val := round(value, 0)]

  # --- Fjern hjelpekolonner --------------------------------------------------
  colx <- c("by_tid", "value", "s", "t", "t2", "prev")
  long[, (colx) := NULL]

  return(long[])
}
