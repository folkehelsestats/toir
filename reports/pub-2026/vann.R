## remotes::install_github("folkehelsestats/highdir")

## Skal ikke publiseres før 18.mars
## --------------------------------------------
pkgs <- c("data.table", "rio", "highcharter")
sapply(pkgs, require, character.only = TRUE)

vannFil <- "O:\\Prosjekt\\Rusdata\\illegale_rus/Vann20260305.xlsx"
dt <- rio::import(vannFil, sheet = "thca", skip = 1 ) |> as.data.table()
setDT(dt)

# Extract time labels from row 1
time_labels <- unlist(dt[1, -1])   # skip Ukedag column

# Create new column names: Town_Time
newCols <- c("Ukedag",
              paste0(rep(c("Bergen","Oslo","Trondheim"),
                         each = 4),
                     "_",
                     time_labels))

# Apply names
setnames(dt, newCols)

# Remove the first row (header row)
dt <- dt[-1]

long <- melt(
  dt,
  id.vars = "Ukedag",
  variable.name = "by_tid",
  value.name = "value"
)

## Split byen og tid
long[, c("by", "tid") := tstrsplit(by_tid, "_", fixed = TRUE)]
long[, by_tid := NULL]
long[]

## long[, Ukedag := fifelse(
##   grepl("^Gjennomsnitt", Ukedag),
##   tstrsplit(Ukedag, " ", keep = 2),
##   Ukedag
## )]

long[, Ukedag := as.character(Ukedag)]

# fjern prefikset "Gjennomsnitt" + mellomrom
long[, Ukedag := sub("^Gjennomsnitt\\s+", "", Ukedag)]

# Create a shifted (previous row) version of Ukedag
long[, prev := shift(Ukedag)]

# Now update SD rows
long[Ukedag == "SD", Ukedag := paste0(prev, "SD")]

# Remove helper column
long[, prev := NULL]
long[, value  := as.numeric(value)]

library(highdir)
spec <- hd_spec(long[Ukedag == "ukedag"], x = "tid", y = "value", group = "by")
opts <- hd_opts(title = "THCA")
fg <- hd_make(spec, "column", opts, use_js = F)
fg
