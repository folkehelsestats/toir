
calc_simple_pros <- function(DT, var, col, cat = NULL) {
  # Ensure DT is a data.table
  data.table::setDT(DT)

  # Count rows by 'var', excluding NA and value 9
  restot <- DT[!is.na(get(var)) & !(get(var) %in% 8:9),
            .N,
            keyby = var]

  # Add percentage column
  restot[, pros := round(N / sum(N) * 100, 1)]
  restot[, col := col]

  if (!is.null(cat)) {
    # Count rows by 'var' and 'cat', excluding NA and value 9
    res <- DT[!is.na(get(var)) & !(get(var) %in% 8:9),
              .N,
              keyby = c(var, cat)]

    # Add percentage column within each cat
    res[, pros := round(N / sum(N) * 100, 1), by = c(cat)]
    res[, col := col]

    restot[, (cat) := "Alle"]
    out <- data.table::rbindlist(list(res, restot), use.names = TRUE, fill = TRUE)
  }

  data.table::setnames(out, names(out), c("count", "cat", "n", "pros", "type"))

  return(out[])
}

## Use population for 2025 only including ages 16-64
dt25 <- DD[year == 2025]

## Hvis Can1=ja
## Can2
## Hvor mange ganger har du prøvd eller brukt cannabis?
## En gang betyr for eksempel én enkelt joint/pipe, to joints/piper samme dag regnes som to ganger.
## 1. En gang
## 2. 2-5 ganger
## 3. 6-10 ganger
## 4. 11 – 50 ganger
## 5. Mer enn 50 ganger

## Merge 1 & 2 to match similar categories as can9
dt25[, can2ny := fcase(can2 %in% 1:2, 1,
                       can2 == 3, 2,
                       can2 == 4, 3,
                       can2 == 5, 4)]

canBruk1 <- calc_simple_pros(dt25, "can2", "Noen ganger", cat = "kjonnSTR")
canBruk12 <- calc_simple_pros(dt25, "can2ny", "Noen ganger", cat = "kjonnSTR")


## Hvis Can6=ja (Brukt cannabis de siste 12 månedene)
## Can9
## Hvor mange ganger har du brukt cannabis de siste 12 månedene?
## En gang tilsvarer for eksempel én joint/pipe, to ganger tilsvarer to joints/piper og så videre.
## 1. 1-5 ganger
## 2. 6-10 ganger
## 3. 11-50 ganger
## 4. Mer enn 50 ganger

canBruk2 <- calc_simple_pros(dt25, "can9", "Siste 12 måneder", cat = "kjonnSTR")


## Hvis Can10=ja (Bruk cannabis i løpet av de siste 4 uker)
## Can11
## Hvor mange dager i de siste fire ukene har du brukt cannabis?
## 1. 20 dager eller mer
## 2. 10-19 dager
## 3. 4-9 dager
## 4. 1-3 dager

canBruk3 <- calc_simple_pros(dt25, "can11", "Siste 4 uker", cat = "kjonnSTR")

## ---------
## Dataset for figure
## ---------

## canNo <- data.table::rbindlist(list(canBruk12, canBruk2))
canNo <- canBruk2

vals <- data.table::data.table(from = 1:4,
                               to = c("1-5 ganger",
                                      "6-10 ganger",
                                      "11-50 ganger",
                                      "Mer enn 50 ganger"))

canNo[vals, on = c(count = "from"), vals := i.to]

## canNo[, vals := factor(count,
##                        levels = 1:4,
##                        labels = c("1-5 ganger",
##                                       "6-10 ganger",
##                                       "11-50 ganger",
##                                       "Mer enn 50 ganger"))]
