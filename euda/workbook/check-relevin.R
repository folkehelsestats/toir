## Question on relevin use is a control question to identify possible over-reporting.
## Check Relevin answers and if this should be excluded from the analysis
## --------------------------------------

source(here::here("setup.R"))
source(here::here("data-source.R"))

dx <- copy(DT)

## OBS! Exclude those that used Relevin Ans2_d == 1
## dt <- dt[ans2_d != 1]
ans2 <- grep("ans2_", names(dx), ignore.case = T, value=T)

dx[, (ans2) := lapply(.SD, function(x) {
  fcase(
    x == 1, 1,
    x %in% c(2, 8, 9), NA,
    default = NA
  )
}), .SDcols = ans2]

dx[, ansSum := rowSums(.SD, na.rm = TRUE), .SDcols = ans2]
dx[, max(ansSum)]
ansX <- c(ans2, "ansSum", "year")
dx[ansSum == 8, ..ansX]
dx[ansSum == 7 & ans2_d == 1, ..ansX]

dx[ans2_d == 1 & ansSum < 6 , ..ansX][order(ansSum)]
