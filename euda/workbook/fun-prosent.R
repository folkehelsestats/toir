
library(data.table)

calc_pro <- function(dt, tot, var, col){
  out <- dt[tot == 1, .(pro = round(mean(var == 1, na.rm = TRUE)*100, digits = 1),
                        total = .N,
                        n = sum(var == 1, na.rm = TRUE)), keyby = year,
            env = list(
              tot = tot,
              var = var)]

  out[, can := col][]
}
