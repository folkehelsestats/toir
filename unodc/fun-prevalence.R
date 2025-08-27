
## no - nominator
## de - denominator
cal_prev <- function(dt, no, de){
  tot <- dt[, round(sum(no, na.rm = T)/sum(de, na.rm = T)*100, digits = 1), env = list(no = no, de = de)]
  kjonn <- dt[, round(sum(no, na.rm = TRUE)/sum(de, na.rm = T)*100, digits = 1), keyby = Kjonn, env = list(no = no, de = de)]
  alder <- dt[, round(sum(no, na.rm = T)/sum(de, na.rm = T)*100, digits = 1), keyby = agecat, env = list(no = no, de = de)]
  begge <- dt[, round(sum(no, na.rm = T)/sum(de, na.rm = T)*100, digits = 1), keyby = .(Kjonn, agecat), env = list(no = no, de = de)]

  list(total = tot, kjonn = kjonn, alder = alder, begge = begge)
}
