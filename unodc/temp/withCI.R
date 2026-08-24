
enhetTbl <- ddt[, {
  model <- lm(totalcl ~ alder + kjonn, weights = nyvekt2, data = .SD)
  em <- emmeans(model, ~ 1)
  em_smry <- summary(em)
  data.table(
    adj_mean = em_smry$emmean,
    SE = em_smry$SE,
    lower_95CI = em_smry$lower.CL,
    upper_95CI = em_smry$upper.CL,
    adj_enhet = em_smry$emmean/1.5,
    SE_enhet = em_smry$SE/1.5,
    lower_enhet = em_smry$lower.CL/1.5,
    upper_enhet = em_smry$upper.CL/1.5
  )
}, by = year]


# Round enhet columns to 0 decimal place
colnr <- ncol(enhetTbl)
enhetTbl[, (6:colnr) := lapply(.SD, round, digits = 1), .SDcols = 6:colnr]
enhetTbl[, .(year, adj_enhet, lower_enhet, upper_enhet)]
