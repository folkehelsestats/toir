## Gender and total prevalence
## ---------------------------
## no - nomerator
## de - denominator
general_form <- function(dt, no, de, weight = "vekt2", ...){
  
  #d <- get_prev_ci(dt = dt, no = no, de = de, ...)
  d <- torr::calc_prevalence(data = dt,
                             denominator = de,
                             year_var = "year",
                             outcome_var = no,
                             weight_var = weight)

  
  dk <- torr::calc_prevalence(data = dt,
                             denominator = de,
                             year_var = "year",
                             outcome_var = no,
                             weight_var = weight,
                             by = "kjonn")

  d[, kjonn := 3] #total
  dd <- data.table::rbindlist(list(d, dk), use.names = TRUE, fill = TRUE)
  dd[.(kjonn = 1:3, to = c("Male", "Female", "Total")), on = "kjonn", gender := i.to]
  data.table::setorder(dd, kjonn)
  data.table::setcolorder(dd, "gender", after = "rolling_period")
  return(dd[])
}

## Form style - 2.3 Broad age group
## --------------------------------
# make life easier to match the form style

broad_form <- function(dt, no, de, weight = "vekt2", ...){

  #d <- get_prev_ci(dt = dt, no = no, de = de, ...)

  dk <- torr::calc_prevalence(data = dt,
                             denominator = de,
                             year_var = "year",
                             outcome_var = no,
                             weight_var = weight,
                             by = c("kjonn", "agecat"))

  da <- torr::calc_prevalence(data = dt,
                             denominator = de,
                             year_var = "year",
                             outcome_var = no,
                             weight_var = weight,
                             by = "agecat")

  
  da[, kjonn := 3] #total
  dd <- data.table::rbindlist(list(da, dk), use.names = TRUE, fill = TRUE)
  dd[.(kjonn = 1:3, to = c("Male", "Female", "Total")), on = "kjonn", gender := i.to]
  data.table::setorder(dd, agecat, kjonn)
  data.table::setcolorder(dd, "gender", after = "rolling_period")

  for (x in unique(dd$agecat)) {
    cat("\nAge:", as.character(x), "\n")
    print(dd[agecat == x])
  }
  
  invisible(dd)
}

# tot - Dataset for total
# cat - dataset for gender categories
broad_form_cat <- function(tot, cat){

  dx <- rbindlist(list(tot, cat), use.names = TRUE, fill = TRUE)
  dx <- dx[order(agecat, Kjonn, outcome_level), .(agecat, gender, percentage, canfreq)]

  can <- unique(dx$canfreq)

  dd <- vector(mode = "list", length = length(can))

  for (i in seq_len(length(can))){
    x <- can[i]
    dd[[i]] <- dx[canfreq == x]
  }

  names(dd) <- paste0("Cannabis freq: ", can)
  return(dd)
}
