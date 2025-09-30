## Gender and total prevalence
## ---------------------------

general_form <- function(dt, no, de, ...){

  d <- get_prev_ci(dt = dt, no = no, de = de, ...)

  d$total[, gender := "Total"]
  dx <- rbindlist(list(d$total, d$kjonn), use.names = TRUE, fill = TRUE)
  dx[, Kjonn := NULL]
  dx[, gender := factor(gender, levels = c("Male", "Female", "Total"))]
  data.table::setcolorder(dx, "gender", before = "percentage")
  dx <- dx[order(gender)]
  return(dx)
}

## Form style - 2.3 Broad age group
## --------------------------------
# make life easier to match the form style

broad_form <- function(dt, no, de, ...){

  d <- get_prev_ci(dt = dt, no = no, de = de, ...)

  d$alder[, gender := "Total"]
  dx <- rbindlist(list(d$begge, d$alder), use.names = TRUE, fill = TRUE)
  dx[, Kjonn := NULL]
  dx <- dx[order(agecat)]

  age <- unique(dx$agecat)

  dd <- vector(mode = "list", length = length(age))

  for (i in seq_len(length(age))){
    x <- age[i]
    dd[[i]] <- dx[agecat == x]
  }

  names(dd) <- paste0("Agegp: ", age)
  return(dd)
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
