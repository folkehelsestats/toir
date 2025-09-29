## Form style - 2.3 Broad age group
## --------------------------------
# make life easier to match the form style

show_form <- function(dt, no, de){

  d <- get_prev_ci(dt = dt, no = no, de = de)

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
