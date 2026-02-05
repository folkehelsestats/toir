
gender_prop <- function(DTT, nominator, denominator,
                        yr = 2025,
                        agemax = NULL,
                        type = c("ltp", "lyp")){

  nom <- paste0(type, "_", nominator)
  dom <- paste0(type, "Pop_", denominator)

  if (is.null(agemax)){
    DTT <- DTT[year == yr]
  } else {
    DTT <- DTT[year == yr & alder <= agemax]
  }

ltp <- calc_percentage_total_ci(
  DTT[year == yr],
  outcome_var = nom[1],
  group_vars = "kjonnSTR",
  weight_var = "vekt",
  denominator_var = dom[1],
  na_treatment = "as_zero",
  round_digits = 1,
  include_diagnostics = TRUE,
  include_total = TRUE,
  total_label = "Alle"
)

## outLtp <- append_total_row(ltp)
outLtp <- ltp

lyp <- calc_percentage_total_ci(
  DTT[year == yr],
  outcome_var = nom[2],
  group_vars = "kjonnSTR",
  weight_var = "vekt",
  denominator_var = dom[2],
  na_treatment = "as_zero",
  round_digits = 1,
  include_diagnostics = TRUE,
  include_total = TRUE,
  total_label = "Alle"
)

## outLyp <- append_total_row(lyp)
outLyp <- lyp

  out <- rbindlist(list(
  outLtp[, periode := "Noen gang"],
  outLyp[, periode := "Siste 12 måneder"]
), use.names = TRUE, fill = TRUE)

out[, stuff := denominator]

return(out[])
}


kokainDT <- gender_prop(DTT, "cocaine", "kokain")
mdmaDT <- gender_prop(DTT, "mdma", "mdma")
ampheDT <- gender_prop(DTT, "amphetamines", "amfetaminer")

andreDT <- data.table::rbindlist(list(kokainDT, mdmaDT, ampheDT))
andreDT[, percentage := round(percentage, 1)]

### ---- Yngre <=30
### ------------------
kokainDTyng <- gender_prop(DTT, "cocaine", "kokain", agemax = 30)
mdmaDTyng <- gender_prop(DTT, "mdma", "mdma", agemax = 30)
ampheDTyng <- gender_prop(DTT, "amphetamines", "amfetaminer", agemax = 30)

andreDTyng <- data.table::rbindlist(list(kokainDTyng, mdmaDTyng, ampheDTyng))
andreDTyng[, percentage := round(percentage, 1)]
