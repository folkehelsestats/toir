## All type of illegal drugs
## -------------------------

vars01 <- c("cannabis", "cocaine", "mdma", "amphetamines", "heroin", "ghb", "lsd", "other")

vars_no <- c(
  "Cannabis",
  "Kokain",
  "Ecstasy/MDMA",
  "Amfetaminer",
  "Heroin",
  "GHB",
  "LSD",
  "Annet"
)


outcome_var <- paste0("ltp_", vars01)

out <- vector("list", length(outcome_var))

for (i in seq_along(outcome_var)) {
  out[[i]] <- calc_percentage_ci(
    dt = DTT[year == 2025],
    outcome_var = outcome_var[i],
    weight_var = "vekt",
    denominator_var = "anypop",
    group_vars = "year",
    na_treatment = "as_zero",
    round_digits = 1,
    include_diagnostics = TRUE
  )
  out[[i]][, substance := outcome_var[i]]
}

anyDT <- rbindlist(out, use.names = TRUE, fill = TRUE)
anyDT[, substance := gsub("ltp_", "", substance)]
setorder(anyDT, -percentage)

anyDT[.(substance = vars01, norwegian = vars_no), on = .(substance), substance := i.norwegian]
