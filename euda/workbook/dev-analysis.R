## Analysis of Moving Average from 2022 to 2024
## --------------------------------------
source(here::here("setup.R"))
source(here::here("data-source.R"))

# clean labelled attributes coz it makes troubles
DT[, (names(DT)) := lapply(.SD, \(x) { attributes(x) <- NULL; x })]

DT[year %in% c(2022,2024), kjonn := fcase(kjonn == 1, 2, #Kvinne
                                          kjonn == 0, 1)] #Mann
dt <- DT[year %in% 2022:2024]
nrow(dt)


## for (j in names(dt)) {
##   # Remove attributes from the column
##   col <- dt[[j]]
##   attributes(col) <- NULL

##   # Set the cleaned column back into dt
##   data.table::set(dt, j = j, value = col)
## }

## Relevin exclusion
## ------------------
## OBS! Exclude those that used Relevin Ans2_d == 1
## dt <- dt[ans2_d != 1]

dt[ans2_d == 1, .N]
ans2 <- grep("ans2_", names(dx), ignore.case = TRUE, value = TRUE)
ansTmp <- paste0(ans2, "_tmp")

dt[, (ansTmp) := lapply(.SD, function(x) {
  fcase(
    x == 1, 1,
    x %in% c(2, 8, 9), NA,
    default = NA
  )
}), .SDcols = ans2]

dt[, ansSum := rowSums(.SD, na.rm = TRUE), .SDcols = ansTmp]
dt <- dt[ansSum != 8] # Remove those who answered yes to all drugs including Relevin
dt[, (ansTmp) := NULL] # Remove temporary columns

## Age groups
## -------------
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-age.R")

dt <- group_age_standard(dt, var = "alder", type = "rusund",
                         new_var = "agecat")

## Exclude all missing and not answered Can1 or Ans1
## Denominator for Illigal rusmidler
## ------------
dt[, canpop := ifelse(can1 %in% 1:2, 1, 0)]
dt[, .N, keyby = canpop]
dt[, .N, keyby = can1]

dt[, narkpop := ifelse(ans1 %in% 1:2, 1, 0)]
dt[, .N, keyby = narkpop]
dt[, .N, keyby = ans1]

dt[canpop == 1 | narkpop == 1, anypop := 1][
  is.na(anypop), anypop := 0]

## Figure 6: Cannabis use all adults (16-64) and young adults (16-34), 2022-2024
## --------------------------------------------------
## Free text - Other types
## Bør sjekke tekst fra ans2sps
dt[, .N, keyby = ans2sps][!grep("9999", ans2sps)]

dt[grep("lsd", ans2sps, ignore.case = TRUE), "AndreLSD" := 1]
dt[grep("hasj", ans2sps, ignore.case = TRUE), "AndreCannabis" := 1]

## includes free text answers to the new variables
dt[, Can1_ny := can1][AndreCannabis == 1, Can1_ny := 1]

## ---------------------
## Lifetime  prevalence
## ---------------------

dt[Can1_ny == 1, ltp_cannabis := 1] #Cannabis-type drugs


canltpKjonn <- dt[, .(
  cases = sum(ltp_cannabis, na.rm = TRUE),
  denom = sum(canpop, na.rm = TRUE)
), by = .(year, kjonn)][order(year)]

canltpMenn <- canltpKjonn[kjonn == 1]
canltpMenn[, `:=`(
  cases_3yr = data.table::frollsum(cases, n = 3, align = "right"),
  denom_3yr = data.table::frollsum(denom, n = 3, align = "right")
)]

canltpMenn[, pct_3yr := fifelse(denom_3yr > 0, 100 * cases_3yr / denom_3yr, NA_real_)]

setkey(canltpMenn, year)

canltpKvinner <- canltpKjonn[kjonn == 2]
canltpKvinner[, `:=`(
  cases_3yr = data.table::frollsum(cases, n = 3, align = "right"),
  denom_3yr = data.table::frollsum(denom, n = 3, align = "right")
)]

canltpKvinner[, pct_3yr := fifelse(denom_3yr > 0, 100 * cases_3yr / denom_3yr, NA_real_)]

setkey(canltpKvinner, year)

canltpAll <- dt[, .(
  cases = sum(ltp_cannabis, na.rm = TRUE),
  denom = sum(canpop, na.rm = TRUE)
), by = .(year)][order(year)]

canltpAll[, `:=`(
  cases_3yr = data.table::frollsum(cases, n = 3, align = "right"),
  denom_3yr = data.table::frollsum(denom, n = 3, align = "right")
)]

canltpAll[, pct_3yr := fifelse(denom_3yr > 0, 100 * cases_3yr / denom_3yr, NA_real_)]
canltpAll[, kjonn := 0] #All

setkey(canltpAll, year)

source(here::here("euda/workbook/fun-moving-percentage.R"))
dtt <- calc_moving_prevalence(dt, "ltp_cannabis", "canpop", "kjonn")
dtt[, type := "LTP"]
dtt[, grp := "All Adults (16-64)"]
dtt <- dtt[year == 2024]

dty <- dt[agecat %chin% c("16-24", "25-34")] #16-34 yrs
dtyt <- calc_moving_prevalence(dty, "ltp_cannabis", "canpop", "kjonn")
dtyt[, type := "LTP"]
dtyt[, grp := "Young Adults (16-34)"]
dtyt <- dtyt[year == 2024]

tbLTP <- rbindlist(list(dtt, dtyt))


create_tbl_grp <- function(dt, type, grp, filter = 2024, value = "pct_3yr") {
  dt <- dt[year == filter]
  dt[, type := type]
  dt[, grp := grp]
  dt[, value := round(get(value), 1)]
  dt[, year := NULL]
}


tbLTP <- rbindlist(list(
  create_tbl_grp(dtt, "LTP", "All Adults (16-64)"),
  create_tbl_grp(dtyt, "LTP", "Young Adults (16-34)")
))

## ---------------------
## Last year prevalence
## ---------------------

dt[can6 == 1, lyp_cannabis := 1] #Cannabis-type drugs
dty[can6 == 1, lyp_cannabis := 1] #Cannabis-type drugs

dtlyp <- calc_moving_prevalence(dt, "lyp_cannabis", "canpop", "kjonn")
dtlypYng <- calc_moving_prevalence(dty, "lyp_cannabis", "canpop", "kjonn")

tbLYP <- rbindlist(list(
  create_tbl_grp(dtlyp, "LYP", "All Adults (16-64)"),
  create_tbl_grp(dtlypYng, "LYP", "Young Adults (16-34)")
))

## -------------------
## Last month prevalence
## ---------------------

dt[can10 == 1, lmp_cannabis := 1] #Cannabis-type drugs
dty[can10 == 1, lmp_cannabis := 1] #Cannabis-type drugs

dtlmp <- calc_moving_prevalence(dt, "lmp_cannabis", "canpop", "kjonn")
dtlmpYng <- calc_moving_prevalence(dty, "lmp_cannabis", "canpop", "kjonn")

tbLMP <- rbindlist(list(
  create_tbl_grp(dtlmp, "LMP", "All Adults (16-64)"),
  create_tbl_grp(dtlmpYng, "LMP", "Young Adults (16-34)")
))

## Plotting
tblCannabis <- rbindlist(list(tbLTP, tbLYP, tbLMP))

# Set factor order for age_group
tblCannabis[, gender := factor(kjonn, levels = c(2,1,0),
                              labels = c("Women", "Men", "Total"))]
tblCannabis[, type := factor(type, levels = c("LTP", "LYP", "LMP"))]
## data[, gender := factor(gender, levels = c("Men", "Women", "Total"))]

hdir_color <- c("#025169", "#0069E8",
                "#7C145C", "#047FA4",
                "#C68803", "#38A389",
                "#6996CE", "#366558",
                "#BF78DE", "#767676")

# Create the plot
ggplot(tblCannabis, aes(x = type, y = value, fill = gender)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = value),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.5) +
  facet_wrap(~grp) +
  ## scale_fill_manual(values = c("Men" = "#4472C4", "Women" = "#C44444", "Total" = "#70AD47")) +
  scale_fill_manual(values = hdir_color) +
  labs(x = "Prevalence Type",
       y = "Percentage (%)",
       fill = "",
       title = "Cannabis use with cumulative percentage (2022-2024)") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = c("#E8F4F8", "#FFF4E6"),
                                     color = "grey80", linewidth = 0.5),
    panel.background = element_rect(fill = c("#E8F4F8", "#FFF4E6"),
                                     color = NA),
    legend.position = "bottom"
  ) +
  ylim(0, 42)


## ---------------------
## Figure 8: Freq of cannabis use all adults (16-64), 2022-2024
## ---------------------
dt[, .N, keyby = can2]
dt[can2 %in% 1:5, freq_cannabis := can2]

dtCanUse <- dt[!is.na(freq_cannabis), .N, keyby = freq_cannabis]
canTot <- dtCanUse[, sum(N, na.rm = TRUE) ]
dtCanUse[, pct := round(100 * N / canTot, 1), by = freq_cannabis]

## Set factor order
dtCanUse[, can_lab := factor(freq_cannabis, levels = c(1,2,3,4,5),
                             labels = c("Once",
                                        "2-5 times",
                                        "6-10 times",
                                        "11-50 times",
                                        "More than 50 times"))]

source(here::here("euda/workbook/fun-plot-facet.R"))
create_plot(dtCanUse, x = "can_lab", y = "pct", fill = "can_lab",
            hdir_color = hdir_color,
            title = "Frequency of cannabis use among users (2022-2024)",
            xlab = "",
            ylab = "Cumulative Percentage (%)", lglab = "", show_legend = FALSE)

## ---------------------
## Other narcotics prevalence
## ---------------------

dt[grep("cb", ans2sps, ignore.case = TRUE), "AndreNPS" := 1]
dt[grep("psilocybin", ans2sps, ignore.case = TRUE), "AndreSOPP" := 1]
dt[str_detect(ans2sps, regex("methamfethamin|metamfethamin", ignore_case = TRUE)), "AndreAmfetamin" := 1]
dt[grep("ketamin", ans2sps, ignore.case = TRUE), "AndreKetamin" := 1]
dt[grep("psilocybin", ans2sps, ignore.case = TRUE), "AndreSopp" := 1]

## includes free text answers to the new variables
dt[, Ans2_c_ny := ans2_c][AndreAmfetamin == 1, Ans2_c_ny := 1]
dt[, Ans2_g_ny := ans2_g][AndreLSD == 1, Ans2_g_ny := 1]
## dt[, Ans2_y_ny := ans2_y][AndreSopp == 1, Ans2_y_ny := 1]

dt[ans1 == 1, ltp_drugs := 1] #Other drugs than cannabis

## Any drug
dt[, ltp_any := fcase(ltp_drugs == 1, 1,
                      ltp_cannabis == 1, 1,
                      default = 0)]

dt[ans2_a == 1, ltp_cocaine := 1] #Cocaine-type drugs
dt[ans2_b == 1, ltp_mdma := 1] #"Ecstasy" type substances
dt[Ans2_c_ny == 1, ltp_amphetamines := 1] #Amphetamine-type stimulants
dt[ans2_d == 1, ltp_relevin := 1]
dt[ans2_e == 1, ltp_heroin := 1] #Heroin
dt[ans2_f == 1, ltp_ghb := 1] #Other sedatives and tranquillizers
dt[Ans2_g_ny == 1, ltp_lsd := 1] #LSD
## dt[ans2_x == 1, ltp_nps := 1] #Any new psychoactive substances (NPS)
## dt[Ans2_y_ny == 1, ltp_sopp := 1] #Sopp
dt[ans2_h == 1, ltp_other := 1]

dt[Dop1 == 1, ltp_steroid := 1] #prestasjonsfremmende midler
dt[Drukket1 == 1 | Drukk1b == 1, ltp_alcohol := 1]

dt[, ltp_tobacco := fcase(Tob1 == 1, 1,
                          Tob14 %in% 1:2, 1,
                          default = 0)]
