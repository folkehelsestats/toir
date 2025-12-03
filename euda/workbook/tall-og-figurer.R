## Data source and setup
## --------------------------------------
options(scipen = 999) # disable scientific notation
source(here::here("setup.R"))
source(here::here("data-source.R"))

## Helper function to calculate prevalence with total
## ---------------------

source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-age.R")
source(here::here("euda/workbook/fun-plot-facet.R"))
source(here::here("euda/workbook/fun-moving-percentage.R"))
source(here::here("euda/workbook/fun-plot-facet.R"))
source(file.path(here::here(), "euda/workbook/fun-line-plot.R"))
source(file.path(here::here(), "euda/workbook/fun-prosent.R"))

hdir_color <- c("#025169", "#0069E8",
                "#7C145C", "#047FA4",
                "#C68803", "#38A389",
                "#6996CE", "#366558",
                "#BF78DE", "#767676")

chc3 <- c("#5F9EA0", "#E1B378", "#7C145C")

## popvar: population variable
## by: grouping variable
calc_prev_all <- function(dt, var, popvar, by = NULL) {

  dtTot <- calc_moving_prevalence(dt, var, popvar)

  if (!is.null(by)){
    dtBy <- calc_moving_prevalence(dt, var, popvar, by)
    dtTot[, (by) := "Total"]
    dtTot <- rbindlist(list(dtBy, dtTot), use.names = TRUE)
  }
  return(dtTot[])
}

## Make summary table function for plotting
## -----------------------------
## type: LTP, LYP, LMP
## grp: Group labels for legend or facets
## filter: Year filter
## value: Value column to use
create_tbl_grp <- function(dt, type, grp, filter = 2024, value = "pct_roll") {
  dt <- dt[year == filter]
  dt[, type := type]

  if (!missing(grp))
    dt[, grp := grp]

  dt[, value := round(get(value), 1)]
  dt[, year := NULL]
}

## ==================================
## Data preparation
## ----------------------------------

# clean labelled attributes coz it makes troubles
DT[, (names(DT)) := lapply(.SD, \(x) { attributes(x) <- NULL; x })]

DT[year %in% c(2022,2024), kjonn := fcase(kjonn == 1, 2, #Kvinne
                                          kjonn == 0, 1)] #Mann

## Age groups
## -------------
DT <- group_age_standard(DT, var = "alder", type = "rusund",
                         new_var = "agecat")

## Relevin exclusion
## ------------------
## OBS! Exclude those that used Relevin Ans2_d == 1

ans2 <- grep("ans2_", names(DT), ignore.case = TRUE, value = TRUE)
ansTmp <- paste0(ans2, "_tmp")

DT[, (ansTmp) := lapply(.SD, function(x) {
  fcase(
    x == 1, 1,
    x %in% c(2, 8, 9), NA,
    default = NA
  )
}), .SDcols = ans2]

DT[, ansSum := rowSums(.SD, na.rm = TRUE), .SDcols = ansTmp]
# Remove ONLY those who answered yes to all drugs including Relevin
DT <- DT[ansSum != 8]
DT[, (ansTmp) := NULL]


## Exclude all missing and not answered Can1 or Ans1
## Denominator for Illigal rusmidler
## ------------
DT[, canpop := ifelse(can1 %in% 1:2, 1, 0)]
DT[, narkpop := ifelse(ans1 %in% 1:2, 1, 0)]
DT[canpop == 1 | narkpop == 1, anypop := 1][
  is.na(anypop), anypop := 0]


## Dataset for 2022-2024 only
## -------------------------
dt <- DT[year %in% 2022:2024]

## --------------------------------------------------
## Figure 6: Cannabis use all adults (16-64) and young adults (16-34), 2022-2024
## --------------------------------------------------

## includes free text answers to the new variables
dt[grep("lsd", ans2sps, ignore.case = TRUE), "AndreLSD" := 1]
dt[grep("hasj", ans2sps, ignore.case = TRUE), "AndreCannabis" := 1]

dt[, Can1_ny := can1][AndreCannabis == 1, Can1_ny := 1]


## Age group 16-34 and functions
## ----------------------------
## source(here::here("euda/workbook/fun-moving-percentage.R"))
dty <- dt[agecat %chin% c("16-24", "25-34")] #16-34 yrs

## Lifetime  prevalence
## ---------------------

dt[Can1_ny == 1, ltp_cannabis := 1] #Cannabis-type drugs
dty[Can1_ny == 1, ltp_cannabis := 1] #Cannabis-type drugs

dttKj <- calc_moving_prevalence(dt, "ltp_cannabis", "canpop", "kjonn")
dttTot <- calc_moving_prevalence(dt, "ltp_cannabis", "canpop")
dttTot[, kjonn := 0] #Total
dtt <- rbindlist(list(dttKj, dttTot), use.names = TRUE)

dtytKj <- calc_moving_prevalence(dty, "ltp_cannabis", "canpop", "kjonn")
dtytTot <- calc_moving_prevalence(dty, "ltp_cannabis", "canpop")
dtytTot[, kjonn := 0] #Total
dtyt <- rbindlist(list(dtytKj, dtytTot), use.names = TRUE)

tbLTP <- rbindlist(list(
  create_tbl_grp(dtt, "LTP", "All Adults (16-64)"),
  create_tbl_grp(dtyt, "LTP", "Young Adults (16-34)")
))

## Last year prevalence
## ---------------------

dt[can6 == 1, lyp_cannabis := 1] #Cannabis-type drugs
dty[can6 == 1, lyp_cannabis := 1] #Cannabis-type drugs

dtlypKjonn <- calc_moving_prevalence(dt, "lyp_cannabis", "canpop", "kjonn")
dtlypTot <- calc_moving_prevalence(dt, "lyp_cannabis", "canpop")
dtlypTot[, kjonn := 0] #Total
dtlyp <- rbindlist(list(dtlypKjonn, dtlypTot), use.names = TRUE)

dtlypYngKjonn <- calc_moving_prevalence(dty, "lyp_cannabis", "canpop", "kjonn")
dtlypYngTot <- calc_moving_prevalence(dty, "lyp_cannabis", "canpop")
dtlypYngTot[, kjonn := 0] #Total
dtlypYng <- rbindlist(list(dtlypYngKjonn, dtlypYngTot), use.names = TRUE)

tbLYP <- rbindlist(list(
  create_tbl_grp(dtlyp, "LYP", "All Adults (16-64)"),
  create_tbl_grp(dtlypYng, "LYP", "Young Adults (16-34)")
))

## Last month prevalence
## ---------------------

dt[can10 == 1, lmp_cannabis := 1] #Cannabis-type drugs
dty[can10 == 1, lmp_cannabis := 1] #Cannabis-type drugs

dtlmpKj <- calc_moving_prevalence(dt, "lmp_cannabis", "canpop", "kjonn")
dtlmpTot <- calc_moving_prevalence(dt, "lmp_cannabis", "canpop")
dtlmpTot[, kjonn := 0] #Total
dtlmp <- rbindlist(list(dtlmpKj, dtlmpTot), use.names = TRUE)

dtlmpYngKjonn <- calc_moving_prevalence(dty, "lmp_cannabis", "canpop", "kjonn")
dtlmpYngTot <- calc_moving_prevalence(dty, "lmp_cannabis", "canpop")
dtlmpYngTot[, kjonn := 0] #Total
dtlmpYng <- rbindlist(list(dtlmpYngKjonn, dtlmpYngTot), use.names = TRUE)


tbLMP <- rbindlist(list(
  create_tbl_grp(dtlmp, "LMP", "All Adults (16-64)"),
  create_tbl_grp(dtlmpYng, "LMP", "Young Adults (16-34)")
))

## Plotting
## ---------------------
tblCannabis <- rbindlist(list(tbLTP, tbLYP, tbLMP))

# Set factor order for age_group
tblCannabis[, gender := factor(kjonn, levels = c(2,1,0),
                              labels = c("Women", "Men", "Total"))]
tblCannabis[, type := factor(type, levels = c("LTP", "LYP", "LMP"))]

## source(here::here("euda/workbook/fun-plot-facet.R"))
fig6 <- create_plot(tblCannabis, x = "type", y = "value", fill = "gender",
                    hdir_color = hdir_color, wrap = "grp",
                    title = "Cumulative percentage of cannabis use (2022-2024)",
                    xlab = "Prevalence Type", ylab = "Percentage (%)", lglab = "")

ggplot2::ggsave(
  filename = here::here("euda/workbook/figures/figure6_cannabis_use_2022-2024.png"),
  plot = fig6,
  width = 10,
  height = 6,
  dpi = 300
)

## ----------------------------------
## Figure 7 - Cannabis use across age groups, 2022-2024
## ----------------------------------

ltCanAge <- calc_prev_all(dt, "ltp_cannabis", "canpop", "agecat")
lyCanAge <- calc_prev_all(dt, "lyp_cannabis", "canpop", "agecat")
lmCanAge <- calc_prev_all(dt, "lmp_cannabis", "canpop", "agecat")

tblCanAge <- rbindlist(list(
  create_tbl_grp(ltCanAge, "LTP"),
  create_tbl_grp(lyCanAge, "LYP"),
  create_tbl_grp(lmCanAge, "LMP")
))

tblCanAge <- tblCanAge[!(agecat %chin% c("65-79", "Total"))]
tblCanAge[, type := factor(type, levels = c("LTP", "LYP", "LMP"))]

# Create the plot
fig7 <- create_plot(tblCanAge,
                    x = "type", y = "value", fill = "agecat",
                    hdir_color = hdir_color, wrap = NULL,
                    title = "Cumulative percentage of cannabis use across age groups (2022-2024)",
                    xlab = "Prevalence Type", ylab = "Percentage (%)", lglab = "Age"
                    )

ggplot2::ggsave(
           filename = here::here("euda/workbook/figures/figure7_cannabis_use_agegroups_2022-2024.png"),
           plot = fig7,
           width = 10,
           height = 6,
           dpi = 300)

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

fig8 <- create_plot(dtCanUse, x = "can_lab", y = "pct", fill = "can_lab",
            hdir_color = hdir_color,
            title = "Frequency of cannabis use among users (2022-2024)",
            xlab = "",
            ylab = "Cumulative Percentage (%)", lglab = "", show_legend = FALSE)

ggplot2::ggsave(filename = here::here("euda/workbook/figures/figure8_frequency_cannabis_use_2022-2024.png"),
                plot = fig8,
                width = 8,
                height = 6,
                dpi = 300)

## ----------------------------------
## Figure 11 - Number of amphetamines seizures (methamphetamine and amphetamine), 2008-2024
## ----------------------------------

## source(file.path(here::here(), "euda/workbook/fun-line-plot.R"))
## source(file.path(here::here(), "euda/workbook/fun-prosent.R"))

amfi <- readxl::read_xlsx(here::here("euda/workbook/data/amphetamines-seizures.xlsx"),
                          sheet = "Ark1")

setnames(amfi, names(amfi), tolower(names(amfi)))
amfiDT <- data.table::as.data.table(amfi)

amfiDT[, total := rowSums(.SD, na.rm = TRUE), keyby = year]
amfiDT <- amfiDT[year >= 2010]
amfiW <- melt(amfiDT, id.vars = "year",
                measure.vars = c("methamphetamine", "amphetamine", "total"),
                value.name = "antall",
                variable.name = "amfi")

amfiBesLine <- make_line_plot(data = amfiW,
                             x = "year",
                             y = "antall",
                             color = "amfi",
                             title = "Number of amphetamines seizures (methamphetamine and amphetamine), 2010-2024",
                             caption = "Source: National Crime Investigation Service (Kripos/NCIS)",
                             color_values = chc3,
                             label_col = "amfi",
                             y_lab = NULL)

ggplot2::ggsave(filename = here::here("euda/workbook/figures/figure11_amphetamines_seizures_2010-2024.png"),
                plot = amfiBesLine,
                width = 10,
                height = 6,
                dpi = 300)



## ===========================
## Figure 12 - Cocaine, Amphetamines and MDMA/Ecstasy
## ===========================


dt[ans2_a == 1, ltp_cocaine := 1] #Cocaine-type drugs
dt[ans2_b == 1, ltp_mdma := 1] #"Ecstasy" type substances
dt[Ans2_c_ny == 1, ltp_amphetamines := 1] #Amphetamine-type stimulants

## Lifetime prevalence - all adults
ltCocaine <- calc_prev_all(dt, "ltp_cocaine", "narkpop", "kjonn")
ltAmphetamines <- calc_prev_all(dt, "ltp_amphetamines", "narkpop", "kjonn")
ltMdma <- calc_prev_all(dt, "ltp_mdma", "narkpop", "kjonn")

tbLTPDrugs <- rbindlist(list(
  create_tbl_grp(ltCocaine, "LTP", "Cocaine"),
  create_tbl_grp(ltAmphetamines, "LTP", "Amphetamines"),
  create_tbl_grp(ltMdma, "LTP", "MDMA/Ecstasy")
))

## Last year prevalence - all adults
dt[ans3_1 == 1, lyp_cocaine := 1]
dt[ans3_2 == 1, lyp_mdma := 1]
dt[ans3_3 == 1, lyp_amphetamines := 1]

lyCocaine <- calc_prev_all(dt, "lyp_cocaine", "narkpop", "kjonn")
lyAmphetamines <- calc_prev_all(dt, "lyp_amphetamines", "narkpop", "kjonn")
lyMdma <- calc_prev_all(dt, "lyp_mdma", "narkpop", "kjonn")

tblLYPDrugs <- rbindlist(list(
  create_tbl_grp(lyCocaine, "LYP", "Cocaine"),
  create_tbl_grp(lyAmphetamines, "LYP", "Amphetamines"),
  create_tbl_grp(lyMdma, "LYP", "MDMA/Ecstasy")
))

dtDrugs <- rbindlist(list(tbLTPDrugs, tblLYPDrugs))

dtDrugs[, gender := factor(kjonn, levels = c("1", "2", "Total"), labels = c("Men", "Women", "Total"))]
dtDrugs[, grp := factor(grp, levels = c("Cocaine", "Amphetamines", "MDMA/Ecstasy"))]

fig12 <- create_plot(dtDrugs,
                     x = "gender", y = "value", fill = "grp", wrap = "type",
                     hdir_color = hdir_color, ylim_max = 15,
                     title = "Use of stimulants (LTP and LYP) all adults (16–64), 2022-2024",
                     lglab = "", xlab = "", ylab = "Cumulative Percentage (%)"
                     )

ggplot2::ggsave(filename = here::here("euda/workbook/figures/figure12_stimulant_use_2022-2024.png"),
                plot = fig12,
                width = 10,
                height = 6,
                dpi = 300)

## ===========================
## Figure 13 - Cocaine, Amphetamines and MDMA/Ecstasy across younger adults (16-34)
## ===========================

dty[ans2_a == 1, ltp_cocaine := 1] #Cocaine-type drugs
dty[ans2_b == 1, ltp_mdma := 1] #"Ecstasy" type substances
dty[Ans2_c_ny == 1, ltp_amphetamines := 1] #Amphetamine-type stimulants

## Lifetime prevalence
ltCocaine <- calc_prev_all(dty, "ltp_cocaine", "narkpop", "kjonn")
ltAmphetamines <- calc_prev_all(dty, "ltp_amphetamines", "narkpop", "kjonn")
ltMdma <- calc_prev_all(dty, "ltp_mdma", "narkpop", "kjonn")

tbLTPDrugs <- rbindlist(list(
  create_tbl_grp(ltCocaine, "LTP", "Cocaine"),
  create_tbl_grp(ltAmphetamines, "LTP", "Amphetamines"),
  create_tbl_grp(ltMdma, "LTP", "MDMA/Ecstasy")
))

## Last year prevalence
dty[ans3_1 == 1, lyp_cocaine := 1]
dty[ans3_2 == 1, lyp_mdma := 1]
dty[ans3_3 == 1, lyp_amphetamines := 1]

lyCocaine <- calc_prev_all(dty, "lyp_cocaine", "narkpop", "kjonn")
lyAmphetamines <- calc_prev_all(dty, "lyp_amphetamines", "narkpop", "kjonn")
lyMdma <- calc_prev_all(dty, "lyp_mdma", "narkpop", "kjonn")

tblLYPDrugs <- rbindlist(list(
  create_tbl_grp(lyCocaine, "LYP", "Cocaine"),
  create_tbl_grp(lyAmphetamines, "LYP", "Amphetamines"),
  create_tbl_grp(lyMdma, "LYP", "MDMA/Ecstasy")
))

dtyDrugs <- rbindlist(list(tbLTPDrugs, tblLYPDrugs))

dtyDrugs[, gender := factor(kjonn, levels = c("1", "2", "Total"), labels = c("Men", "Women", "Total"))]
dtyDrugs[, grp := factor(grp, levels = c("Cocaine", "Amphetamines", "MDMA/Ecstasy"))]

fig13 <- create_plot(dtyDrugs,
                     x = "gender", y = "value", fill = "grp", wrap = "type",
                     hdir_color = hdir_color, ylim_max = 15,
                     title = "Use of stimulants (LTP and LYP) young adults (16–34), 2022-2024",
                     lglab = "", xlab = "", ylab = "Cumulative Percentage (%)"
                     )

ggplot2::ggsave(filename = here::here("euda/workbook/figures/figure13_stimulant_use_young_2022-2024.png"),
                plot = fig13,
                width = 10,
                height = 6,
                dpi = 300)

## ===========================
## Figure 14 - Use of stimulants last 12 months in young adults (16-34), 2013-2024
## ===========================

## Last year prevalence
DT[ans3_1 == 1, lyp_cocaine := 1]
DT[ans3_2 == 1, lyp_mdma := 1]
DT[ans3_3 == 1, lyp_amphetamines := 1]

DTYg <- DT[agecat %chin% c("16-24", "25-34")]

movCocain <- calc_moving_prevalence(DTYg, "lyp_cocaine", "narkpop")
movCocain[, drug := "Cocaine"]
movAmphet <- calc_moving_prevalence(DTYg, "lyp_amphetamines", "narkpop")
movAmphet[, drug := "Amphetamines"]
movMdma <- calc_moving_prevalence(DTYg, "lyp_mdma", "narkpop")
movMdma[, drug := "MDMA/Ecstasy"]

tblMovDrugs <- rbindlist(list(movCocain, movAmphet, movMdma))
tblMovDrugs[, value := round(pct_roll, 2)]
tblMovDrugs <- tblMovDrugs[year >= 2014]
tblMovDrugs[, drug := factor(drug, levels = c("Cocaine", "Amphetamines", "MDMA/Ecstasy"))]

fig14 <- make_line_plot(data = tblMovDrugs,
                        x = "year",
                        y = "value",
                        color = "drug",
                        title = "3-year moving average of percentage for stimulants use last 12 months (16-34 yrs old), 2014-2024",
                        caption = "",
                        color_values = chc3,
                        label_col = "drug",
                        y_break_interval = 0.5,
                        y_limit = c(0, 10),
                        y_lab = "Cumulative Percentage (%)"
                        )

ggplot2::ggsave(filename = here::here("euda/workbook/figures/figure14_stimulant_use_movingavg_2014-2024.png"),
                plot = fig14,
                width = 10,
                height = 6,
                dpi = 300)
