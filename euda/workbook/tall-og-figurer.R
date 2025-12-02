## Data source and setup
## --------------------------------------
source(here::here("setup.R"))
source(here::here("data-source.R"))

# clean labelled attributes coz it makes troubles
DT[, (names(DT)) := lapply(.SD, \(x) { attributes(x) <- NULL; x })]

DT[year %in% c(2022,2024), kjonn := fcase(kjonn == 1, 2, #Kvinne
                                          kjonn == 0, 1)] #Mann
dt <- DT[year %in% 2022:2024]


## Relevin exclusion
## ------------------
## OBS! Exclude those that used Relevin Ans2_d == 1

ans2 <- grep("ans2_", names(dt), ignore.case = TRUE, value = TRUE)
ansTmp <- paste0(ans2, "_tmp")

dt[, (ansTmp) := lapply(.SD, function(x) {
  fcase(
    x == 1, 1,
    x %in% c(2, 8, 9), NA,
    default = NA
  )
}), .SDcols = ans2]

dt[, ansSum := rowSums(.SD, na.rm = TRUE), .SDcols = ansTmp]
# Remove ONLY those who answered yes to all drugs including Relevin
dt <- dt[ansSum != 8]
dt[, (ansTmp) := NULL]

## Age groups
## -------------
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-age.R")

dt <- group_age_standard(dt, var = "alder", type = "rusund",
                         new_var = "agecat")

## Exclude all missing and not answered Can1 or Ans1
## Denominator for Illigal rusmidler
## ------------
dt[, canpop := ifelse(can1 %in% 1:2, 1, 0)]
dt[, narkpop := ifelse(ans1 %in% 1:2, 1, 0)]
dt[canpop == 1 | narkpop == 1, anypop := 1][
  is.na(anypop), anypop := 0]

## --------------------------------------------------
## Figure 6: Cannabis use all adults (16-64) and young adults (16-34), 2022-2024
## --------------------------------------------------

## includes free text answers to the new variables
dt[grep("lsd", ans2sps, ignore.case = TRUE), "AndreLSD" := 1]
dt[grep("hasj", ans2sps, ignore.case = TRUE), "AndreCannabis" := 1]

dt[, Can1_ny := can1][AndreCannabis == 1, Can1_ny := 1]


## Age group 16-34 and functions
## ----------------------------
source(here::here("euda/workbook/fun-moving-percentage.R"))
dty <- dt[agecat %chin% c("16-24", "25-34")] #16-34 yrs

create_tbl_grp <- function(dt, type, grp, filter = 2024, value = "pct_roll") {
  dt <- dt[year == filter]
  dt[, type := type]

  if (!missing(grp))
    dt[, grp := grp]

  dt[, value := round(get(value), 1)]
  dt[, year := NULL]
}

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

hdir_color <- c("#025169", "#0069E8",
                "#7C145C", "#047FA4",
                "#C68803", "#38A389",
                "#6996CE", "#366558",
                "#BF78DE", "#767676")

source(here::here("euda/workbook/fun-plot-facet.R"))
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

ltCanAge <- calc_moving_prevalence_multi(dt, "ltp_cannabis", "canpop", "agecat")
lyCanAge <- calc_moving_prevalence(dt, "lyp_cannabis", "canpop", "agecat")
lmCanAge <- calc_moving_prevalence(dt, "lmp_cannabis", "canpop", "agecat")


tblCanAge <- rbindlist(list(
  create_tbl_grp(ltCanAge, "LTP"),
  create_tbl_grp(lyCanAge, "LYP"),
  create_tbl_grp(lmCanAge, "LMP")
))

tblCanAge <- tblCanAge[agecat != "65-79"]

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

source(here::here("euda/workbook/fun-plot-facet.R"))
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

source(file.path(here::here(), "euda/workbook/fun-line-plot.R"))
source(file.path(here::here(), "euda/workbook/fun-prosent.R"))

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


chc3 <- c("#5F9EA0", "#E1B378", "#7C145C")

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
