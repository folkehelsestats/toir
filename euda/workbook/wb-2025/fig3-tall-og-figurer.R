# ## if needed -------------------------------------------------------------------
# pkgs <- c("ggplot2", "ggrepel")
# install.packages(pkgs)
# ## -----------------------------------------------------------------------------

library(torr)
library(data.table)
library(highdir)
library(ggplot2)
library(ggrepel)

## Data source and setup
## --------------------------------------
path25 <- here::here("euda", "workbook", "wb-2025")
options(scipen = 999) # disable scientific notation
source(here::here("setup.R")) #load packages
source(here::here("euda", "workbook", "wb-2025", "data.R"))

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
chc4 <- c("#5F9EA0", "#E1B378", "#BB7BDA", "#7C145C")

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
create_tbl_grp <- function(dt, type, grp, filter = 2025, value = "pct_roll") {
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

## Age groups
## -------------
DT <- torr::group_age_standard(DT, var = "alder", type = "rusund",
                         new_var = "agecat")

# ## Relevin exclusion
# ## ------------------
# ## OBS! Exclude those that used Relevin Ans2_d == 1

# ans2 <- grep("ans2_", names(DT), ignore.case = TRUE, value = TRUE)
# ansTmp <- paste0(ans2, "_tmp")

# DT[, (ansTmp) := lapply(.SD, function(x) {
#   fcase(
#     x == 1, 1,
#     x %in% c(2, 8, 9), NA,
#     default = NA
#   )
# }), .SDcols = ans2]

# DT[, ansSum := rowSums(.SD, na.rm = TRUE), .SDcols = ansTmp]
# # Remove ONLY those who answered yes to all drugs including Relevin
# DT <- DT[ansSum != 8]
# DT[, (ansTmp) := NULL]


## -----------------------------------------------------------------------------
## Create dinominator variables
## -----------------------------------------------------------------------------

## Exclude all missing and not answered Can1 or Ans1
## Denominator for Illigal rusmidler
## ------------
DT[, canpop := ifelse(can1 %in% 1:2, 1, 0)]
DT[, narkpop := ifelse(ans1 %in% 1:2, 1, 0)]
DT[canpop == 1 | narkpop == 1, anypop := 1][
  is.na(anypop), anypop := 0]

## using cuci package
## -----------------
CanVars = c("can1", "can6", "can10")
#Dette lager ltpPop_cannabis, lypPop_cannabis, lmpPop_cannabis
DT <- torr::create_cann_pop(DT, vars = CanVars)


## ===========================
## Figure 3 - Cannabis use last 12 months all adults (16-64), 2012-2025
## ==========================

ddt <- copy(DT) #Rename to use it locally

# clean labelled attributes coz it makes troubles
ddtc <- ddt[, lapply(.SD, \(col) {
  attributes(col) <- NULL
  col
})]

hasj <- calc_pro(ddtc, "can6", "can7_a", "hasj")
marihuana <- calc_pro(ddtc, "can6", "can7_b", "marihuana")
canoil <- calc_pro(ddtc, "can6", "can7_c", "canoil")

dcan <- data.table::rbindlist(list(hasj, marihuana, canoil), use.names = TRUE)
dcan[can == "hasj", canlab := "Resin"]
dcan[can == "marihuana", canlab := "Herbal/plants"]
dcan[can == "canoil", canlab := "Cannabis oil"]


chc4 <- c("#025169", "#0069E8",
                "#7C145C", "#047FA4",
                "#C68803", "#38A389",
                "#6996CE", "#366558",
                "#BF78DE", "#767676")

# chc4 <- c("#5F9EA0", "#E1B378", "#BB7BDA", "#7C145C")

annotdf <- dcan[year == 2021]
annotdf[, loc := fcase(can == "hasj", pro + 2 ,
                       can == "marihuana", pro - 2,
                       can == "canoil", pro - 2)]

library(ggplot2)
## Linje diagram
can12types <- ggplot() +
  geom_line(data = dcan[can != "total"], aes(x = year, y = pro, color = can), linewidth = 1.5) +
  geom_line(data = dcan[can == "total"], aes(x = year, y = pro, color = can), linewidth = 3) +
  scale_color_manual(values = chc4) +
  ## geom_text(data = dcan[year == 2025], aes(x = year, y = pro, label = canlab), hjust = -0.1) + #hjust -0.1 nudges label slightly to the right
  scale_x_continuous(
    limits = c(2012, 2025),
    breaks = seq(2012, 2025, 1)
    ## expand = expansion(mult = c(0.03, 0.25)) # Add 10% space to the right
  ) +
  scale_y_continuous(breaks = seq(0, max(dcan$pro) + 10, 10)) +
  theme_classic() +
  labs(title = "Types of cannabis used last 12 months all adults (16-64), 2012-2025",
       y = "", x = "") +
  theme(
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed")
  ) +
  guides(color = "none") +
  ## annotate("text", x = rep(2021, 3), y = c(4, 53, 75), label = c("canoil", "mari", "hasj"))
  geom_text(data = annotdf, aes(x = year, y = loc, label = canlab))


ggplot2::ggsave(filename = here::here(path25, "figures/cantypes.png"),
                plot = can12types,
                width = 8, height = 4, dpi = 300)

## -----------------------------------------------------------------------------
## Figure 4: Cannabis use all adults (16-64), 2012 - 2025
## -----------------------------------------------------------------------------
DD <- DT[alder < 65]

## Denominator
CanVars = c("can1", "can6", "can10")
#Dette lager ltpPop_cannabis, lypPop_cannabis, lmpPop_cannabis
DD <- torr::create_cann_pop(DD, vars = CanVars)

## Cannabis
## -----------------------------------------------------------------------------
DD <- torr::is_case(DD, "can1", "ltp_cannabis")
DD <- torr::is_case(DD, "can6", "lyp_cannabis")
DD <- torr::is_case(DD, "can10", "lmp_cannabis")

ltp64 <- torr::calc_prevalence(DD, "ltpPop_cannabis", "year", "ltp_cannabis", weight_var = "vekt2")
lyp64 <- torr::calc_prevalence(DD, "lypPop_cannabis", "year", "lyp_cannabis", weight_var = "vekt2")
lmp64 <- torr::calc_prevalence(DD, "lmpPop_cannabis", "year", "lmp_cannabis", weight_var = "vekt2")

source(here::here("euda/workbook/wb-2025/fun-fig4-fig5.R"))
fig4 <- create_dataset_fig4_fig5(ltp = ltp64, lyp = lyp64, lmp = lmp64)

source("c:/Users/ykama/Git-hdir/toir/euda/workbook/wb-2025/fig4-og-5.R")

gg64 <- make_gg(fig4$long_dt, fig4$label_dt, fig4$grid_dt,
                title = "Cannabis use all adults (16–64), 2012-2025",
                subtitle = "", cols = fig4$cols)

ggsave(
  filename = here::here(path25, "figures/can64.png"),
  plot = gg64,
  width = 8,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "white"
)

## -----------------------------------------------------------------------------
## Figure 5: Cannabis use young adults (16–34), 2012-2025
## -----------------------------------------------------------------------------
DU <- DT[alder < 35]

## Denominator
CanVars = c("can1", "can6", "can10")
#Dette lager ltpPop_cannabis, lypPop_cannabis, lmpPop_cannabis
DU <- torr::create_cann_pop(DU, vars = CanVars)

## Cannabis
## -----------------------------------------------------------------------------
DU <- torr::is_case(DU, "can1", "ltp_cannabis")
DU <- torr::is_case(DU, "can6", "lyp_cannabis")
DU <- torr::is_case(DU, "can10", "lmp_cannabis")

ltp34 <- torr::calc_prevalence(DU, "ltpPop_cannabis", "year", "ltp_cannabis", weight_var = "vekt2")
lyp34 <- torr::calc_prevalence(DU, "lypPop_cannabis", "year", "lyp_cannabis", weight_var = "vekt2")
lmp34 <- torr::calc_prevalence(DU, "lmpPop_cannabis", "year", "lmp_cannabis", weight_var = "vekt2")

source(here::here("euda/workbook/wb-2025/fun-fig4-fig5.R"))
fig5 <- create_dataset_fig4_fig5(ltp = ltp34, lyp = lyp34, lmp = lmp34)

source("c:/Users/ykama/Git-hdir/toir/euda/workbook/wb-2025/fig4-og-5.R")

gg34 <- make_gg(fig5$long_dt, fig5$label_dt, fig5$grid_dt,
                title = "Cannabis use young adults (16–34), 2012-2025",
                subtitle = "", cols = fig5$cols)

ggplot2::ggsave(
  filename = here::here(path25, "figures/can34.png"),
  plot = gg34,
  width = 8,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "white"
)


## --------------------------------------------------
## Figure 6: Cannabis use all adults (16-64) and young adults (16-34), 2022-2025
## --------------------------------------------------

## Dataset for 2023-2025 only
## -------------------------
dt <- DD[year %in% 2023:2025] #16-64

# Fix norsk bokstaver
source("https://raw.githubusercontent.com/folkehelsestats/rusus/refs/heads/main/encode.R")
dt[, ans2sps := fix_encode(ans2sps)]

## includes free text answers to the new variables
dt[grep("lsd", ans2sps, ignore.case = TRUE), "AndreLSD" := 1]
dt[grep("hasj", ans2sps, ignore.case = TRUE), "AndreCannabis" := 1]

dt[, Can1_ny := can1][AndreCannabis == 1, Can1_ny := 1]


## Age group 16-34 and functions
## ----------------------------
## source(here::here("euda/workbook/fun-moving-percentage.R"))
dty <- dt[alder < 35] #16-34 yrs

## Lifetime  prevalence
## ---------------------
dt <- torr::is_case(dt, "Can1_ny", "ltp_cannabis")
dty <- torr::is_case(dty, "Can1_ny", "ltp_cannabis")

source(here::here("euda/workbook/wb-2025/fun-prev-all.R"))

dlp64 <- create_prev_data(dt, denom = "ltpPop_cannabis",
                         outcome = "ltp_cannabis",
                         pop = "All Adults (16-64)",
                         grp = "LTP")

dlp34 <- create_prev_data(dty, denom = "ltpPop_cannabis",
                         outcome = "ltp_cannabis",
                         pop = "Young Adults (16-34)",
                         grp = "LTP")

## Last year prevalence
## ---------------------

dt  <- torr::is_case(dt, "can6", "lyp_cannabis")
dty <- torr::is_case(dty, "can6", "lyp_cannabis")

dyp64 <- create_prev_data(dt, denom = "lypPop_cannabis",
                          outcome = "lyp_cannabis",
                          pop = "All Adults (16-64)",
                          grp = "LYP")

dyp34 <- create_prev_data(dty, denom = "lypPop_cannabis",
                          outcome = "lyp_cannabis",
                          pop = "Young Adults (16-34)",
                          grp = "LYP")

## Last month prevalence
## ---------------------

dt  <- torr::is_case(dt, "can10", "lmp_cannabis")
dty <- torr::is_case(dty, "can10", "lmp_cannabis")

dmp64 <- create_prev_data(dt, denom = "lmpPop_cannabis",
                          outcome = "lmp_cannabis",
                          pop = "All Adults (16-64)",
                          grp = "LMP")

dmp34 <- create_prev_data(dty, denom = "lmpPop_cannabis",
                          outcome = "lmp_cannabis",
                          pop = "Young Adults (16-34)",
                          grp = "LMP")


DX <- data.table::rbindlist(list(dlp64, dlp34, dyp64, dyp34, dmp64, dmp34), use.names = TRUE)

old <- c("years", "kjonn", "pros", "ci_l", "ci_u", "pop", "grp", "gender")
new <- c("years", "kjonn", "Estimate", "LowerCI", "UpperCI", "Population", "Prevalence", "Group")
data.table::setnames(DX, old,new)

## Plotting
## ---------------------
fig6 <- create_fig_ci(DX)

ggplot2::ggsave(
  filename = here::here(path25, "figures/fig6.png"),
  plot = fig6,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)


## ----------------------------------
## Figure 7 - Cannabis use across age groups, 2022-2025
## ----------------------------------

ltCanAge <- torr::ca

ltpCanAge <- torr::calc_prevalence(dt, "ltpPop_cannabis", "year", "ltp_cannabis", weight_var = "vekt2", by = "agecat", rolling_year = 3)
lypCanAge <- torr::calc_prevalence(dt, "lypPop_cannabis", "year", "lyp_cannabis", weight_var = "vekt2", by = "agecat", rolling_year = 3)
lmpCanAge <- torr::calc_prevalence(dt, "lmpPop_cannabis", "year", "lmp_cannabis", weight_var = "vekt2", by = "agecat", rolling_year = 3)

ltpCanAge[, type := "LTP"][, value := ltp_cannabis][, ltp_cannabis := NULL]
lypCanAge[, type := "LYP"][, value := lyp_cannabis][, lyp_cannabis := NULL]
lmpCanAge[, type := "LMP"][, value := lmp_cannabis][, lmp_cannabis := NULL]


tblCanAge <- rbindlist(list(
  ltpCanAge,
  lypCanAge,
  lmpCanAge
))

tblCanAge[, type := factor(type, levels = c("LTP", "LYP", "LMP"))]

# Create the plot
fig7 <- create_plot(tblCanAge,
                    x = "type", y = "value", fill = "agecat",
                    hdir_color = hdir_color, wrap = NULL,
                    title = "Cumulative percentage of cannabis use across age groups (2023-2025)",
                    xlab = "Prevalence Type", ylab = "Percentage (%)", lglab = "Age"
                    )

ggplot2::ggsave(
  filename = here::here(path25, "figures/fig7.png"),
  plot = fig7,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

## ---------------------
## Figure 8: Freq of cannabis use all adults (16-64), 2022-2025
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
            title = "Frequency of cannabis use among users (2023-2025)",
            xlab = "",
            ylab = "Cumulative Percentage (%)", lglab = "", show_legend = FALSE)


ggplot2::ggsave(
  filename = here::here(path25, "figures/fig8.png"),
  plot = fig8,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)


## ----------------------------------
## Figure 11 - Number of amphetamines seizures (methamphetamine and amphetamine), 2008-2025
## ----------------------------------
## Data hentet fra https://www.politiet.no/om-politiet/tall-og-fakta/narkotika/

## source(file.path(here::here(), "euda/workbook/fun-line-plot.R"))
## source(file.path(here::here(), "euda/workbook/fun-prosent.R"))

## Line plot with legend
source(file.path(here::here(), "euda/workbook/fun-make_line_plot2.R"))

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

fig11 <- make_line_plot2(data = amfiW,
                        x = "year",
                        y = "antall",
                        color = "amfi",
                        title = "Number of amphetamines seizures (methamphetamine and amphetamine), 2010-2025",
                        caption = "Source: National Crime Investigation Service (Kripos/NCIS)",
                        color_values = chc3,
                        label_col = "amfi",
                        y_lab = NULL,
                        legend = TRUE,
                        legend_title = "")


ggplot2::ggsave(
  filename = here::here(path25, "figures/fig11.png"),
  plot = fig11,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

## ===========================
## Figure 11.1 - Amphetamines seizures in kg
## ==========================

amfiKg <- readxl::read_xlsx(here::here("euda/workbook/data/ammfetamin-seizures-kg.xlsx"), sheet = "Ark1")
data.table::setDT(amfiKg)
setnames(amfiKg, names(amfiKg), tolower(names(amfiKg)))
setnames(amfiKg, "amfetamin/metamfetaminbase", "amphetamine base")

amfiKg[, total := rowSums(.SD, na.rm = TRUE), by = year]
amfiKgDT <- melt(amfiKg, id.vars = "year",
                measure.vars = c("metamfetamin", "amfetamin", "amphetamine base", "total"),
                value.name = "kg",
                variable.name = "amfi")

amfiKgLine <- make_line_plot2(data = amfiKgDT,
                             x = "year",
                             y = "kg",
                             color = "amfi",
                             title = "Seized amphetamines in kg (methamphetamine and amphetamine), 2015-2025",
                             caption = "Source: National Crime Investigation Service (Kripos/NCIS)",
                             color_values = chc4,
                             label_col = "amfi",
                             y_lab = "Kilograms (kg)",
                             legend = TRUE,
                             legend_title = ""
                             )

ggplot2::ggsave(
  filename = here::here(path25, "figures/fig11-1.png"),
  plot = amfiKgLine,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

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
                     title = "Use of stimulants (LTP and LYP) all adults (16–64), 2022-2025",
                     lglab = "", xlab = "", ylab = "Cumulative Percentage (%)"
                     )

ggplot2::ggsave(filename = here::here("euda/workbook/figures/figure12_stimulant_use_2022-2025.png"),
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
                     title = "Use of stimulants (LTP and LYP) young adults (16–34), 2022-2025",
                     lglab = "", xlab = "", ylab = "Cumulative Percentage (%)"
                     )

ggplot2::ggsave(filename = here::here("euda/workbook/figures/figure13_stimulant_use_young_2022-2025.png"),
                plot = fig13,
                width = 10,
                height = 6,
                dpi = 300)

## ===========================
## Figure 14 - Use of stimulants last 12 months in young adults (16-34), 2013-2025
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
                        title = "3-year moving average of percentage for stimulants use last 12 months (16-34 yrs old), 2014-2025",
                        caption = "",
                        color_values = chc4,
                        label_col = "drug",
                        y_break_interval = 0.5,
                        y_limit = c(0, 10),
                        y_lab = "Cumulative Percentage (%)"
                        )

ggplot2::ggsave(filename = here::here("euda/workbook/figures/figure14_stimulant_use_movingavg_2014-2025.png"),
                plot = fig14,
                width = 10,
                height = 6,
                dpi = 300)

## ===========================
## Figure 15 - Estimated number of people injecting drugs in Norway, 2010–2022
## ==========================

injdt <- readxl::read_xlsx(here::here("O:\\Prosjekt\\Rusdata\\PWID\\pwid_workbook2025.xlsx"),
                           sheet = "Ark1")
setDT(injdt)
ids <- injdt[, .N, keyby = År]$År
ids <- ids[!is.na(ids)]
injdt[, item := as]
injdt <- injdt[!is.na(År)]

omkod <- data.table(v1 = ids, v2 = c("Antall", "diedMX", "pwidMX",
                                     "yearMX", "lowCI", "sd", "sdMX",
                                     "tot", "highCI"))

injdt[, item := c("Antall", "pwid", "sd", "upCI",
                  "lowCI", "yrMX", "diedMX", "pwidMX",
                  "sdMX", "upCI_MX", "lowCI_MX")]

injx <- injdt[item %in% c("pwid", "pwidMX")]
injx[, År := NULL]

pwid <- melt(injx, id.vars = "item",
             measure.vars = c("2019", "2020", "2021", "2022", "2023", "2025"),
             variable.name = "year",
             value.name = "n")

pwid[, year := as.integer(as.character(year))]

make_line_plot(pwid, x = "year", y = "n", color = "item",
               title = "Estimated number of people injecting drugs in Norway, 2019-2025",
               color_values = chc3,
               label_col = "item",
               y_lab = "Estimated number of people injecting drugs",
               y_limit = c(0, 30000)
               )


library(ggplot2)
## Linje diagram
pwidPlot <- ggplot() +
  geom_line(data = pwid, aes(x = year, y = n, color = item), linewidth = 1.5) +
  scale_color_manual(values = chc4) +
  ## geom_text(data = dcan[year == 2025], aes(x = year, y = pro, label = canlab), hjust = -0.1) + #hjust -0.1 nudges label slightly to the right
  scale_x_continuous(
    limits = c(2019, 2025),
    breaks = seq(2019, 2025, 1)
    ## expand = expansion(mult = c(0.03, 0.25)) # Add 10% space to the right
  ) +
  ## scale_y_continuous(breaks = seq(0, max(pwid$n) + 15000, 100)) +
  theme_classic() +
  labs(title = "Estimated number of people injecting drugs in Norway, 2010–2022",
       y = "", x = "") +
  theme(
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed")
  ) +
  guides(color = "none")
  ## annotate("text", x = rep(2021, 3), y = c(4, 53, 75), label = c("canoil", "mari", "hasj"))


ggplot2::ggsave(filename = here::here("euda/workbook/figures/pwid.png"),
                plot = can12types,
                width = 6, height = 4, dpi = 300)
