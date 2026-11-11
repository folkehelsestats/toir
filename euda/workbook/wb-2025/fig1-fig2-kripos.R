## Data hentet fra https://www.politiet.no/om-politiet/tall-og-fakta/narkotika/
## -------------------------------

root <- "c:/Users/ykama/Git-hdir/toir/euda/workbook/wb-2025"
source(file.path(root, "setup.R"))

library(data.table)
library(ggplot2)

## resin = hasj
## herbal = marihuana

## Number of cannabis seizures
## Cannabis - antall beslag
CanBeslag <- data.table(
  year = 2011:2025,
  resin = c(9949, 9729, 10391, 8945, 10124, 9953, 8779, 8023, 10026, 5814, 5040, 4533, 5082, 5202, 4534),
  begge = c(1182, 1373, 1501, 1616, 1269, 959, 1729, 1413, 1375, 938, 867, 728, 604, 737, 630),
  herbal = c(2829, 3482, 4337, 5287, 3480, 2447, 2831, 2738, 1647, 2796, 1938, 1420, 1306, 1608, 1841)
)

CanBeslag[, total := rowSums(.SD), by = year]


## Amount of cannabis seized (resin and herbal/plants) in kilograms, 2011-2025
## Cannabis - beslaglagt mengde (kg)
CanKg <- data.table(
  year = 2011:2025,
  resin = c(2548, 1630, 2285, 1919, 2018, 3026, 2048, 2658, 2244, 1785, 1513, 3971, 3281, 2591, 3290),
  herbal  = c(433, 454, 653, 829, 368, 683, 508, 354, 457, 819, 737, 1173, 533, 1087, 2144)
)

CanKg[, total := rowSums(.SD), by = year]

CanBesW <- melt(CanBeslag, id.vars = "year",
                measure.vars = c("herbal", "begge", "resin", "total"),
                value.name = "antall",
                variable.name = "can")

CanBesW[, canlab := fcase(can == "herbal", "Herbal/plants",
                          can == "begge", "Resin/herbal",
                          can == "resin", "Resin",
                          can == "total", "Total seized")]

CanKgW <- melt(CanKg, id.vars = "year",
               measure.vars = c("resin", "herbal", "total"),
               value.name = "kg",
               variable.name = "can")

CanKgW[, canlab := fcase(can == "herbal", "Herbal/plants",
                         can == "resin", "Resin",
                         can == "total", "Total seized")]

## HDIR prefered colors
chdir <- c('#206276',
           '#7C145C',
           '#3699B6',
           '#D17A00',
           '#67978A',
           '#BB7BDA',
           '#959588',
           '#CD3D57',
           '#47A571')

chc4 <- c("#5F9EA0", "#E1B378", "#BB7BDA", "#7C145C")
chc3 <- c("#5F9EA0", "#E1B378", "#7C145C")

## Figure 1
## ----------
## ## Line Diagram
## source(file.path(here::here(), "euda/workbook/fun-line-plot.R"))

CanBesLine <- make_line_plot(data = CanBesW,
                             x = "year",
                             y = "antall",
                             color = "can",
                             title = "Number of cannabis seizures (resin and herbal/plants), 2011-2025",
                             caption = "Source: National Crime Investigation Service (Kripos/NCIS)",
                             color_values = chc4,
                             label_col = "canlab",
                             y_lab = NULL)

ggplot2::ggsave(file.path(root, "figures/beslag2025.png"), CanBesLine)


## Figure 2
## ---------
## Cannabis KG
CanKgLine <- make_line_plot(data = CanKgW,
                            x = "year",
                            y = "kg",
                            color = "can",
                            title = "Number of cannabis seizures (resin and herbal/plants) in kilograms, 2011-2025",
                            caption = "Source: National Crime Investigation Service (Kripos/NCIS)",
                            y_break_interval = 500,
                            color_values = chc3,
                            label_col = "canlab",
                            y_lab = "kilograms (kg)")


ggplot2::ggsave(file.path(root, "figures/beslagKg2025.png"), CanKgLine)


## -----------------------------------------------------------------------------
## Amount seized
## -----------------------------------------------------------------------------
## CanBes / CanBesW - Beslag data long/wide format
## CanKg / CanKgW - Amount i kg long/wide format

## Change in total seized from last year 2024 vs 2025
tot25 <- CanBeslag[year == 2025, total]
tot24 <- CanBeslag[year == 2024, total]
round((tot25 - tot24)/tot24*100, digits = 1)

## Total KG change from 2024 to 2025
kg2024 <- CanKg[year == 2024, total]
kg2025 <- CanKg[year == 2025, total]
round((kg2025 - kg2024)/kg2024*100, digits = 1)


## Resin
resin2019 <- CanBesW[year == 2019 & can == "resin", antall]
resin2025 <- CanBesW[year == 2025 & can == "resin", antall]
round((resin2019 - resin2025)/resin2019*100, digits = 1)

res25 <- CanKg[year == 2025, resin]
round(res25/kg2025*100, digits = 1)

## Total kg
tot2024 <- CanKgW[year == 2024 & can == "total", kg]
tot2025 <- CanKgW[year == 2025 & can == "total", kg]

totKG2024_2025 <- tot2025-tot2024
sts <- totKG2024_2025 < 0 # check if it's decreasing or increasing
kgStatus2024_2025 <- round(abs(totKG2024_2025)/tot2024*100, digits = 1)

## Herbal kg
herbal2025 <- CanKgW[year == 2025 & can == "herbal", kg]
round(herbal2025/tot2025*100, digits = 1)

herbal2024 <- CanKgW[year == 2024 & can == "herbal", kg]
round(herbal2024/tot2024*100, digits = 1)

her24 <- CanKg[year == 2024, herbal]
round(her24/kg2024*100, digits = 1)

her25 <- CanKg[year == 2025, herbal]
round(her25/kg2025*100, digits = 1)
