## Data hentet fra https://www.politiet.no/om-politiet/tall-og-fakta/narkotika/
## -------------------------------

library(data.table)
library(ggplot2)

## resin = hasj
## herbal = marihuana

## Number of cannabis seizures
## Cannabis - antall beslag
CanBeslag <- data.table(
  year = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024),
  resin = c(9949, 9729, 10391, 8945, 10124, 9953, 8779, 8023, 10026, 5814, 5040, 4533, 5082, 5202),
  begge = c(1182, 1373, 1501, 1616, 1269, 959, 1729, 1413, 1375, 938, 867, 728, 604, 737),
  herbal = c(2829, 3482, 4337, 5287, 3480, 2447, 2831, 2738, 1647, 2796, 1938, 1420, 1306, 1608)
)

CanBeslag[, total := rowSums(.SD), by = year]


## Amount of cannabis seized (resin and herbal/plants) in kilograms, 2011-2024
## Cannabis - beslaglagt mengde (kg)
CanKg <- data.table(
  year = 2011:2024,
  resin = c(2548, 1630, 2285, 1919, 2018, 3026, 2048, 2658, 2244, 1785, 1513, 3971, 3281, 2591),
  herbal  = c(433, 454, 653, 829, 368, 683, 508, 354, 457, 819, 737, 1173, 533, 1087)
)

CanKg[, total := rowSums(.SD), by = year]


## Figure
## ------
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

## ## Line Diagram
## source(file.path(here::here(), "euda/workbook/fun-line-plot.R"))

CanBesLine <- make_line_plot(data = CanBesW,
                             x = "year",
                             y = "antall",
                             color = "can",
                             title = "Number of cannabis seizures (resin and herbal/plants), 2011-2024",
                             caption = "Source: National Crime Investigation Service (Kripos/NCIS)",
                             color_values = chc4,
                             label_col = "canlab",
                             y_lab = NULL)



## Cannabis KG
CanKgLine <- make_line_plot(data = CanKgW,
                            x = "year",
                            y = "kg",
                            color = "can",
                            title = "Number of cannabis seizures (resin and herbal/plants) in kilograms, 2011-2024",
                            caption = "Source: National Crime Investigation Service (Kripos/NCIS)",
                            y_break_interval = 500,
                            color_values = chc3,
                            label_col = "canlab",
                            y_lab = "kilograms (kg)")

## Numbers
## KG
kg2023 <- CanKgW[year == 2023 & can == "total", kg]
kg2024 <- CanKgW[year == 2024 & can == "total", kg]

## Resin
resin2019 <- CanBesW[year == 2019 & can == "resin", antall]
resin2024 <- CanBesW[year == 2024 & can == "resin", antall]
nedResin2019_2024 <- round((resin2019 - resin2024)/resin2019*100, digits = 1)

## Total
tot2023 <- CanKgW[year == 2023 & can == "total", kg]
tot2024 <- CanKgW[year == 2024 & can == "total", kg]

totKG2023_2024 <- tot2024-tot2023
sts <- totKG2023_2024 < 0 # check if it's decreasing or increasing
kgStatus2023_2024 <- round(abs(totKG2023_2024)/tot2023*100, digits = 1)

## Herbal
herbal2024 <- CanKgW[year == 2024 & can == "herbal", kg]
herbal2024pro <- round(herbal2024/tot2024*100, digits = 1)

herbal2023 <- CanKgW[year == 2023 & can == "herbal", kg]
herbal2023pro <- round(herbal2023/tot2023*100, digits = 1)
