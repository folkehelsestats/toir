## Data hentet fra https://www.politiet.no/om-politiet/tall-og-fakta/narkotika/
## -------------------------------

library(data.table)
library(ggplot2)


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

chdir <- c('#206276',
           '#7C145C',
           '#3699B6',
           '#D17A00',
           '#67978A',
           '#BB7BDA',
           '#959588',
           '#CD3D57',
           '#47A571')

chc <- c("#5F9EA0", "#E1B378", "#BB7BDA", "#7C145C")

## plot1
CanBesPlot <- ggplot(data = CanBesW[can != "total"], aes(x = year, y = antall, group = can)) +
  geom_col(aes(fill = can)) +
  geom_text(aes(label = antall), position = position_stack(vjust = 0.8), size = 2.8) +
  labs(title = "Number of cannabis seizures (resin and herbal/plants), 2011-2024",
       y = "", x = "") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(2011, 2024, 1)) +
  scale_fill_manual(values = chc, labels = c("herbal" = "Herbal/plants",
                                             "begge" = "Resin/herbal",
                                             "resin" = "Resin")) +
  theme_classic() +
  guides(fill = guide_legend(direction = "vertical", reverse = TRUE)) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.95, 0.95), # top-right corner
    legend.justification = c("right", "top"),  # aligns legend box to that corner
    legend.title = element_blank())

CanBesPlot

ggplot() +
  geom_line(data = CanBesW[can != "total"], aes(x = year, y = antall, color = can), size = 1.5) +
  geom_line(data = CanBesW[can == "total"], aes(x = year, y = antall, color = can), size = 3) +
  scale_color_manual(values = chc) +
  geom_text(data = CanBesW[year == 2024], aes(x = year, y = antall, label = canlab), hjust = -0.1) + #hjust -0.1 nudges label slightly to the right
  scale_x_continuous(
    limits = c(2011, 2024),
    breaks = seq(2011, 2024, 1),
    expand = expansion(mult = c(0.03, 0.25)) # Add 10% space to the right
  ) +
  scale_y_continuous(breaks = seq(0, max(CanBesW$antall), 1000)) +
  theme_classic() +
  labs(title = "Number of cannabis seizures (resin and herbal/plants), 2011-2024",
       y = "", x = "") +
  theme(
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed")
  ) +
  guides(color = "none")


## Cannabis KG
CanKgPlot <- ggplot(data = CanKgW, aes(x = year, y = kg, group = can)) +
  geom_col(aes(fill = can)) +
