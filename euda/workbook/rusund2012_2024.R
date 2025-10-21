## if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
## library("here")

## ## ## Line Diagram
## source(file.path(here::here(), "euda/workbook/fun-line-plot.R"))
## source(file.path(here::here(), "euda/workbook/fun-prosent.R"))

dataPath <- "O:\\Prosjekt\\Rusdata"

## Data 2012 - 2024
ddt <- readRDS(file.path(dataPath, "Rusundersøkelsen", "Rusus historiske data", "data_2012_2024.rds"))
data.table::setDT(ddt)

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

chc4 <- c("#5F9EA0", "#E1B378", "#BB7BDA", "#7C145C")

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
  ## geom_text(data = dcan[year == 2024], aes(x = year, y = pro, label = canlab), hjust = -0.1) + #hjust -0.1 nudges label slightly to the right
  scale_x_continuous(
    limits = c(2011, 2024),
    breaks = seq(2011, 2024, 1)
    ## expand = expansion(mult = c(0.03, 0.25)) # Add 10% space to the right
  ) +
  scale_y_continuous(breaks = seq(0, max(dcan$pro) + 10, 10)) +
  theme_classic() +
  labs(title = "Number of cannabis seizures (resin and herbal/plants), 2011-2024",
       y = "", x = "") +
  theme(
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed")
  ) +
  guides(color = "none") +
  ## annotate("text", x = rep(2021, 3), y = c(4, 53, 75), label = c("canoil", "mari", "hasj"))
  geom_text(data = annotdf, aes(x = year, y = loc, label = canlab))
