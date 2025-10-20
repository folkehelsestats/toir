if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library("here")

## ## ## Line Diagram
## source(file.path(here::here(), "euda/workbook/fun-line-plot.R"))

dataPath <- "O:\\Prosjekt\\Rusdata"

## Data 2012 - 2024
ddt <- readRDS(file.path(dataPath, "Rusundersøkelsen", "Rusus historiske data", "data_2012_2024.rds"))


hasj <- calc_pro(ddt, "can6", "can7_a", "hasj")
marihuana <- calc_pro(ddt, "can6", "can7_b", "marihuana")
canoil <- calc_pro(ddt, "can6", "can7_c", "canoil")

dcan <- data.table::rbindlist(list(hasj, marihuana, canoil), use.names = TRUE)
dcan[can == "hasj", canlab := "Resin"]
dcan[can == "marihuana", canlab := "Herbal/plants"]
dcan[can == "canoil", canlab := "Cannabis oil"]

chc4 <- c("#5F9EA0", "#E1B378", "#BB7BDA", "#7C145C")

can12types <- make_line_plot(data = dcan,
                             x = "year",
                             y = "pro",
                             color = "can",
                             title = "Type of cannabis used last 12 months all adults (16-64), 2012-2024",
                             color_values = chc4,
                             label_col = "canlab",
                             y_breaks = seq(0,80,10),
                             y_lab = "Percentage",
                             hvjust = c(-0.3, 0))
