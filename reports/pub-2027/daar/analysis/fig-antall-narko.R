
library(highcharter)

dodFil <- "Hovedkategorier 1996-2025.xlsx"
dodDT <- rio::import(file.path(path, dodFil), encoding = "Latin-1")

setnames(dodDT, names(dodDT), c("year", "narko", "psykisk", "uten", "med"))

DT <- dodDT[, 1:2]
# Calculate long-term average
avg <- mean(DT$narko, na.rm = TRUE)

# Create data for Highcharts
plotDT <- copy(DT)
setDT(plotDT)
plotDT[, deviation := narko - avg]
plotDT[, color := ifelse(deviation > 0, "#7C145C", "#025169")]

plotDT[, point := purrr::pmap(
  list(deviation, color, narko),
  ~list(
    y = ..1,
    color = ..2,
    narko = ..3
  )
)]

narkoAntall <- highchart() |>
  hc_chart(type = "column") |>
  hc_xAxis(categories = plotDT$year) |>
  hc_yAxis(
    title = list(text = "Forskjellen fra gjennomsnittet antall"),
    plotLines = list(list(value = 0, color = "black", width = 1))
  ) |>
  hc_add_series(
    data = plotDT$point,
    showInLegend = FALSE
  ) |>
  hc_tooltip(
    useHTML = TRUE,
    pointFormatter = JS("
      function() {
        return '<b>År:</b> ' + this.category +
               '<br><b>Antall:</b> ' + this.narko +
               '<br><b>Forskjellen:</b> ' + Highcharts.numberFormat(this.y,1);
      }
    ")
  ) |>
  highcharter::hc_exporting(
    enabled = TRUE
  ) |>
  highcharter::hc_title(text = "Antall rarkotikautløste dødsfall",
                        align = "left") |>
  highcharter::hc_subtitle(text = "Forskjellen mellom gjennomsnittet antallet i 1996-2025",
                           align = "left")


# highdir::get_palette()
#  [1] "#025169" "#0069E8" "#7C145C" "#C68803" "#047FA4" "#38A389" "#6996CE" "#366558" "#BF78DE"
# [10] "#767676"
