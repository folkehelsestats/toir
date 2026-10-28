library(data.table)
library(highcharter)


## solution ----
figStoff <- hchart(
  stoff_long,
  "area",
  hcaes(
    x = year,
    y = antall,
    group = Stofftype
  )
) |>
  hc_legend(enabled = TRUE) |>
  highcharter::hc_colors(highdir::get_palette("hdir")) |>
#   hc_size(width = 1400, height = 600) |>
  hc_plotOptions(
    area = list(stacking = "normal")
  ) |>
  hc_xAxis(
    title = list(text = ""),
    tickInterval = 1,
    allowDecimals = FALSE,
    labels = list(rotation = -45, step = 1),
    minPadding = 0,
    maxPadding = 0
  ) |>
  hc_tooltip(
    shared = TRUE,
    crosshairs = TRUE
  ) |>
  highcharter::hc_exporting(
    enabled = TRUE
  ) |>
  highcharter::hc_title(text = "Narkotikautløste dødsfall etter stoffer",
                        align = "left") |>
  highcharter::hc_subtitle(text = "Antall dødsfall i perioden, 2006-2025",
                           align = "left")
