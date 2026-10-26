
# Stacked line graph
# --------------------

dtc <- dcast(dagecat, year ~ alder, value.var = "antall")
cols <- names(dtc)[-1]
dtc[, total := rowSums(.SD), .SDcols = cols]
dtcl <- dtc[, c("year", "total")]

hc <- highchart() |>
  hc_legend(enabled = TRUE) |>
  highcharter::hc_colors(highdir::get_palette("hdir")) |>
#   highcharter::hc_theme(highcharter::hc_theme_smpl()) |>
  hc_chart(
    type = "area",
    backgroundColor = "#FFFFFF",
    plotBackgroundColor = "#FFFFFF"
  ) |>
  hc_xAxis(
    categories = dtc$year,
    title = list(text = "Årgang"),
    tickInterval = 1,
    allowDecimals = FALSE
  ) |>
  hc_yAxis(
    title = list(text = "Antall"),
    min = 40,
    gridLineColor = "#d9d9d9"
  ) |>
  hc_plotOptions(
    area = list(
      stacking = "normal",
      lineWidth = 1,
      marker = list(
        enabled = TRUE,
        radius = 3
      )
    ),
    series = list(
      dataLabels = list(enabled = FALSE)
    )
  ) |>
  # Bottom series
  hc_add_series(
    name = "0-29 år",
    data = dtc[[2]],
    fillOpacity = 0.5
  ) |>
  hc_add_series(
    name = "30-39 år",
    data = dtc[[3]],
    fillOpacity = 0.5
  ) |>
  hc_add_series(
    name = "40-49 år",
    data = dtc[[4]],
    fillOpacity = 0.5
  ) |>
  hc_add_series(
    name = "50-59 år",
    data = dtc[[5]],
    fillOpacity = 0.5
  ) |>
  hc_add_series(
    name = "60-69 år",
    data = dtc[[6]],
    fillOpacity = 0.5
  ) |>
  hc_add_series(
    name = "70 år og over",
    data = dtc[[7]],
    fillOpacity = 0.5
  ) |>
  ## # Overlay line
  hc_add_series(
  name = "Total",
  type = "line",
  data = dtc[["total"]],
  color = "black",
  lineWidth = 4
) |>
  hc_tooltip(
    shared = TRUE,
    valueDecimals = 0
  ) |>
  highcharter::hc_exporting(
    enabled = TRUE
  ) |>
  highcharter::hc_title(text = "Narkotikautløste dødsfall etter alderskategorier",
                        align = "left") |>
  highcharter::hc_subtitle(text = "Antall dødsfall fordelt på ulike aldersgrupper, 2021-2025",
                           align = "left")
