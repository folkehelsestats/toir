
debut <- DT[, .N, keyby = can3][!is.na(can3) & can3 != 99]

## # Ensure data.table and sorted by x
## data.table::setDT(debut2)
debut <- debut[!is.na(can3) & !is.na(N)][order(can3)]
debut[, pros := round(N/sum(N)*100, digits = 1)]

# Basic line chart (continuous x)
highcharter::hchart(debut, type = "line", hcaes(x = can3, y = pros)) %>%
  highcharter::hc_title(text = "Line chart: N over can3") %>%
  highcharter::hc_xAxis(title = list(text = "can3")) %>%
  highcharter::hc_yAxis(title = list(text = "N")) %>%
  highcharter::hc_colors("#2C7FB8") %>%
  highcharter::hc_tooltip(pointFormat = "Alder: {point.x}<br>Prosent: <b>{point.y}%</b>") %>%
  highcharter::hc_exporting(enabled = TRUE)

## # Build explicit point objects with x, y, and pros
## pts <- highcharter::list_parse2(
##   debut22[, .(x = as.numeric(can3), y = as.numeric(N), pros = pros)]
## )

## # Option A: pointFormat (simple and fast)
## highcharter::highchart() %>%
##   highcharter::hc_chart(type = "line") %>%
##   highcharter::hc_title(text = "N over can3 (med pros i tooltip)") %>%
##   highcharter::hc_xAxis(title = list(text = "can3")) %>%
##   highcharter::hc_yAxis(title = list(text = "N")) %>%
##   highcharter::hc_add_series(name = "N", data = pts, color = "#2C7FB8", lineWidth = 2) %>%
##   highcharter::hc_tooltip(
##     useHTML = TRUE,
##     headerFormat = "",
##     pointFormat = "N: <b>{point.y}</b><br/>Pros: <b>{point.pros}</b>"
##   )
