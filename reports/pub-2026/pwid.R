## Skal ikke publiseres før 18.mars
## --------------------------------------------
pkgs <- c("data.table", "rio", "highcharter")
sapply(pkgs, require, character.only = TRUE)

fwid <- "O:\\Prosjekt\\Rusdata\\PWID\\pwid_rolling_mean.xlsx"
pwid <- rio::import(fwid, sheet = "Ark1") |> as.data.table()
pwid[, var := 1:.N]

pwid[, var := factor(var, labels = c("dod", "sproy", "sd", "lowCI", "upCI"))]

# 1. Melt wide → long
long_dt <- melt(
  pwid[, -1],
  id.vars = "var",
  variable.name = "year",
  value.name = "value",
  variable.factor = FALSE
)

# 2. Cast long → wide: var becomes columns
wide_dt <- dcast(
  long_dt,
  year ~ var,
  value.var = "value"
)

## wide_dt[, year := as.numeric(year)]

title1 <- "Beregnet antall PWID blant rusmiddelbrukere i Norge"
subtitle1 <- "Data fra 2019-2024. En rullerende gjennomsnitt over 3 år"

## Create base chart with title and subtitle
hcx <- highcharter::highchart() |>
  highcharter::hc_title(
                 text = title1,
                 margin = 20,
                 align = "left",
                 style = list(useHTML = TRUE)
               ) |>
  highcharter::hc_subtitle(
                 text = subtitle1,
                 align = "left"
               )

x_col <- "year"
y_col <- "sproy"
line_color = "#206276"

## Add main line series
hc0 <- hcx |>
  highcharter::hc_add_series(
                 data = wide_dt,
                 name = "PWID",
                 type = "line",
                 id = "ci",
                 highcharter::hcaes(x = .data[[x_col]], y = .data[[y_col]]),
                 lineWidth = 2,
                 showInLegend = FALSE,
                 color = highcharter::hex_to_rgba(line_color),
                 marker = list(
                   symbol = "circle" ,
                   enabled = TRUE,
                   radius = 4
                 )
               )


y_axis_title = "Glidende gjennomsnitt sprøytebrukere"
ylim = c(0, 15000)

## Configure y-axis
## if (!is.null(ylim)) {
  hc1 <- hc0 |>
    highcharter::hc_yAxis(
                   title = list(text = y_axis_title),
                   accessibility = list(
                     enabled = TRUE,
                     description = tolower(y_axis_title)
                   ),
                   tickInterval = 1000, #tick only for each 1000
                   min = ylim[1],
                   max = ylim[2]
                 )
## } else {
##   hc1 <- hc0 |>
##     highcharter::hc_yAxis(
##                    title = list(text = y_axis_title),
##                    accessibility = list(
##                      enabled = TRUE,
##                      description = tolower(y_axis_title)
##                    ),
##                    min = 0
##                  )
## }

lower_col = "lowCI"
upper_col = "upCI"
## Add confidence interval area
hc2 <- hc1 |>
  highcharter::hc_add_series(
                 data = wide_dt,
                 name = "95% CI",
                 type = "arearange",
                 highcharter::hcaes(x = .data[[x_col]], low = .data[[lower_col]], high = .data[[upper_col]]),
                 linkedTo = "ci",
                 showInLegend = FALSE,
                 color = highcharter::hex_to_rgba(line_color),
                 fillOpacity = 0.6,
                 lineWidth = 0,
                 marker = list(enabled = FALSE)
               )


x_axis_title = "Årgangene"
caption = "Kilde: bla.. bla.."
credits_text = "Hdir"
credits_href = "https://www.hdir.no"
## Add axes, caption, and credits
  hc3 <- hc2 |>
    highcharter::hc_xAxis(
                   title = list(text = x_axis_title),
                   accessibility = list(
                     enabled = TRUE,
                     ## description = paste0("årgangene fra ", min(data[[x_col]]), " til ", max(data[[x_col]]))
                     description = "Data fra 2019 - 2024",
                     tickInterval = 1,
                     allowDecimals = FALSE
                   )
                 ) |>
    highcharter::hc_caption(text = caption,
                            align = "right") |>
    highcharter::hc_credits(
                   enabled = FALSE,
                   text = credits_text,
                   href = credits_href
                 )


## Add tooltip, exporting, and accessibility
hc4 <- hc3 |>
  highcharter::hc_tooltip(shared = TRUE) |>
  highcharter::hc_exporting(enabled = TRUE) |>
  highcharter::hc_add_dependency(name = "modules/accessibility.js")


#### ------------------------------------------
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-ci-graph.R")

create_ci_graph(wide_dt,
                x_col = "year",
                y_col = "sproy",
                lower_col = "lowCI",
                upper_col = "upCI")
