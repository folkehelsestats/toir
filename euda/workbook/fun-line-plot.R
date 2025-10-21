#' Create a Line Plot with Highlighted Total Line
#'
#' Creates a line plot with multiple groups, where one group (typically "total")
#' is emphasized with a thicker line. Labels are automatically added at the end
#' of each line.
#'
#' @param data A data frame containing the data to plot
#' @param x Character string specifying the column name for x-axis variable
#' @param y Character string specifying the column name for y-axis variable
#' @param color Character string specifying the column name for color grouping variable
#' @param title Character string for the plot title
#' @param caption Give credit or footnote eg. Source: bla..bla..bla.
#' @param color_values Named vector of colors for each group. If NULL, uses ggplot2 defaults
#' @param highlight_value Value in the color column to highlight with thicker line (default: "total")
#' @param highlight_width Line width for highlighted group (default: 3)
#' @param regular_width Line width for regular groups (default: 1.5)
#' @param label_col Character string specifying column name for labels. If NULL, uses color column
#' @param x_breaks Numeric vector of x-axis breaks. If NULL, uses ggplot2 defaults
#' @param y_breaks Numeric vector of y-axis breaks eg. seq(0,80,10). If NULL, calculated automatically
#' @param y_break_interval Numeric value for y-axis break intervals (default: 1000)
#' @param x_limits Numeric vector of length 2 for x-axis limits. If NULL, uses data range
#' @param x_expansion Numeric vector of length 2 for expansion multipliers (default: c(0.03, 0.25) to add 10% to the right)
#' @param show_grid Logical, whether to show horizontal grid lines (default: TRUE)
#' @param x_lab Character string for x-axis label (default: "")
#' @param y_lab Character string for y-axis label (default: "")
#' @param hvjust Adjust position of text legend horizontally and vertically eg. c(-0.1, 0.2) horizontally slightly to the right and vertially up
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' make_line_plot(
#'   data = CanBesW,
#'   x = "year",
#'   y = "antall",
#'   color = "can",
#'   title = "Number of cannabis seizures, 2011-2024"
#' )
#'
#' # With custom colors
#' colors <- c("resin" = "#E69F00", "herbal" = "#56B4E9", "total" = "#000000")
#' make_line_plot(
#'   data = CanBesW,
#'   x = "year",
#'   y = "antall",
#'   color = "can",
#'   title = "Cannabis Seizures",
#'   color_values = colors
#' )
#' }
make_line_plot <- function(data,
                           x,
                           y,
                           color,
                           title,
                           caption = NULL,
                           color_values = NULL,
                           highlight_value = "total",
                           highlight_width = 3,
                           regular_width = 1.5,
                           label_col = NULL,
                           x_breaks = NULL,
                           y_breaks = NULL,
                           y_break_interval = 1000,
                           x_limits = NULL,
                           x_expansion = c(0.03, 0.25),
                           show_grid = TRUE,
                           x_lab = "",
                           y_lab = "",
                           hvjust = NULL) {

  # Load required package
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }

  library(ggplot2)

  # Set label column to color column if not specified
  if (is.null(label_col)) {
    label_col <- color
  }

  # Split data into regular and highlighted groups
  data_regular <- data[data[[color]] != highlight_value, ]
  data_highlight <- data[data[[color]] == highlight_value, ]

  # Get the last x value for labels
  max_x <- max(data[[x]], na.rm = TRUE)
  data_labels <- data[data[[x]] == max_x, ]

  # Create base plot
  p <- ggplot()

  # Add regular lines
  if (nrow(data_regular) > 0) {
    p <- p + geom_line(
      data = data_regular,
      aes(x = .data[[x]], y = .data[[y]], color = .data[[color]]),
      linewidth = regular_width
    )
  }

  # Add highlighted line
  if (nrow(data_highlight) > 0) {
    p <- p + geom_line(
      data = data_highlight,
      aes(x = .data[[x]], y = .data[[y]], color = .data[[color]]),
      linewidth = highlight_width
    )
  }

  # Add color scale if provided
  if (!is.null(color_values)) {
    p <- p + scale_color_manual(values = color_values)
  }

  # Add labels at the end of lines
  if (is.null(hvjust)){
  p <- p + geom_text(
    data = data_labels,
    aes(x = .data[[x]], y = .data[[y]], label = .data[[label_col]]),
    hjust = -0.1 # -0.1 will place slightly to the right
  )
  } else {
    hjust <- hvjust[1]
    vjust <- hvjust[2]
    p <- p + geom_text(
               data = data_labels,
               aes(x = .data[[x]], y = .data[[y]], label = .data[[label_col]]),
               hjust = hjust,
               vjust = vjust
             )
  }


  # Set x-axis scale
  if (is.null(x_limits)) {
    x_limits <- range(data[[x]], na.rm = TRUE)
  }

  if (is.null(x_breaks)) {
    x_breaks <- seq(x_limits[1], x_limits[2], by = 1)
  }

  p <- p + scale_x_continuous(
    limits = x_limits,
    breaks = x_breaks,
    expand = expansion(mult = x_expansion) #Add % space to the right
  )

  # Set y-axis scale
  if (is.null(y_breaks)) {
    y_max <- max(data[[y]], na.rm = TRUE)
    y_breaks <- seq(0, y_max, by = y_break_interval)
  }

  p <- p + scale_y_continuous(breaks = y_breaks)

  # Apply theme
  if (is.null(caption)){
    p <- p + labs(title = title,
         y = y_lab,
         x = x_lab)
  } else {
    p <- p + labs(title = title,
         y = y_lab,
         x = x_lab,
         caption = caption)
  }

  p <- p + theme_classic() +
    guides(color = "none") #no legend

  # Add grid lines if requested
  if (show_grid) {
    p <- p + theme(
      panel.grid.major.y = element_line(
        color = "grey80",
        linewidth = 0.5,
        linetype = "dashed"
      )
    )
  }

  return(p)
}
