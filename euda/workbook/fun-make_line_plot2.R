make_line_plot2 <- function(data,
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
                           y_limits = NULL,
                           x_expansion = c(0.03, 0.25),
                           show_grid = TRUE,
                           x_lab = "",
                           y_lab = "",
                           hvjust = NULL,
                           legend = TRUE,
                           legend_title = NULL) {
  # Check required package
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }

  # Set label column to color column if not specified
  if (is.null(label_col)) {
    label_col <- color
  }

  # Split data into regular and highlighted groups
  data_regular <- data[data[[color]] != highlight_value, , drop = FALSE]
  data_highlight <- data[data[[color]] == highlight_value, , drop = FALSE]

  # Get the rows at the largest x value for end-of-line labels
  max_x <- max(data[[x]], na.rm = TRUE)
  data_labels <- data[data[[x]] == max_x, , drop = FALSE]

  # Create base plot
  p <- ggplot2::ggplot()

  # Add regular lines
  if (nrow(data_regular) > 0) {
    p <- p + ggplot2::geom_line(
      data = data_regular,
      mapping = ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]],
        color = .data[[color]],
        group = .data[[color]]
      ),
      linewidth = regular_width
    )
  }

  # Add highlighted line
  if (nrow(data_highlight) > 0) {
    p <- p + ggplot2::geom_line(
      data = data_highlight,
      mapping = ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]],
        color = .data[[color]],
        group = .data[[color]]
      ),
      linewidth = highlight_width
    )
  }

  # Apply manual colors if supplied
  if (!is.null(color_values)) {
    p <- p + ggplot2::scale_color_manual(values = color_values)
  }

  # Use a bottom legend, or label each line at its final x value
  if (isTRUE(legend)) {
    p <- p +
      ggplot2::guides(color = ggplot2::guide_legend()) +
      ggplot2::theme(legend.position = "bottom",
                     legend.text = element_text(size = 13))
  } else {
    if (is.null(hvjust)) {
      p <- p + ggplot2::geom_text(
        data = data_labels,
        mapping = ggplot2::aes(
          x = .data[[x]],
          y = .data[[y]],
          label = .data[[label_col]]
        ),
        hjust = -0.1
      )
    } else {
      p <- p + ggplot2::geom_text(
        data = data_labels,
        mapping = ggplot2::aes(
          x = .data[[x]],
          y = .data[[y]],
          label = .data[[label_col]]
        ),
        hjust = hvjust[1],
        vjust = hvjust[2]
      )
    }

    p <- p +
      ggplot2::guides(color = "none") +
      ggplot2::theme(legend.position = "none")
  }

  # Set x-axis scale
  if (is.null(x_limits)) {
    x_limits <- range(data[[x]], na.rm = TRUE)
  }
  if (is.null(x_breaks)) {
    x_breaks <- seq(x_limits[1], x_limits[2], by = 1)
  }

  # Use less right-side space when legend is enabled
  # that space is only needed for text label
  if (legend) {
    current_expansion <- c(x_expansion[1], 0)
  } else {
    current_expansion <- x_expansion
  }

  p <- p + scale_x_continuous(
    limits = x_limits,
    breaks = x_breaks,
    expand = expansion(mult = current_expansion)
  )

  # Set y-axis scale
  if (is.null(y_limits)) {
    y_limits <- c(0, max(data[[y]], na.rm = TRUE))
  }
  if (is.null(y_breaks)) {
    y_breaks <- seq(y_limits[1], y_limits[2], by = y_break_interval)
  }

  p <- p + ggplot2::scale_y_continuous(
    breaks = y_breaks,
    limits = y_limits
  )

  # Apply labels
  if (is.null(caption)) {
    p <- p + labs(
      title = title,
      y = y_lab,
      x = x_lab,
      color = if (is.null(legend_title)) color else legend_title
    )
  } else {
    p <- p + labs(
      title = title,
      y = y_lab,
      x = x_lab,
      caption = caption,
      color = if (is.null(legend_title)) color else legend_title
    )
  }
  # Apply base theme
  p <- p + ggplot2::theme_classic()

  # Add horizontal grid lines if requested
  if (show_grid) {
    p <- p + ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(
        color = "grey80",
        linewidth = 0.5,
        linetype = "dashed"
      )
    )
  }

  return(p)
}
