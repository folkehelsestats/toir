
#' Create a customized ggplot2 bar chart with optional faceting and custom labels
#'
#' This function generates a bar chart using \pkg{ggplot2} with customizable
#' aesthetics for x, y, and fill variables. It supports optional faceting via
#' \code{facet_wrap()} or \code{facet_grid()} and allows custom axis labels and title.
#'
#' @param data A data frame containing the variables to plot.
#' @param x Character string specifying the variable for the x-axis.
#' @param y Character string specifying the variable for the y-axis.
#' @param fill Character string specifying the variable for fill color.
#' @param hdir_color A character vector of colors for \code{scale_fill_manual()}.
#' @param wrap Optional character string specifying the variable for \code{facet_wrap()}.
#' @param grid Optional character vector of length 2 specifying row and column variables for \code{facet_grid()}.
#' @param title Plot title (default: "Cumulative percentage of cannabis use across age groups (2022-2024)").
#' @param xlab Label for x-axis (default: "Prevalence Type").
#' @param ylab Label for y-axis (default: "Percentage (%)").
#' @param lglab Label for the legend (default: "").
#' @param show_legend Logical indicating whether to display the legend (default: TRUE).
#' @param ylim_max Numeric value specifying the maximum limit for the y-axis (default: 42).
#'
#' @return A \code{ggplot} object.
#' @examples
#' hdir_color <- c("#025169", "#0069E8", "#7C145C", "#047FA4",
#'                 "#C68803", "#38A389", "#6996CE", "#366558",
#'                 "#BF78DE", "#767676")
#'
#' # Example with facet_wrap:
#' # create_plot(tblCanAge, x = "type", y = "value", fill = "agecat",
#' #             hdir_color = hdir_color, wrap = "grp")
#'
#' # Example with facet_grid:
#' # create_plot(tblCanAge, x = "type", y = "value", fill = "agecat",
#' #             hdir_color = hdir_color, grid = c("grp", "sex"))
#'
#' @import ggplot2
#' @export

create_plot <- function(data, x, y, fill, hdir_color,
                        wrap = NULL, grid = NULL,
                        title = "Title is here",
                        xlab = "X-label", ylab = "Y-label",
                        lglab = "Legend name",
                        show_legend = TRUE,
                        ylim_max = 42) {

  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y, fill = fill)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::geom_text(ggplot2::aes_string(label = y),
                       position = ggplot2::position_dodge(width = 0.8),
                       vjust = -0.5, size = 3.5) +
    ggplot2::scale_fill_manual(values = hdir_color) +
    ggplot2::labs(x = xlab, y = ylab, fill = lglab, title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      strip.background = ggplot2::element_rect(fill = c("#E8F4F8", "#FFF4E6"),
                                               color = "grey80", linewidth = 0.5),
      panel.background = ggplot2::element_rect(fill = c("#E8F4F8", "#FFF4E6"),
                                               color = NA),
      legend.position = if (show_legend) "bottom" else "none"
    ) +
    ggplot2::ylim(0, ylim_max)

  # Add faceting
  if (!is.null(wrap)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", wrap)))
  } else if (!is.null(grid) && length(grid) == 2) {
    p <- p + ggplot2::facet_grid(stats::as.formula(paste(grid[1], "~", grid[2])))
  }

  return(p)
}



## create_plot <- function(data, x, y, fill, hdir_color,
##                         wrap = NULL, grid = NULL,
##                         title = "Title is here",
##                         xlab = "X-label", ylab = "Y-label",
##                         lglab = "Legend name",
##                         show_legend = TRUE,
##                         ylim_max = 42) {

##   p <- ggplot2::ggplot(data, ggplot2::aes(x = {{ x }}, y = {{ y }}, fill = {{ fill }})) +
##     ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
##     ggplot2::geom_text(ggplot2::aes(label = {{ y }}),
##                        position = ggplot2::position_dodge(width = 0.8),
##                        vjust = -0.5, size = 3.5) +
##     ggplot2::scale_fill_manual(values = hdir_color) +
##     ggplot2::labs(x = xlab, y = ylab, fill = lglab, title = title) +
##     ggplot2::theme_minimal() +
##     ggplot2::theme(
##       panel.grid.major.x = ggplot2::element_blank(),
##       strip.text = ggplot2::element_text(face = "bold", size = 11),
##       strip.background = ggplot2::element_rect(fill = c("#E8F4F8", "#FFF4E6"),
##                                                color = "grey80", linewidth = 0.5),
##       panel.background = ggplot2::element_rect(fill = c("#E8F4F8", "#FFF4E6"),
##                                                color = NA),
##       legend.position = if (show_legend) "bottom" else "none"
##     ) +
##     ggplot2::coord_cartesian(ylim = c(0, ylim_max))  # Changed from ylim()

##   # Add faceting using tidy evaluation
##   if (!is.null(wrap)) {
##     p <- p + ggplot2::facet_wrap(ggplot2::vars({{ wrap }}))
##   } else if (!is.null(grid) && length(grid) == 2) {
##     p <- p + ggplot2::facet_grid(ggplot2::vars({{ grid[[1]] }}) ~ ggplot2::vars({{ grid[[2]] }}))
##   }

##   return(p)
## }
