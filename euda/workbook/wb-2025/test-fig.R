library(data.table)
library(ggplot2)
library(ggrepel)

# Example data
dt <- data.table(
  year = 2012:2024,
  Lifetime = c(19.2,23.3,21.9,20.9,20.6,24.5,23.6,23.2,25,25.5,27.1,28,29.9),
  `Past year` = c(3.4,5.1,4.2,4.2,3.9,5.3,5.4,5.1,4.3,4.4,5.5,5.4,6.8),
  `Past month` = c(1.5,1.7,1.6,1.7,1.7,2.5,2.1,2.3,1.6,1.8,2.4,2.1,2.9)
)

# Convert to long format
long_dt <- data.table::melt(
  dt,
  id.vars = "year",
  variable.name = "measure",
  value.name = "value"
)

# Last observations used for direct labels
last_dt <- long_dt[year == max(year)]

ggplot(long_dt,
       aes(year, value,
           color = measure,
           group = measure)) +

  geom_line(linewidth = 1.3) +

  geom_point(size = 2.5) +

#   # Only label last point
#   geom_text_repel(
#     data = last_dt,
#     aes(label = paste0(measure, " (", value, "%)")),
#     hjust = 0,
#     direction = "y",
#     nudge_x = 0.5,
#     show.legend = FALSE
#   ) +

#   geom_label(
#     data = last_dt,
#     aes(label = sprintf("%.1f%%", value)),
#     fill = "white",
#     label.size = 0,
#     fontface = "bold"
#   ) +
  
geom_text(
  aes(label = value),
  size = 3.2,
  fontface = "bold",
  vjust = -0.7,
  show.legend = FALSE
) +

#   geom_text(
#     aes(label = value),
#     size = 3,
#     alpha = 0.8,
#     check_overlap = TRUE
#   ) +

scale_x_continuous(
  breaks = 2012:2024,
    expand = expansion(mult = c(0.01, 0.15))
  ) +

  scale_colour_manual(
    values = c(
      "Lifetime" = "#1F77B4",
      "Past year" = "#FF7F0E",
      "Past month" = "#7F7F7F"
    )
  ) +

  labs(
    title = "Cannabis use among adults (16–64), 2012–2024",
    subtitle = "Percentage reporting use",
    x = NULL,
    y = "%"
  ) +

  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  )


## Option 2
## ---------
# geom_text(
#   aes(label = value),
#   size = 3.2,
#   fontface = "bold",
#   vjust = -0.7,
#   show.legend = FALSE
# )

geom_text(
  aes(label = value),
  size = 3,
  alpha = 0.8,
  check_overlap = TRUE
)


geom_label(
  data = last_dt,
  aes(label = sprintf("%.1f%%", value)),
  fill = "white",
  label.size = 0,
  fontface = "bold"
)
