
make_gg <- function(long_dt, label_dt, grid_dt, title, subtitle, cols){
  ggplot(
    long_dt,
    aes(
      x = year,
      y = value,
      colour = measure,
      group = measure
    )
  ) +
    # Draw horizontal gridlines only within the plotting area.
    # They stop at the final year and do not extend under labels.
    geom_segment(
      data = grid_dt,
      aes(
        x = x_start,
        xend = x_end,
        y = y,
        yend = y
      ),
      inherit.aes = FALSE,
      colour = "#D9D9D9",
      linewidth = 0.4,
      linetype = "dashed" #solid, dotted, dotdash, longdash, twodash
    ) +
    # Lines
    geom_line(linewidth = 1) +

  # Circular points
  geom_point(
    aes(colour = measure),
    fill = "white",
    shape = 21,
    size = 7, # circle size
    stroke = 0.3 #border tickness
  ) +

  # Values centered inside the circles
  geom_text(
    aes(label = value),
    colour = "black",
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +

  scale_colour_manual(values = cols) +
  scale_x_continuous(
    breaks = seq(
      min(long_dt$year, na.rm = TRUE),
      max(long_dt$year, na.rm = TRUE),
      by = 1
    ),
    expand = expansion(mult = c(0.02, 0.15)) # add extra space around the plotted area
  ) +
  labs(
    title = title,
    subtitle = subtitle,
    x = NULL,
    y = "Percentage"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(
          t = 12, r = 120, b = 12, l = 12
        )) +
  geom_text(
    data = label_dt,
    aes(x = x_lab, label = measure),
    hjust = 0,
    size = 5,
    colour = "#222222"
  ) +
  # Preserve the original scale and allow labels outside
  # the panel without clipping them.
  coord_cartesian(clip = "off")
}
