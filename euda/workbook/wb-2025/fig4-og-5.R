


make_gg <- function(long_dt, label_dt){
  ggplot(
    long_dt,
    aes(
      x = year,
      y = value,
      colour = measure,
      group = measure
    )
  ) +
    # Lines
    geom_line(linewidth = 1) +

  # Circular points
  geom_point(
    aes(colour = measure),
    fill = "white",
    shape = 21,
    size = 10, # circle size
    stroke = 1 #border tickness
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
    breaks = 2012:2024,
    expand = expansion(mult = c(0.02, 0.15)) # add extra space around the plotted area
  ) +

  labs(
    title = "Cannabis use among adults (16–64), 2012–2024",
    subtitle = "Percentage reporting use",
    x = NULL,
    y = "%"
  ) +

  theme_minimal(base_size = 12) +
  theme(legend.position = "none") +
  geom_text(
    data = label_dt,
    aes(x = x_lab, label = measure),
    hjust = 0,
    size = 5,
    colour = "#222222"
  ) +
  coord_cartesian(clip = "off")
}
