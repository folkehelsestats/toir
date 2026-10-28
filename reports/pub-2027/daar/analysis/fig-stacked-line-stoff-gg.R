library(ggplot2)
library(scales)

# # Use the standard ggplot2 discrete color palette
# stoff_colors <- scales::hue_pal()(length(unique(stoff_long$Stofftype)))
# names(stoff_colors) <- unique(stoff_long$Stofftype)



## Use round to the legend
## -----------------------

library(ggplot2)
library(scales)

stoff_colors <- c("#F23E2E","#C68803", "#0069E8", "#A86CF5", "#55E048", "#366558","#7C145C") #
names(stoff_colors) <- unique(stoff_long$Stofftype)

figStoff <- ggplot(
  stoff_long,
  aes(
    x = year,
    y = antall,
    fill = Stofftype,
    group = Stofftype
  )
) +
  geom_area(
    position = "stack",
    alpha = 1
  ) +

  # Invisible points generate round legend symbols
  geom_point(
    aes(
      x = year,
      y = antall,
      color = Stofftype
    ),
    alpha = 0,
    show.legend = TRUE
  ) +

  scale_fill_manual(
    values = stoff_colors,
    guide = "none"
  ) +

  scale_color_manual(
    values = stoff_colors,
    guide = guide_legend(
      nrow = 2,
      byrow = TRUE,
      override.aes = list(
        alpha = 1,
        shape = 16,
        size = 8 # 3 small, 4 medium, 6 large and 8 extra large dots
      )
    )
  ) +

  scale_x_continuous(
    breaks = sort(unique(stoff_long$year)),
    labels = sort(unique(stoff_long$year)),
    expand = expansion(mult = c(0, 0))
  ) +

  scale_y_continuous(
    limits = c(0, 400),
    breaks = seq(0, 400, by = 50),
    expand = expansion(mult = c(0, 0.02))
  ) +

  labs(
    x = "",
    y = NULL,
    color = "Stofftype"
  ) +

  theme_minimal(base_size = 25) +

  theme(
    panel.grid.major.y = element_line(
      color = "gray",
      linetype = "dashed",
      linewidth = 0.5
    ),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),

    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0
    ),

    axis.text.x = element_text(
      size = 25,
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ),
    axis.text.y = element_text(size = 25),

    legend.text = element_text(size = 25),
    legend.position = "bottom",
    legend.title = element_blank(),

    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 15, 10, 10)
  )

figStoff


# Stacked area chart
figStoff <- ggplot(
  stoff_long,
  aes(
    x = year,
    y = antall,
    fill = Stofftype,
    group = Stofftype
  )
) +
  geom_area(
    position = "stack",
    alpha = 1
  ) +
  scale_x_continuous(
    breaks = sort(unique(stoff_long$year)),
    labels = sort(unique(stoff_long$year)),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    limits = c(0, 400),
    breaks = seq(0, 400, by = 50),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
#     title = "Narkotikautløste dødsfall etter stoffer",
#     subtitle = "Antall dødsfall i perioden, 2006-2025",
    x = "",
    y = NULL,
    fill = "Stofftype"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid.major.y = element_line(
      color = "gray",
      linetype = "dashed", #solid, dotted, dotdash, longdash, twodash
      linewidth = 0.5
    ),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0
    ),
    axis.text.x = element_text(
      size = 20,       # Label font size
      angle = 90,      # Vertical labels
      hjust = 1,       # Horizontal alignment
      vjust = 0.5      # Vertical alignment
    ),
    axis.text.y = element_text(
      size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 15, 10, 10)
  ) +
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE)
  )

# Display the graph
figStoff

## Colors
library(highdir)

hdir_colors <- highdir::get_palette("hdir")
hdir_colors

colors <- c("#F23E2E", "#0069E8", "#7C145C", "#C68803", "#047FA4", "#55E048", "#A86CF5", "#366558")



figStoff <- figStoff +
  scale_fill_manual(values = colors)

figStoff

# ## To save
# ## -------
ggsave(
  filename = "dodsfall_stoff.png",
  plot = figStoff,
  width = 20,
  height = 10,
  units = "in",
  dpi = 750
)

# ggsave(
#   filename = "Narkotikautloeste_dodsfall.pdf",
#   plot = figStoff,
#   width = 14,
#   height = 6,
#   units = "in"
# )
