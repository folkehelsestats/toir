install.packages(c("ggplot2", "dplyr", "tidyr", "ggrepel"))



# ---------------------------------------------------------
# 1. Load packages
# ---------------------------------------------------------

library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)
library(ggrepel)

# Expected input:
# df has one row per year, with columns:
# year, Andre_og_uspesifiserte_stoffer, Andre_opioider,
# Avhengighet, Heroin, Metadon,
# Sentralstimulerende_midler, Syntetiske_opioider
#
# Replace these example column names with your actual
# column names if they differ.


# ---------------------------------------------------------
# 2. Define readable display names and colours
# ---------------------------------------------------------

# Named colours ensure that each category always receives
# the same colour, regardless of its order in the data.

palette <- c(
  "Syntetiske opioider" =
    "#356859",
  "Sentralstimulerende midler" =
    "#36B84A",
  "Metadon" =
    "#A16AE8",
  "Heroin" =
    "#D69F00",
  "Avhengighet" =
    "#E64B35",
  "Andre opioider" =
    "#2878B5",
  "Andre og uspesifiserte stoffer" =
    "#8E397D"
)


# Custom colour-blind-conscious palette inspired by Okabe-Ito.
# The darker amber is easier to see on a white background
# than the original very bright yellow.

palette <- c(
  "Syntetiske opioider" =
    "#007A5E",
  "Sentralstimulerende midler" =
    "#56B4A9",
  "Metadon" =
    "#AA78C2",
  "Heroin" =
    "#D99A00",
  "Avhengighet" =
    "#D55E00",
  "Andre opioider" =
    "#0072B2",
  "Andre og uspesifiserte stoffer" =
    "#CC79A7"
)

# =============================================================================
# Okabe-Ito palette
# https://jfly.uni-koeln.de/color/
# =============================================================================

palette <- c(
  "Syntetiske opioider"             = "#009E73",
  "Sentralstimulerende midler"      = "#56B4E9",
  "Metadon"                         = "#CC79A7",
  "Heroin"                          = "#E69F00",
  "Avhengighet"                     = "#D55E00",
  "Andre opioider"                  = "#0072B2",
  "Andre og uspesifiserte stoffer"  = "#000000"
)


# Set the stacking order from the bottom of the chart
# to the top. The same order is used for direct labels.

category_order <- names(palette)


# ---------------------------------------------------------
# 3. Prepare dataset 
# ---------------------------------------------------------
library(data.table)

# Convert to data.table by reference (no full copy).
setDT(stoff_long)

# Define the stacking order from bottom to top.
category_order <- c(
  "Syntetiske opioider",
  "Sentralstimulerende midler",
  "Metadon",
  "Heroin",
  "Avhengighet",
  "Andre opioider",
  "Andre og uspesifiserte stoffer"
)

# Set the factor levels to control stacking and label order.
stoff_long[, Stofftype := factor(
  Stofftype,
  levels = category_order
)]

# Rename columns for consistency with the earlier ggplot code.
# This is optional; alternatively, keep Stofftype and antall.
setnames(
  stoff_long,
  old = c("Stofftype", "antall"),
  new = c("category", "value")
)

# Validate the data.
stopifnot(
  is.numeric(stoff_long$value),
  all(stoff_long$value >= 0, na.rm = TRUE)
)

# ---------------------------------------------------------
# 4. Calculate label positions for the last year
# ---------------------------------------------------------

# Find the latest year in the dataset.
latest_year <- max(stoff_long$year, na.rm = TRUE)

# Select observations for the final year and sort them
# according to the stacking order.
label_data <- stoff_long[year == latest_year]
setorder(label_data, category)

# Calculate the centre of each area:
# cumulative = upper boundary of the stacked area
# label_y = centre of the area
label_data[, cumulative := cumsum(value)]

label_data[, label_y := cumulative - value / 2]

# Keep only categories with positive values.
label_data <- label_data[
  !is.na(value) & value > 0
]

# Convert factor labels to character for geom_text_repel().
label_data[, category := as.character(category)]

# ---------------------------------------------------------
# 4b. Create custom horizontal gridlines
# ---------------------------------------------------------

grid_df <- data.frame(
  y = seq(0, 400, by = 50),
  x_start = min(stoff_long$year),
  x_end = latest_year
)

# ---------------------------------------------------------
# 5. Draw the stacked area chart
# ---------------------------------------------------------

p <- ggplot(
  stoff_long,
  aes(
    x = year,
    y = value,
    fill = category,
    group = category
  )
) +
  # Draw horizontal gridlines only within the plotting area.
  # They stop at the final year and do not extend under labels.
  geom_segment(
    data = grid_df,
    aes(
      x = x_start,
      xend = x_end,
      y = y,
      yend = y
    ),
    inherit.aes = FALSE,
    colour = "#D9D9D9",
    linewidth = 0.4
  ) +
  geom_area(
    position = position_stack(reverse = TRUE)
    #     colour = "white",
#     linewidth = 0.3
  ) +
  # Direct labels replace the conventional legend.
  # Leader lines connect labels to their respective areas.
  geom_text_repel(
    data = label_data,
    aes(
      x = year,
      y = label_y,
      label = category
    ),
    inherit.aes = FALSE,
    force = 2,
    nudge_x = 1.3,
    direction = "y",
    hjust = 0,
    size = 5.5, #text size
    colour = "#222222",
    segment.colour = "#555555",
    segment.size = 0.35,
    box.padding = 0.35,
    point.padding = 0.2,
    max.overlaps = Inf,
    min.segment.length = 0,
    seed = 123
  ) +
  # Apply the palette to the stacked areas.
  # drop = FALSE keeps the colour mapping stable even if
  # a category is absent in a particular subset of the data.
  scale_fill_manual(
    values = palette,
    breaks = category_order,
    drop = FALSE
  ) +
  # Leave room on the right for the direct labels.
  scale_x_continuous(
    breaks = seq(
      min(stoff_long$year),
      max(stoff_long$year),
      by = 1
    ),
    expand = expansion(mult = c(0.005, 0.35))
  ) +
  scale_y_continuous(
    limits = c(0, 400),
    breaks = seq(0, 400, by = 50),
    expand = expansion(mult = c(0, 0.02))
  ) +
  # Preserve the original scale and allow labels outside
  # the panel without clipping them.
  coord_cartesian(clip = "off") +
  # Clear axis labels and a restrained grid.
  labs(
    title = "",
    x = NULL,
    y = "Antall narkotikautløste dødsfall",
    fill = NULL,
    caption = paste(
      "Datakilde:",
      "Dødsårsaksregisteret, Folkehelseinstituttet"
    )
  ) +
  # Remove the legend because each area is labelled.
  guides(fill = "none") +
  # A white background and light horizontal gridlines
  # improve readability in reports and print.
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      colour = "#222222"
    ),
    axis.title.y = element_text(
      colour = "#333333",
      margin = margin(r = 10)
    ),
    axis.text = element_text(
      colour = "#444444",
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(
      t = 12, r = 120, b = 12, l = 12
    ),
    plot.caption = element_text(
      size = 12,
      colour = "#333333",
      hjust = 1 #right align = 1 and left align = 0
    )
  )

# Display the chart.
print(p)


# ---------------------------------------------------------
# 6. Export a high-resolution figure
# ---------------------------------------------------------

# A wider output gives the direct labels enough room.
# Use a larger width than the original plot to prevent
# labels from being crowded or cut off.

ggsave(
  filename = "dodsfall_stoff.png",
  plot = p,
  width = 14,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)
