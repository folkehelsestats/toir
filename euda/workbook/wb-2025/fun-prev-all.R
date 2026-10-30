
create_prev_data <- function(dt = dt,
                             denom,
                             outcome,
                             pop = "All Adult",
                             grp = "LTP"){

  d64kj <- torr::calc_prevalence(dt, denominator = denom,
                                 year_var = "year",
                                 outcome_var = outcome,
                                 weight_var = "vekt2",
                                 by = "kjonn",
                                 rolling_year = 3)

  d64t <- torr::calc_prevalence(dt, denominator = denom,
                                year_var = "year",
                                outcome_var = outcome,
                                weight_var = "vekt2",
                                rolling_year = 3)

  d64t[, kjonn := 3]

  ddk64 <- data.table::rbindlist(list(d64kj, d64t), use.names = TRUE)
  ddk64[, pop := pop]
  ddk64[, grp := grp]

  old <- c("rolling_period", outcome)
  new <- c("years", "pros")
  data.table::setnames(ddk64, old, new)

  ddk64[, gender := factor(kjonn, levels = c(2,1,3),
                           labels = c("Women", "Men", "Total"))]

  return(ddk64[])
}




create_fig_ci <- function(cannabis){
  
# Ensure plotting order
cannabis[, Prevalence := factor(
  Prevalence,
  levels = c("LTP","LYP","LMP")
)]

cannabis[, Group := factor(
  Group,
  levels = c("Women","Men","Total")
)]

# Position object used by bars and CI lines
pd <- position_dodge(width = 0.8)

# Position value text
offset <- 0.03 * max(cannabis$UpperCI) 

ggplot(
  cannabis,
  aes(
    x = Prevalence,
    y = Estimate,
    fill = Group
  )
) +

  # Bars
  geom_col(
    position = pd,
    width = 0.7,
    color = "white"
  ) +

  # Confidence intervals
  geom_errorbar(
    aes(
      ymin = LowerCI,
      ymax = UpperCI
    ),
    position = pd,
    width = 0.20,
    linewidth = 0.6
  ) +

  # Percentage labels
  geom_text(
    aes(
      y = UpperCI + offset,
      label = sprintf("%.1f", Estimate)
    ),
    position = pd,
    size = 3.5
  ) +

  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1))
  ) +

  # Facets
  facet_wrap(
    ~Population,
    nrow = 1
  ) +

  # Colours
  scale_fill_manual(
    values = c(
      "Women" = "#0B5D6E",
      "Men" = "#1976D2",
      "Total" = "#8E1B63"
    )
  ) +

  # Labels
  labs(
    title = "Cumulative percentage with CI of cannabis use (2023–2025)",
    x = "Prevalence Type",
    y = "Percentage (%)",
    fill = NULL
  ) +

  # Give some room for labels above bars
  expand_limits(y = max(cannabis$UpperCI) * 1.1) +

  # Theme
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )


}
