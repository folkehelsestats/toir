# Source: Norwegian Institute of Public Health (NIPH)
# Use of cannabis (ESPAD)
canUse <- data.frame(
  Year = c(1995, 1999, 2003, 2007, 2011, 2015, 2019, 2024),
  Noen_gang = c(6.0, 12.5, 8.9, 6.5, 5.5, 6.6, 8.4, 10.3),
  Siste_12_mnd = c(4.4, 9.1, 5.9, 4.7, 4.6, 5.0, 6.9, 8.0),
  Siste_30_dager = c(2.4, 4.1, 2.6, 2.7, 1.8, 1.9, 3.4, 3.8)
)


library(ggplot2)
library(data.table)
#Bruk av ulike cannabisprodukter (prosent) blant 15-16-åringer som oppga å ha brukt
#cannabis siste 12 måneder, 2019 og 2024.
# Cannabis use among 15-16 yrs last 12 months (ESPAD)
canType <- data.table(
  canprod = c(
    "Resin",
    "Marihuana",
    "Cannabis oil",
    "Cannabis edible",
    "Cannabis-infused beverage"
  ),
  y2019 = c(92, 47, 17, 30, 18),
  y2024 = c(88, 61, 29, 33, 20)
)

canType[, can := c("hasj", "marihuana", "canoil", "canspis", "candrikk")]
canTYP <- melt(canType, id.vars = c("canprod", "can"),
               value.name = "pro",
               measure.vars = c("y2019", "y2024"),
               variable.name = "year")
canTYP[, year := as.factor(sub("y", "", year))]

chc4 <- c("#5F9EA0", "#E1B378", "#BB7BDA", "#7C145C")

ggplot(canTYP, aes(x = canprod, y = pro, fill = year)) +
  geom_col(position = position_dodge2(width = 0.9)) +
  geom_text(aes(label = pro),
            position = position_dodge2(width = 0.9),
            hjust = -0.3, size = 3.5) +
  scale_fill_manual(values = chc4) +
  theme_classic() +
  labs(title = "Cannabis use among 15-16 yrs last 12 months", y = "Percent", x = "",
       fill = "Year", caption = "Source: Norwegian Institute of Public Health (NIPH)") +
  coord_flip() +
  theme(legend.position = c(0.9, 0.3),
        legend.justification = c(1, 0))
