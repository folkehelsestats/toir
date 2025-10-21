# Source: Norwegian Institute of Public Health (NIPH)
# Use of cannabis
canUse <- data.frame(
  Year = c(1995, 1999, 2003, 2007, 2011, 2015, 2019, 2024),
  Noen_gang = c(6.0, 12.5, 8.9, 6.5, 5.5, 6.6, 8.4, 10.3),
  Siste_12_mnd = c(4.4, 9.1, 5.9, 4.7, 4.6, 5.0, 6.9, 8.0),
  Siste_30_dager = c(2.4, 4.1, 2.6, 2.7, 1.8, 1.9, 3.4, 3.8)
)

# Cannabis use among 15-16 yrs last 12 months
canType <- data.frame(
  Cannabisprodukter = c(
    "Hasj",
    "Marihuana",
    "Cannabisolje- eller ekstrakter",
    "Cannabisholdige produkter som kan spises",
    "Cannabisholdige produkter som kan drikkes"
  ),
  `2019` = c(92, 47, 17, 30, 18),
  `2024` = c(88, 61, 29, 33, 20)
)
