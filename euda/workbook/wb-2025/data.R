

dataPath <- "O:\\Prosjekt\\Rusdata"
source("https://raw.githubusercontent.com/folkehelsestats/rusus/refs/heads/main/folder-path.R")

## Data 2012 - 2025
DT <- readRDS(file.path(Rususdata, "Rusus_samlet/rusus_2012_2025.rds"))
