## Tall fra DÅR
## ------------

pacpac <- function(p) {
  invisible(lapply(p, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }))
}

pkgs <- c("rio", "data.table")
pacpac(pkgs)

fil <- "Narkotikadødsfall per fylke 2019-2024.xlsx"
fpath <- "o:/Prosjekt/Rusdata/PWID/DÅR"

dx <- rio::import(file.path(fpath, fil))
setDT(dx)
oldN <- names(dx)[-c(1,2)]
newN <- paste0("dar", oldN)
setnames(dx, oldN, newN)

dx[, .N, keyby = Stofftype]
dx[Stofftype == "Avhengighet", sum(dar2024)]
dx[Stofftype == "Heroin", sum(dar2024)]
dx[Stofftype!= "Narkotikautløste dødsfall totalt", sum(dar2024), by = Bostedsfylke]
