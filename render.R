if (!require("pacman")) install.packages("pacman")
pkgs <- c("rmarkdown", "knitr", "webshot")
pacman::p_unload(pacman::p_loaded(), character.only = T)
pacman::p_load(pkgs, character.only = T)
#webshot::install_phantomjs()
#knitr::spin("leaflet_map.R", format = c("Rmd"))
rmarkdown::render("leaflet_map.R") 

