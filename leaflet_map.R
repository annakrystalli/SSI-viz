#' ## Leaflet interactive visualisation of UK RSEs

#' ### pkgs
if (!require("pacman")) install.packages("pacman")
pkgs <- c("leaflet", "tidyverse","sp", "spatstat")
pacman::p_unload(pacman::p_loaded(), character.only = T)
pacman::p_load(pkgs, character.only = T)

#' ### data
pc_coords <- read_csv("data/ukpostcodes.csv") # source: https://www.freemaptools.com/download-uk-postcode-lat-lng.htm
rse_pc <- read_csv("data/RSE_email_postcode.csv") %>% 
    set_tidy_names(syntactic = T) %>% 
    rename(postcode = Unique.postcodes) 

#' ## data process
#' ammend NA postcodes    
rse_pc[rse_pc$postcode == "EH9 3JZ", "postcode"] <- "EH8 9AB"
rse_pc[rse_pc$postcode == "BS16 1QU", "postcode"] <- "BS2 0JA"

#' join lat/lon from postcodes
rse_pc <- rse_pc %>% left_join(pc_coords, by = "postcode")
#' convert to spdf
spdf <- SpatialPointsDataFrame(coords = rse_pc[,c("longitude", "latitude")], data = rse_pc,
                       proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#' ## INTERACTIVE LEAFLET PLOT
m <- leaflet(data = spdf) %>% addTiles() %>% addMarkers()
m    
  

#' #### Special ICON
#' Make a custom icon. See more [here](http://rstudio.github.io/leaflet/markers.html)
leafIcons <- icons(
    iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
    iconWidth = 38, iconHeight = 95,
    iconAnchorX = 22, iconAnchorY = 94,
    shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
    shadowWidth = 50, shadowHeight = 64,
    shadowAnchorX = 4, shadowAnchorY = 62
)
#' INTERACTIVE LEAFLET PLOT
leaflet(data = spdf) %>% addTiles() %>% addMarkers(icon = leafIcons)


