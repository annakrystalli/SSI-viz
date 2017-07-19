#' ## Leaflet interactive visualisation of UK RSEs

#' ### pkgs
if (!require("pacman")) install.packages("pacman")
pkgs <- c("leaflet", "tidyverse","sp", "spatstat", "wesanderson")
pacman::p_unload(pacman::p_loaded(), character.only = T)
pacman::p_load(pkgs, character.only = T)
#' ### custom functions
#' **`cap_first`** <https://gist.github.com/annakrystalli/70820dbfd0457601ddd0203dcc3a8476>
cap_first <- function(X){
    simple_cap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        out <- paste(toupper(substring(s, 1,1)), substring(s, 2),
                     sep="", collapse=" ")
        out <- gsub("And", "and", out)
        out <- gsub("&", "and", out)
        out <- gsub("Of", "of", out)
        out
    }
    sapply(X, simple_cap, USE.NAMES = F) 
}
#' **`get_radius`** used to get radius to correctly represent relative area of circles, consistent with the concept of 
#' [proportional ink](https://makingmaps.net/2007/08/28/perceptual-scaling-of-map-symbols/) as discussed in
#' [Calling Bull*** tutorial](http://callingbullshit.org/tools/tools_proportional_ink.html?utm_content=buffer8d9fb&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer)
get_radius <- function(area){
    sqrt(area/pi)
}

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

#' ***
#' ## INTERACTIVE LEAFLET PLOTS
#' ### Basic
m <- leaflet(data = spdf) %>% addProviderTiles(providers$OpenStreetMap, group = "OSM") %>% addMarkers()
m    
  
#' ### RSE count ~ size of circle
#' use parameter `size` to multiply proportional areas by a constant.
size <- 4
#' RSE count indicated by size of circle, single colour. Hover over institutional info
leaflet(data = spdf) %>% addProviderTiles(providers$OpenStreetMap, group = "OSM") %>% 
    addCircleMarkers(radius = ~get_radius(Count)*size, fillOpacity = 0.5,
                     label = ~paste0(cap_first(Name), " (", Count, ")"),
                     color = wes_palette("GrandBudapest")[2])

#' ## RSE count ~ size of circle + colour scale
#' RSE count indicated by size of circle as well as colour scale. Hover over institutional info. Colour palette from `wesanderson` package. 
#' use parameter `size` to multiply proportional areas by a constant.
size <- 4
# create palette
pal <- colorNumeric(
    palette = as.character(wes_palette("Darjeeling")[c(2,3,4,1)]),
    domain = spdf$Count)

leaflet(data = spdf) %>% addProviderTiles(providers$OpenStreetMap, group = "OSM") %>% 
    addCircleMarkers(radius = ~get_radius(Count)*size, fillOpacity = 0.7, 
                     color = ~pal(Count),
                     stroke = FALSE,
                     label = ~paste0(cap_first(Name), " (", Count, ")")) %>%
    addLegend("bottomright", pal = pal, values = ~Count,
              title = "RSE counts",
              opacity = 1)

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
leaflet(data = spdf) %>% addProviderTiles(providers$OpenStreetMap, group = "OSM") %>% addMarkers(icon = leafIcons)


