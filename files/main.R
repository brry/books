# free Book exchange places
# interactive map from different sources
# Berry Boessenkool, berry-b@gmx.de

# Needed packages -----

if(!requireNamespace("berryFunctions", quietly=TRUE)) install.packages("berryFunctions")
library(berryFunctions) # for 'library2' (like previous line) + usage in scrape scripts

library2(leaflet) # leaflet, addTiles, addCircleMarkers, addMeasure
library2(leaflet.extras) # addControlGPS, gpsOptions, activateGPS, addSearchOSM, addFullscreenControl



# Scrape sources ----

write_books <- function(table, file) write.table(x=table, file=file, sep="\t", 
                                                 quote=FALSE, row.names=FALSE, na="")
read_books <- function(file, ...) read.table(file, sep="\t", header=TRUE, quote="", 
                                        stringsAsFactors=FALSE, comment.char="", ...)

if(FALSE) # be kind, only scrape manually
{
source("files/scrape.R") # 10-50 secs
write_books(table_wiki, "files/table_wiki.txt")
write_books(table_tauschgnom, "files/table_tauschgnom.txt")
write_books(table_osm, "files/table_osm.txt")
write_books(table_lesestunden, "files/table_lesestunden.txt")
write_books(table_boite, "files/table_boite.txt")
write_books(table_openbookcase, "files/table_openbookcase.txt")
}


{
table_wiki        <- read_books("files/table_wiki.txt")
table_tauschgnom  <- read_books("files/table_tauschgnom.txt")
table_osm         <- read_books("files/table_osm.txt")
table_lesestunden <- read_books("files/table_lesestunden.txt")
table_boite       <- read_books("files/table_boite.txt")
table_openbookcase<- read_books("files/table_openbookcase.txt")
}

# colors ----

{
       table_wiki$col <- "blue"
 table_tauschgnom$col <- "red"
        table_osm$col <- "green"
table_lesestunden$col <- "#ff9900"
      table_boite$col <- "purple"
table_openbookcase$col <- "black"

       table_wiki$group <- "Wikipedia"
 table_tauschgnom$group <- "Tauschgnom"
        table_osm$group <- "OSM"
table_lesestunden$group <- "Lesestunden"
      table_boite$group <- "BoiteLire"
table_openbookcase$group <- "Openbookcase"
  
# Merge sources ----

table <- Reduce(function(...) merge(..., all=TRUE), list(
  table_wiki, 
  table_tauschgnom,
  table_osm,
  table_lesestunden,
  table_boite,
  table_openbookcase
  ))



# Map ----
table[table==""] <- NA
sel <- ! colnames(table) %in% c("col","group")
table$lat <- round(table$lat, 6)
table$lon <- round(table$lon, 6)
table$popup <- berryFunctions::popleaf(table, sel=sel, na.rm=TRUE)
html <- paste0('Map by <a href="https://github.com/brry/books">Berry B</a>, ', Sys.Date())

map <- leaflet(table) %>% 
  addTiles(group = "OSM (default)", options=providerTileOptions(maxZoom=19)) %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Esri WorldImagery", options=providerTileOptions(maxZoom=20)) %>%
  #addResetMapButton() %>% 
  addCircleMarkers(~lon, ~lat, popup=~popup, color=~col, group=~group) %>% 
  addControl(position="topright", html='<font size="1">Zoom in before loading layers.</font>') %>% 
  addLayersControl(
    baseGroups=c("OSM (default)", "Esri WorldImagery"),
    overlayGroups=c("Wikipedia", "Tauschgnom", "OSM", "Lesestunden", "BoiteLire", "Openbookcase"),
    options=layersControlOptions(collapsed=FALSE)) %>% 
  hideGroup("Wikipedia") %>% 
  hideGroup("Tauschgnom") %>% 
  hideGroup("OSM") %>% 
  hideGroup("Lesestunden") %>% 
  hideGroup("BoiteLire") %>% 
  hideGroup("Openbookcase") %>% 
  addControl(position="bottomleft", html=html) %>% 
  addSearchOSM(options=searchOptions(autoCollapse=TRUE, minLength=2, hideMarkerOnCollapse=TRUE, zoom=16)) %>% 
  addControlGPS(options=gpsOptions(position="topleft", 
                activate=TRUE, autoCenter=TRUE, maxZoom=16, setView=TRUE)) %>% 
  addMeasure(primaryLengthUnit="kilometers", primaryAreaUnit="hectares",
            activeColor="#3D535D", completedColor="#7D4479", position="topleft") %>% 
  addScaleBar(position="topleft") %>% 
  addFullscreenControl()
map
}


# Export:
{
htmlwidgets::saveWidget(map, "index.html", selfcontained=TRUE)
# HTML head for mobile devices:
# https://stackoverflow.com/questions/42702394/make-leaflet-map-mobile-responsive
map_h <- readLines("index.html")
map_h <- sub('<title>leaflet</title>', x=map_h,
 '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>\n<title>brry.github.io/books</title>')
writeLines(map_h, "index.html") ; rm(map_h)
}  
