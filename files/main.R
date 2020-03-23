# free Book exchange places
# interactive map from different sources
# Berry Boessenkool, berry-b@gmx.de

# Needed packages -----

if(!requireNamespace("berryFunctions", quietly=TRUE)) install.packages("berryFunctions")
library(berryFunctions) # for 'library2' (like previous line) + usage in scrape scripts

library2(leaflet) # leaflet, addTiles, addCircleMarkers, addMeasure
library2(leaflet.extras) # addControlGPS, gpsOptions, activateGPS, addSearchOSM



# Scrape sources ----

source("files/scrape_wiki.R") # 10-30 secs
write.table(table_wiki, "files/table_wiki.txt", sep="\t", quote=FALSE, row.names=FALSE, na="")
table_wiki <- read.table("files/table_wiki.txt", sep="\t", header=TRUE, quote="", stringsAsFactors=FALSE)

source("files/scrape_tauschgnom.R")
table_tauschgnom <- table_tauschgnom[1:5, ] # permission request pending
write.table(table_tauschgnom, "files/table_tauschgnom.txt", sep="\t", quote=FALSE, row.names=FALSE, na="")
table_tauschgnom <- read.table("files/table_tauschgnom.txt", sep="\t", header=TRUE, quote="", stringsAsFactors=FALSE)

source("files/scrape_osm.R")
write.table(table_osm, "files/table_osm.txt", sep="\t", quote=FALSE, row.names=FALSE, na="")
table_osm <- read.table("files/table_osm.txt", sep="\t", header=TRUE, quote="", stringsAsFactors=FALSE)

source("files/scrape_lesestunden.R")
write.table(table_lesestunden, "files/table_lesestunden.txt", sep="\t", quote=FALSE, row.names=FALSE, na="")
table_lesestunden <- read.table("files/table_lesestunden.txt", sep="\t", header=TRUE, quote="", stringsAsFactors=FALSE)




# colors ----

table_wiki$col <- "blue"
table_tauschgnom$col <- "red"
table_osm$col <- "green"
table_lesestunden$col <- "yellow"

table_wiki$group <- "Wikipedia"
table_tauschgnom$group <- "Tauschgnom"
table_osm$group <- "OSM"
table_lesestunden$group <- "Lesestunden"
  
# Merge sources ----

table <- Reduce(function(...) merge(..., all=TRUE), list(
  table_wiki, 
  table_tauschgnom,
  table_osm,
  table_lesestunden
  ))

write.table(table, "files/table.txt", sep="\t", quote=FALSE, row.names=FALSE, na="")
table <- read.table("files/table.txt", sep="\t", header=TRUE, fill=TRUE, quote="", stringsAsFactors=FALSE)


# Map ----
{
table[table==""] <- NA
sel <- ! colnames(table) %in% c("col","group")
table$popup <- berryFunctions::popleaf(table, sel=sel, na.rm=TRUE)
html <- paste0('Map by <a href="https://github.com/brry/books">Berry B</a>, ', Sys.Date())

map <- leaflet(table) %>% 
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Esri WorldImagery") %>%
  #addResetMapButton() %>% 
  addCircleMarkers(~lon, ~lat, popup=~popup, color=~col, group=~group) %>% 
  addLayersControl(
    baseGroups=c("OSM (default)", "Esri WorldImagery"),
    overlayGroups=c("Wikipedia", "Tauschgnom", "OSM", "Lesestunden"),
    options=layersControlOptions(collapsed=FALSE)) %>% 
  addControl(position="bottomleft", html=html) %>% 
  addSearchOSM(options=searchOptions(autoCollapse=TRUE, minLength=2, hideMarkerOnCollapse=TRUE)) %>% 
  addScaleBar() %>% 
  addMeasure(primaryLengthUnit="kilometers", primaryAreaUnit="hectares",
            activeColor="#3D535D", completedColor="#7D4479") %>% 
  addControlGPS(options=gpsOptions(position="topleft", 
                activate=TRUE, autoCenter=TRUE, maxZoom=15, setView=TRUE))
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
