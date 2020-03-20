# free Book exchange places
# interactive map from different sources
# Berry Boessenkool, berry-b@gmx.de

library(leaflet) # leaflet, addTiles, addCircleMarkers
library(leaflet.extras) # addControlGPS, gpsOptions, activateGPS


# Wikipedia ----

source("scrape_wiki.R")
wiki_table <- pbapply::pblapply(wiki_lists, scrape_wiki) # ca 30 secs
wiki_table <- Reduce(function(...) merge(..., all=TRUE), wiki_table)
wiki_table[wiki_table==""] <- NA
write.table(wiki_table, "table.txt", sep="\t", quote=FALSE, row.names=FALSE, na="")


# Map ----

wiki_table$popup <- berryFunctions::popleaf(wiki_table, na.rm=TRUE)

map <- leaflet(wiki_table) %>% addTiles() %>% 
       addCircleMarkers(~lon, ~lat, popup=~popup) %>% 
       addControlGPS(options=gpsOptions(position="topleft", 
                     activate=TRUE, autoCenter=TRUE, maxZoom=15, setView=TRUE))
map

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
