# free Book exchange places
# interactive map from different sources
# Berry Boessenkool, berry-b@gmx.de


# Needed packages -----
if(!requireNamespace("berryFunctions", quietly=TRUE)) install.packages("berryFunctions")
berryFunctions::library2("rjson") # for the function fromJSON (in scrape.R)
berryFunctions::library2(leaflet) # leaflet, addTiles, addCircleMarkers, addMeasure
berryFunctions::library2(leaflet.extras) # addControlGPS, gpsOptions, activateGPS, 
                                         # addSearchOSM, addFullscreenControl
# Custom functions:
write_books <- function(table, file) write.table(x=table, file=file, sep="\t", 
                       quote=FALSE, row.names=FALSE, na="")
read_books <- function(file, ...) read.table(file, sep="\t", header=TRUE, quote="", 
                 stringsAsFactors=FALSE, comment.char="", ...)
truncString <- function(x, n) ifelse(nchar(x)>n, paste0(substring(x, 1, n), "[trunc]"), x)

# Scrape sources ----
# be kind, only scrape manually, don't execute accidentally
if(FALSE) source("files/scrape.R") # 10-50 secs


if(!exists("table_wpl")){
table_wpl <- read_books("files/table_wpl.txt")
table_tgn <- read_books("files/table_tgn.txt")
table_osm <- read_books("files/table_osm.txt")
table_lst <- read_books("files/table_lst.txt")
table_bal <- read_books("files/table_bal.txt")
table_obc <- read_books("files/table_obc.txt")
}

# colors, labels ----

grp <- read.table(header=TRUE, sep=";", strip.white=TRUE, comment.char="", as.is=TRUE, text="
Abb; Label        ; Color   ; URL
wpl; Wikipedia    ; blue    ; de.wikipedia.org/wiki/Liste_öffentlicher_Bücherschränke
tgn; Tauschgnom   ; red     ; www.tauschgnom.de/offene-buecherschraenke-karte
osm; OSM          ; green   ; overpass-turbo.eu
lst; Lesestunden  ; #ff9900 ; www.lesestunden.de/karte-oeffentlicher-buecherschraenke
bal; Boite a Lire ; purple  ; www.boite-a-lire.com 
obc; Openbookcase ; black   ; openbookcase.org/map
")
grp$Group <- paste0('<font color="',grp$Color,'">\U2B24 </font><a href="https://',
                    grp$URL,'">',grp$Label,'</a>')
rownames(grp) <- grp$Abb

table_wpl$group <- grp["wpl","Group"] ; table_wpl$col <- grp["wpl", "Color"]
table_tgn$group <- grp["tgn","Group"] ; table_tgn$col <- grp["tgn", "Color"]
table_osm$group <- grp["osm","Group"] ; table_osm$col <- grp["osm", "Color"]
table_lst$group <- grp["lst","Group"] ; table_lst$col <- grp["lst", "Color"]
table_bal$group <- grp["bal","Group"] ; table_bal$col <- grp["bal", "Color"]
table_obc$group <- grp["obc","Group"] ; table_obc$col <- grp["obc", "Color"]

# Merge sources ----

table <- Reduce(function(...) merge(..., all=TRUE), list(
  table_wpl, 
  table_tgn,
  table_osm,
  table_lst,
  table_bal,
  table_obc
  ))


# Map ----
table[table==""] <- NA
ll <- table[,c("lat","lon")] ; table$lat <- NULL ; table$lon <- NULL
ll <- round(ll, 6)
tocut <- ! colnames(table) %in% c("group", "OSM")
table[,tocut] <- lapply(table[,tocut], truncString, 80) # cut long comments to reduce html size
table <- cbind(table, ll) ; rm(ll, tocut)
sel <- ! colnames(table) %in% c("col","group")
if(min(table$lon, na.rm=TRUE) < -180) stop("min table$lon is too small: ", min(table$lon, na.rm=TRUE))
if(max(table$lon, na.rm=TRUE) >  180) stop("max table$lon is too large: ", max(table$lon, na.rm=TRUE))
if(min(table$lat, na.rm=TRUE) <  -90) stop("min table$lat is too small: ", min(table$lat, na.rm=TRUE))
if(max(table$lat, na.rm=TRUE) >   90) stop("max table$lat is too large: ", max(table$lat, na.rm=TRUE))
table$popup <- berryFunctions::popleaf(table, sel=sel, na.rm=TRUE) ; rm(sel)
# table$id <- nchar(table$popup)

ref_html <- paste0('Map by <a href="https://github.com/brry/books">Berry B</a>, ', Sys.Date())
map <- leaflet(table) %>% 
  addTiles(group = "OSM (default)", options=providerTileOptions(maxZoom=19)) %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Esri WorldImagery", 
                   options=providerTileOptions(maxZoom=20)) %>%
  #addResetMapButton() %>% 
  addCircleMarkers(~lon, ~lat, popup=~popup, color=~col, group=~group) %>% 
  addControl(position="topright", html='<font size="1">Zoom in before loading layers.</font>') %>% 
  addLayersControl(
    baseGroups=c("OSM (default)", "Esri WorldImagery"),
    overlayGroups=grp$Group,
    options=layersControlOptions(collapsed=FALSE)) %>% 
  hideGroup(grp$Group) %>% 
  addControl(position="bottomleft", html=ref_html) %>% 
  addSearchOSM(options=searchOptions(autoCollapse=TRUE, minLength=2, hideMarkerOnCollapse=TRUE, zoom=16)) %>% 
  addControlGPS(options=gpsOptions(position="topleft", 
                activate=TRUE, autoCenter=TRUE, maxZoom=16, setView=TRUE)) %>% 
  addMeasure(primaryLengthUnit="kilometers", primaryAreaUnit="hectares",
            activeColor="#3D535D", completedColor="#7D4479", position="topleft") %>% 
  addScaleBar(position="topleft") %>% 
  addFullscreenControl() %>% 
  setView(10, 50, zoom=5)
print(map)
rm(ref_html)

# Export:
if(T){
htmlwidgets::saveWidget(map, "index.html", selfcontained=TRUE)
# HTML head for mobile devices:
# https://stackoverflow.com/questions/42702394/make-leaflet-map-mobile-responsive
map_h <- readLines("index.html")
map_h <- sub('<title>leaflet</title>', x=map_h,
 '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>\n<title>brry.github.io/books</title>')
writeLines(map_h, "index.html") ; rm(map_h)
}  
