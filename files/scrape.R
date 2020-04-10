# Scrape several databases of book cases
# Berry Boessenkool, 2019/2020, berry-b@gmx.de

# stop source() execution if scraping happened recently.
if(difftime(Sys.time(), file.mtime("files/table_wik.txt"), units="d") < 10)
  stop("Pages scraped less than 10 days ago. Run manually if needed.")

# 1. Wikipedia ----

# Wikipedia Liste oeffentlicher Buecherzellen
# Scrape all subpages into one grand table
# Berry Boessenkool, Sept 2019 + Mrz 2020, berry-b@gmx.de

# list of URLS -----

# DE Bundeslaender
wiki_lists <- paste0("in_",c("Bayern", "Berlin", "Brandenburg", 
  "der_Freien_Hansestadt_Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", 
  "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Sachsen", 
  "Sachsen-Anhalt", "Schleswig-Holstein", "Th%C3%BCringen"))
wiki_lists <- c(wiki_lists, "im_Saarland")

# DE Baden Wuerttemberg:
bw <- "https://de.wikipedia.org/wiki/Liste_%C3%B6ffentlicher_B%C3%BCcherschr%C3%A4nke_in_Baden-W%C3%BCrttemberg"
bw <- readLines(bw, encoding="UTF-8")
bw <- bw[grep('<td><a href="/wiki/Liste', bw)]
bw <- gsub('" title=.*',"",bw)
bw <- gsub(".*BCcherschr%C3%A4nke_","",bw)
#
wiki_lists <- c(wiki_lists, bw)
rm(bw)

# Other lists
wiki_lists <- c(wiki_lists, "", "in_%C3%96sterreich", "in_der_Schweiz")
              
"https://de.wikipedia.org/wiki/Liste_%C3%B6ffentlicher_B%C3%BCcherschr%C3%A4nke_"

# scrape function -----

scrape_wiki <- function(listURL)
{out <- try({
# browser()
# append URL ----
if(listURL!="") listURL <- paste0("_",listURL)
url <- paste0("https://de.wikipedia.org/wiki/Liste_%C3%B6ffentlicher_B%C3%BCcherschr%C3%A4nke",
              listURL)
if(listURL=="") listURL <- "_landingPage" # for error messaging + source reference
listURL <- paste0("de.wiki",listURL)

# read table ----
html <- readLines(url, encoding="UTF-8")
html <- html[1:grep("</table>", html)[1] ] # first table only
#
TAB <- XML::readHTMLTable(doc=html, stringsAsFactors=FALSE, header=TRUE)
TAB <- TAB[[1]]

# rename / select Columns ----
colnames(TAB) <- sub("\n",                "",     colnames(TAB))
colnames(TAB) <- sub("Ausf.*hrung",       "Typ",  colnames(TAB))
colnames(TAB) <- sub("Geokoordinaten",    "Lage", colnames(TAB))
if(!"Ort" %in% colnames(TAB)) 
  {
  colnames(TAB) <- sub("Stadtteil",       "Ort",  colnames(TAB))
  colnames(TAB) <- sub("Bezirk.*Ortsteil","Ort",  colnames(TAB))
  }
TAB$Bild <- NULL # remove column
TAB$Seit <- berryFunctions::removeSpace(gsub(".*\U{2660}", "", TAB$Seit))
TAB$Lage <- gsub("\U{2299}","",TAB$Lage)
TAB$Lage <- gsub("Koordinaten fehlen! Hilf mit.","",TAB$Lage)
TAB[] <- lapply(TAB, gsub, pattern="\n", replacement=" _ ")
TAB[] <- lapply(TAB, gsub, pattern="\t", replacement=" _ ")

# Column order:
# ToDo!
#TAB <- TAB[,c("Typ","Ort","Seit","Anmerkung","Lage")]

# Coordinates from html source code (tr: table row) ----
tr_beg <- grep( "<tr>", html)
tr_end <- grep("</tr>", html)
if(length(tr_beg)-1!=nrow(TAB)) stop("Length tr_beg (",length(tr_beg),") != nrow TAB (",nrow(TAB),")")
if(length(tr_end)-1!=nrow(TAB)) stop("Length tr_end (",length(tr_end),") != nrow TAB (",nrow(TAB),")")

coords <- sapply(2:length(tr_beg), function(i) # start at second row (first is table header)
  {
  cc <- html[tr_beg[i]:tr_end[i] ]
  cc <- paste(cc, collapse="")
  cc <- strsplit(cc, split="params=")[[1]][2]
  cc <- strsplit(cc, split="region:")[[1]][1]
  if(cc=="" | is.na(cc)) return(c(lat=NA, lon=NA))
  cc <- strsplit(cc, "_")[[1]]
  lat <- as.numeric(cc[1])
  if(cc[2]=="S") lat <- -lat
  lon <- as.numeric(cc[3])
  if(cc[4]=="W") lon <- -lon
  c(lat=lat, lon=lon)
  })
coords <- data.frame(t(coords))
# collecting, output
cbind(data.frame(Source=listURL), TAB, coords)
}, silent=TRUE)
if(inherits(out, "try-error")) message(listURL, "   failed with ", sub("\n", "  ", out))
out
}


# Test ----

# currently manual testing. ToDo: systematic tests
if(FALSE){
str(testcase)
testcase <- scrape_wiki(wiki_lists[1]) # Bayern
testcase <- scrape_wiki(wiki_lists[3]) # Brandenburg
testcase <- scrape_wiki(wiki_lists[55]) # Landkreis_Ravensburg
testcase <- scrape_wiki(wiki_lists[60]) # Landing page
}


# Call ----

table_wiki <- pbapply::pblapply(wiki_lists, scrape_wiki) # ca 30 secs
table_wiki <- Reduce(function(...) merge(..., all=TRUE), table_wiki)
rm(wiki_lists, scrape_wiki)



# 2. Tauschgnom.de ----

# The database has meta information (adress + case descriptions)
# for now I'm only scraping the map with limited info (but all cases)

h <- readLines("https://www.tauschgnom.de/offene-buecherschraenke-karte", encoding="UTF-8")
h <- grep("addOverlay(", h, fixed=TRUE, value=TRUE)
h <- strsplit(h, "LatLng(", fixed=TRUE)
h <- sapply(h, "[", 2)
h <- gsub("false), title: \"", "", h)
h <- gsub("\"}));", "", h)
h <- gsub(";", ",", h)
h <- strsplit(h, ", ")
h <- berryFunctions::l2df(h)
colnames(h) <- c("lat", "lon", "Ort", "Lage")
h$lat <- as.numeric(h$lat)
h$lon <- as.numeric(h$lon)
h$Source <- "tauschgnom.de"

table_tauschgnom <- h
rm(h)



# 3. OSM ----

# scrape bookcases on osm
# Berry Boessenkool, Mrz 2020, berry-b@gmx.de

# slow way ----

if(FALSE){
berryFunctions::library2(sf)
berryFunctions::library2(osmdata)

cases <- opq(c(3, 46, 16, 55), timeout=60*5) %>%
    add_osm_feature(key="amenity", value="public_bookcase") %>%
    osmdata_sf()

cases <- cases$osm_points
table_osm <- cbind(Source="OSM_overpass_query", cases, sf::st_coordinates(cases))

table_osm$geometry <- NULL
# Correct umlaute:
table_osm[] <- lapply(table_osm, function(x){
  if( ! is.character(x) &  ! is.factor(x)) return(x)
  iconv(x, from="UTF-8", to="UTF-8")
  })

colnames(table_osm) <- sub("X","lon",colnames(table_osm))
colnames(table_osm) <- sub("Y","lat",colnames(table_osm))
}


# much faster way, but manual step ----

browseURL("https://overpass-turbo.eu/")
' Copy the query below for constant bbox, then click "Run" ("Ausfuehren")
node
  [amenity=public_bookcase]
  (44,-2,56,20);
out;
' # "Speichere als OSM Rohdaten" at C:/Dropbox/R/books  /files/export.osm

cases <- readLines("files/export.osm", encoding="UTF-8", warn=FALSE)
n_beg <- grep( "<node id", cases)
n_end <- grep("</node>", cases)
if(length(n_beg)!=length(n_end)) stop("Length n_beg (",length(n_beg),") != length n_end (",length(n_end),")")

table_osm <- pbapply::pblapply(seq_along(n_beg), function(i){
  ci <- cases[n_beg[i]:(n_end[i]-1)]
  co <- strsplit(ci[1], "\"")[[1]]
  co <- data.frame(source="OSM_overpass_query", id=co[2], lat=as.numeric(co[4]), lon=as.numeric(co[6]))
  ci <- sub("public_bookcase:type", "type", ci)
  ci <- sub("&quot;", "'", ci)
  ci <- strsplit(ci[-1], "\"") 
  ci <- sapply(ci, "[", c(2,4))
  ci <- apply(ci, 2, paste, collapse=": ")
  co$OSM <- paste(ci, collapse="<br>")
  return(co)
})
table_osm <- do.call(rbind, table_osm)

# Correct umlaute:
table_osm$OSM <- iconv(table_osm$OSM, from="UTF-8", to="UTF-8")

rm(n_beg, n_end, cases)


# Potential expansion ----

# https://github.com/ToastHawaii/public-bookcase-map/blob/master/src/filters.ts
# amenity="public_bookcase"
# amenity="give_box"
# amenity="library", "fee"="no" #  Library free of charge
# shop="books", "charity"="yes" # Charity book shop
# shop="charity", "books"
# shop="freestore"
# shop="charity", "payment:none"="yes"
# shop="second_hand", "payment:none"="yes"
# shop="charity", "fee"="no"
# shop="second_hand", "fee"="no"`,

# Logical OR combinations can be implemented with the package’s internal c method, so that the above example can be extended to all amenities that are either restaurants OR pubs with
# 
# pubs <- opq ("portsmouth usa") %>%
#     add_osm_feature(key = "amenity", value = "pub") %>%
#     osmdata_sf()
# restaurants <- opq ("portsmouth usa") %>%
#     add_osm_feature(key = "amenity", value = "restaurant") %>%
#     osmdata_sf()
# c (pubs, restaurants)

# osmplotr
# bbox <- get_bbox(c(12.481225, 53.169504, 12.681225, 53.269504)); bbox
# dat_FO <- extract_osm_objects(key="landuse", bbox=bbox, quiet=TRUE, value="forest")



# 4. Lesestunden ----

berryFunctions::library2("rjson")
table_lesestunden <- rjson::fromJSON(file="https://www.lesestunden.de/buchschrank/v1/bookcase")
table_lesestunden <- lapply(table_lesestunden, function(x) {
  x[sapply(x, is.null)] <- NA
  x <- unlist(x)
  x <- gsub("\n", " ", x)
  x <- gsub("\t", " ", x)
  x})
table_lesestunden <- data.frame(do.call(rbind, table_lesestunden), stringsAsFactors=FALSE)
colnames(table_lesestunden) <- sub("lng", "lon", colnames(table_lesestunden))
table_lesestunden$Source <- "lesestunden.de"
table_lesestunden$lat <- as.numeric(table_lesestunden$lat)
table_lesestunden$lon <- as.numeric(table_lesestunden$lon)

# correction lon value:
table_lesestunden$lon[table_lesestunden$id=="4226"] <- 13.082462




# 5. Boite a lire ----


# URL found through https://github.com/Binnette/bookcases-to-check

table_boite <- read.csv2("https://www.boite-a-lire.com/boite_a_lire.csv", 
                         stringsAsFactors=FALSE, encoding="UTF-8")

table_boite$Source <- "boite-a-lire.com"

table_boite[] <- lapply(table_boite, function(x){
  if( ! is.character(x) &  ! is.factor(x)) return(x)
  x <- iconv(x, from="UTF-8", to="UTF-8")
  x <- gsub("\n", " ", x)
  x <- gsub("\t", " ", x)
  x
  })

ll <- strsplit(table_boite$Coord_GPS, ",")
ll <- berryFunctions::l2df(ll)
ll[] <- lapply(ll, as.numeric)
colnames(ll) <- c("lat","lon")

table_boite <- cbind(table_boite, ll)
table_boite$Coord_GPS <- NULL # delete column
rm(ll)



# 6. OpenbookCase ----


# scrape bookcases on openbookcase.org
# Berry Boessenkool, Apr 2020, berry-b@gmx.de

berryFunctions::library2("rjson")
table_openbookcase <- rjson::fromJSON(file="https://openbookcase.org/api/list")
table_openbookcase <- lapply(table_openbookcase[[2]], function(x) {
  x[sapply(x, is.null)] <- NA
  x <- unlist(x)
  x <- gsub("\n", " ", x)
  x <- gsub("\t", " ", x)
  x[x==""] <- NA
  x})
table_openbookcase <- data.frame(do.call(rbind, table_openbookcase), stringsAsFactors=FALSE)
table_openbookcase$Source <- "openbookcase.org"
table_openbookcase$lat <- as.numeric(table_openbookcase$lat)
table_openbookcase$lon <- as.numeric(table_openbookcase$lon)


