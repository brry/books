# Scrape several databases of book cases
# Berry Boessenkool, 2019/2020, berry-b@gmx.de

# stop source() execution if scraping happened recently.
if(difftime(Sys.time(), file.mtime("files/table_wpl.txt"), units="d") < 10)
  stop("Pages scraped less than 10 days ago. Run manually if needed.")




# 1. Wikipedia -----------------------------------------------------------------

# Wikipedia Liste oeffentlicher Buecherzellen
# Scrape all subpages into one grand table

# list of URLS

# DE Bundeslaender:
wiki_lists <- paste0("in_",c("Bayern", "Berlin", "Brandenburg", 
  "der_Freien_Hansestadt_Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", 
  "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Sachsen", 
  "Sachsen-Anhalt", "Schleswig-Holstein", "Th%C3%BCringen"))
wiki_lists <- c(wiki_lists, "im_Saarland")
# DE Baden Wuerttemberg:
bw <- "https://de.wikipedia.org/wiki/Liste_%C3%B6ffentlicher_B%C3%BCcherschr%C3%A4nke_in_Baden-W%C3%BCrttemberg"
bw <- readLines(bw, encoding="UTF-8", warn=FALSE)
bw <- bw[grep('<td><a href="/wiki/Liste', bw)]
bw <- gsub('" title=.*',"",bw)
bw <- gsub(".*BCcherschr%C3%A4nke_","",bw)
wiki_lists <- c(wiki_lists, bw)
rm(bw)
# International lists:
wiki_lists <- c(wiki_lists, "", "in_%C3%96sterreich", "in_der_Schweiz")
              

table_wpl <- pbapply::pblapply(wiki_lists, function(listURL)
{out <- try({
# append URL
if(listURL!="") listURL <- paste0("_",listURL)
url <- paste0("https://de.wikipedia.org/wiki/Liste_%C3%B6ffentlicher_B%C3%BCcherschr%C3%A4nke",
              listURL)
if(listURL=="") listURL <- "_landingPage" # for error messaging + source reference
listURL <- paste0("de.wiki",listURL)

# read table
html <- readLines(url, encoding="UTF-8", warn=FALSE)
html <- html[1:grep("</table>", html)[1] ] # first table only
#
TAB <- XML::readHTMLTable(doc=html, stringsAsFactors=FALSE, header=TRUE)
TAB <- TAB[[1]]

# rename / select columns
if(listURL=="de.wiki_im_Saarland") 
  {
  TAB$V7 <- NULL
  colnames(TAB) <- c("Bild","Ausführung","Ort","Seit","Anmerkung","Lage")
  }
colnames(TAB) <- sub("\n",                "",     colnames(TAB))
colnames(TAB) <- sub("Ausf.*hrung",       "Typ",  colnames(TAB))
colnames(TAB) <- sub("Geokoordinaten",    "Lage", colnames(TAB))
if(!"Ort" %in% colnames(TAB)) 
  {
  colnames(TAB) <- sub("Stadtteil",       "Ort",  colnames(TAB))
  colnames(TAB) <- sub("Bezirk.*Ortsteil","Ort",  colnames(TAB))
  colnames(TAB) <- sub("Ortsteil / Bezirk","Ort",  colnames(TAB))
  }
TAB$Bild <- NULL # remove column
TAB$Seit <- berryFunctions::removeSpace(gsub(".*\U{2660}", "", TAB$Seit))
TAB$Lage <- gsub("\U{2299}","",TAB$Lage)
TAB$Lage <- gsub("Koordinaten fehlen! Hilf mit.","",TAB$Lage)
TAB[] <- lapply(TAB, gsub, pattern="\n", replacement=" _ ")
TAB[] <- lapply(TAB, gsub, pattern="\t", replacement=" _ ")

# Coordinates from html source code (tr: table row)
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
})

table_wpl <- Reduce(function(...) merge(..., all=TRUE), table_wpl)
rm(wiki_lists)
write_books(table_wpl, "files/table_wpl.txt")



# 2. Tauschgnom ----------------------------------------------------------------

# The database has meta information (adress + case descriptions)
# I'm only scraping the map with limited info (but all cases)

table_tgn <- readLines("https://www.tauschgnom.de/offene-buecherschraenke-karte", encoding="UTF-8")
table_tgn <- grep("addOverlay(", table_tgn, fixed=TRUE, value=TRUE)
table_tgn <- strsplit(table_tgn, "LatLng(", fixed=TRUE)
table_tgn <- sapply(table_tgn, "[", 2)
table_tgn <- gsub("false), title: \"", "", table_tgn)
table_tgn <- gsub("\"}));", "", table_tgn)
table_tgn <- gsub(";", ",", table_tgn)
table_tgn <- strsplit(table_tgn, ", ")
table_tgn <- berryFunctions::l2df(table_tgn)
colnames(table_tgn) <- c("lat", "lon", "Ort", "Lage")
table_tgn$lat <- as.numeric(table_tgn$lat)
table_tgn$lon <- as.numeric(table_tgn$lon)
table_tgn$Source <- "tauschgnom.de"
write_books(table_tgn, "files/table_tgn.txt")



# 3. OSM -----------------------------------------------------------------------

browseURL("https://overpass-turbo.eu/")
' Copy the query below for constant bbox, then click "Run" ("Ausfuehren")
node[amenity=public_bookcase](44, -2, 56, 20);
out;
' # "Speichere als OSM Rohdaten" at C:/Dropbox/R/books  /files/export.osm
  
# Potential expansion ----
# https://github.com/ToastHawaii/public-bookcase-map/blob/master/src/filters.ts
# https://wiki.openstreetmap.org/wiki/Overpass_API/Language_Guide#By_exact_name
# https://wiki.openstreetmap.org/wiki/Tags#Finding_your_tag
# https://taginfo.openstreetmap.org/search?q=book
# https://wiki.openstreetmap.org/w/index.php?search=books&title=Special%3ASearch&profile=default&fulltext=1

' 
(
  node[amenity=public_bookcase]({{bbox}});
  node[amenity=give_box]({{bbox}});
  node[amenity=library][name!~ibliothek]({{bbox}});
  );
out;
'

cases <- readLines("files/export.osm", encoding="UTF-8", warn=FALSE)
n_beg <- grep( "<node id", cases)
n_end <- grep("</node>", cases)
if(length(n_beg)!=length(n_end)) stop("Length n_beg (",length(n_beg),") != length n_end (",length(n_end),")")

table_osm <- pbapply::pblapply(seq_along(n_beg), function(i){
  ci <- cases[n_beg[i]:(n_end[i]-1)]
  co <- strsplit(ci[1], "\"")[[1]]
  co <- data.frame(Source="OSM_overpass_query", id=co[2], lat=as.numeric(co[4]), lon=as.numeric(co[6]))
  ci <- sub("public_bookcase:type", "type", ci)
  ci <- sub("&quot;", "'", ci)
  ci <- strsplit(ci[-1], "\"") 
  ci <- sapply(ci, "[", c(2,4))
  ci <- apply(ci, 2, paste, collapse=": ")
  ci <- sapply(ci, truncString, 50) 
  co$OSM <- paste(ci, collapse="<br>")
  return(co)
})
table_osm <- do.call(rbind, table_osm)

# Correct umlaute:
table_osm$OSM <- iconv(table_osm$OSM, from="UTF-8", to="UTF-8")

write_books(table_osm, "files/table_osm.txt")

rm(n_beg, n_end, cases)





# 4. Lesestunden ---------------------------------------------------------------

table_lst <- rjson::fromJSON(file="https://www.lesestunden.de/buchschrank/v1/bookcase")
table_lst <- lapply(table_lst, function(x) {
  x[sapply(x, is.null)] <- NA
  x <- unlist(x)
  x <- gsub("\n", " ", x)
  x <- gsub("\t", " ", x)
  x})
table_lst <- data.frame(do.call(rbind, table_lst), stringsAsFactors=FALSE)
colnames(table_lst) <- sub("lng", "lon", colnames(table_lst))
table_lst$Source <- "lesestunden.de"
table_lst$lat <- as.numeric(table_lst$lat)
table_lst$lon <- as.numeric(table_lst$lon)

# correction lon value:
table_lst$lon[table_lst$id=="4226"] <- 13.082462
write_books(table_lst, "files/table_lst.txt")



# 5. Boite a lire --------------------------------------------------------------

# URL found through https://github.com/Binnette/bookcases-to-check
table_bal <- read.csv2("https://www.boite-a-lire.com/boite_a_lire.csv", 
                         stringsAsFactors=FALSE, encoding="UTF-8", quote="", skip=1)

table_bal$Source <- "boite-a-lire.com"

table_bal[] <- lapply(table_bal, function(x){
  if( ! is.character(x) &  ! is.factor(x)) return(x)
  x <- iconv(x, from="UTF-8", to="UTF-8")
  x <- gsub("\n", " ", x)
  x <- gsub("\t", " ", x)
  x
  })

ll <- strsplit(table_bal$Coord_GPS, ",")
ll <- berryFunctions::l2df(ll)
ll[] <- lapply(ll, as.numeric)
colnames(ll) <- c("lat","lon")

table_bal <- cbind(table_bal, ll)
table_bal$Coord_GPS <- NULL # delete column
write_books(table_bal, "files/table_bal.txt")
rm(ll)



# 6. OpenBookCase --------------------------------------------------------------

table_obc <- rjson::fromJSON(file="https://openbookcase.de/api/list")
table_obc <- lapply(table_obc[[2]], function(x) {
  x[sapply(x, is.null)] <- NA
  x <- unlist(x)
  x <- gsub("\n", " ", x)
  x <- gsub("\t", " ", x)
  x[x==""] <- NA
  x[x==" "] <- NA
  x[x=="-"] <- NA
  x[x=="0"] <- NA
  x[x=="..."] <- NA
  x})
table_obc <- data.frame(do.call(rbind, table_obc), stringsAsFactors=FALSE)
table_obc$Source <- "openbookcase.org"
write_books(table_obc, "files/table_obc.txt")
table_obc <- read_books("files/table_obc.txt")
# remove unicode chars (kyrillic, especially):
table_obc[] <- lapply(table_obc, gsub, pattern="<U.....>", replacement="") 
table_obc[] <- lapply(table_obc, gsub, pattern="&#..;", replacement="") 
table_obc[] <- lapply(table_obc, berryFunctions::removeSpace) 
table_obc$lat <- as.numeric(table_obc$lat)
table_obc$lon <- as.numeric(table_obc$lon)
table_obc$lon <- round(table_obc$lon, 6)
write_books(table_obc, "files/table_obc.txt")

