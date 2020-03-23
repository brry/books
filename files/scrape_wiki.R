# Wikipedia Liste oeffentlicher Buecherzellen
# Scrape all subpages into one grand table
# to be used for plotting on a map
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
