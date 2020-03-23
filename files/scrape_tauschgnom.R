# scrape bookcases on tauschgnom.de
# Berry Boessenkool, Mrz 2020, berry-b@gmx.de

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
