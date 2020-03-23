# scrape bookcases on lesestunden.de
# Berry Boessenkool, Mrz 2020, berry-b@gmx.de



berryFunctions::library2("rjson")
table_lesestunden <- rjson::fromJSON(file="https://www.lesestunden.de/buchschrank/v1/bookcase")
table_lesestunden <- lapply(table_lesestunden, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)})
table_lesestunden <- data.frame(do.call(rbind, table_lesestunden), stringsAsFactors=FALSE)
colnames(table_lesestunden) <- sub("lng", "lon", colnames(table_lesestunden))
table_lesestunden$Source <- "lesestunden.de"
table_lesestunden$lat <- as.numeric(table_lesestunden$lat)
table_lesestunden$lon <- as.numeric(table_lesestunden$lon)

# correction lon value:
table_lesestunden$lon[table_lesestunden$id=="4226"] <- 13.082462
