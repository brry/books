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

