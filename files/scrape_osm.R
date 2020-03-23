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

cases <- readLines("files/export.osm", encoding="UTF-8")
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
