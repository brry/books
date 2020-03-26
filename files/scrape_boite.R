# scrape bookcases on www.boite-a-lire.com
# Berry Boessenkool, Mrz 2020, berry-b@gmx.de

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
