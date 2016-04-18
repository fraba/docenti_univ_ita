## Geolocate atenei
require(SPARQL)
require(stringr)
endpoint <- 'https://query.wikidata.org/sparql'
query <- 
  "
SELECT *  WHERE {
?university wdt:P31 wd:Q3918.
?university wdt:P17 wd:Q38.
?university rdfs:label ?label.
OPTIONAL{?university wdt:P625 ?coordinates}
FILTER(LANG(?label) = '' || LANGMATCHES(LANG(?label), 'it'))
}
"
it_univ <- 
  SPARQL(endpoint, query, curl_args = list(.encoding = 'UTF-8'))$results
it_univ$clean_label <- str_extract(it_univ$label, "(?<=\\\").*?(?=\\\")")
it_univ$wd <- str_extract(it_univ$university, "(?<=\\<).*?(?=\\>)")

atenei <- data.frame(cineca_name = unique(docenti16$Ateneo), stringsAsFactors = FALSE) 

require(stringdist)
require(stringr)
atenei$cineca_name <- str_replace_all(atenei$cineca_name, "[^[:graph:]]", " ") 
atenei$wikidata_name <- NA
for (i in 1:nrow(atenei)) {
  ateneo <- atenei$cineca_name[i]
  n_words <- str_count(ateneo, "\\S+")
  if (n_words == 1) {ateneo <- paste0("università degli studi ", ateneo)}
  atenei$wikidata_name[i] <- 
    it_univ$clean_label[which.min(stringdist(tolower(ateneo), tolower(it_univ$clean_label)))]
  atenei$wikidata_id[i] <- 
    it_univ$wd[which.min(stringdist(tolower(ateneo), tolower(it_univ$clean_label)))]
}

source('https://raw.githubusercontent.com/fraba/R_cheatsheet/master/database.R')
# sqliteWriteTable("atenei_db.sqlite", "atenei", atenei)
atenei <- sqliteGetTable("atenei_db.sqlite", "atenei")

require(jsonlite)
atenei$lat <- NA
atenei$lon <- NA
for (i in 1:nrow(atenei)) {
  entity_id <- gsub("/", "", str_extract(atenei$wikidata_id[i], "/Q(.*)$"))
  print(entity_id)
  base_url <- 'http://www.wikidata.org/wiki/Special:EntityData/'
  wikidata <- fromJSON(paste0(base_url, entity_id, ".json"))
  lat <-
    wikidata$entities[[entity_id]]$claims$P625$mainsnak$datavalue$value$latitude
  lon <- 
    wikidata$entities[[entity_id]]$claims$P625$mainsnak$datavalue$value$longitude
  if (!is.null(lat)) {
    atenei$lat[i] <- lat
    atenei$lon[i] <- lon
  }
}
View(subset(atenei, is.na(lon)))

require(ggmap)
italy_box_y <- c(36, 47.5)
italy_box_x <- c(6.1, 19.5) 

myMap <- get_map(location = c(6.1, 36, 19.5, 47.5), 
                 source="stamen", 
                 maptype="watercolor", 
                 crop=TRUE)
ggmap(myMap) +
  geom_point(data=atenei, aes(x=lon, y=lat), alpha = 0.8, size = 0.8)

require(rgdal)
require(maptools)
require(rgeos)
require(grid)
require(plyr)

italy_map <- 
  readOGR("/Users/francesco/Desktop/GIS_Data/Administrative\ units/Italy/ISTAT/Reg2011_WGS84", 
          layer="Reg2011_WGS84_simp",
          verbose=FALSE)

## Count participation by comune
xy <- cbind(atenei$lon, atenei$lat)
sp_points <-  SpatialPoints(xy)
proj4string(sp_points) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

in_region <- over(sp_points, italy_map)
atenei$regione_name <- in_region$NOME
atenei$regione_istat_id <- in_region$COD_REG
atenei$regione_wikidata_id <- in_region$wid

names(atenei)[1:3] <- c('ateneo_name_cineca', 'ateneo_wikidata_name',
                        'ateneo_wikidata_id')
atenei$ateneo_wikidata_id <- gsub("^(.*)/", "wd:", atenei$ateneo_wikidata_id)

# Add population
for (r in unique(atenei$regione_wikidata_id)) {
  print(r)
  base_url <- 'http://www.wikidata.org/wiki/Special:EntityData/'
  wikidata <- fromJSON(paste0(base_url, r, ".json"))
  pop <-
    wikidata$entities[[r]]$claims$P625$mainsnak$datavalue$value$population
  if (!is.null(pop)) {
    
  }
}

source('https://raw.githubusercontent.com/fraba/R_cheatsheet/master/wikidata.R')
atenei <- 
  AugmentWithWikiData(atenei, 
                      wid_vector = 'regione_wikidata_id', 
                      instance_of = 'Q16110', 
                      attributes = 'P1082', lang = 'it')

atenei$population[atenei$regione_wikidata_id == 'wd:Q1462'] <- 1658649

sqliteWriteTable("atenei_db.sqlite", "atenei", atenei)
