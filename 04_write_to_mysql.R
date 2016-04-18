# SET Connection

setwd("~/ownCloud/docenti_univ_ita")
source('00_connect_to_db.R') # not in repo
## require(RMySQL)
# pw <- {
#   "yourpassword"
# }
# 
# con <- dbConnect(RMySQL::MySQL(), 
#                  host = "999.999.999.99",
#                  port = 3306,
#                  dbname = "db_name", 
#                  username = "user", 
#                  password = pw)
# rm(pw)

source('https://raw.githubusercontent.com/fraba/R_cheatsheet/master/database.R')
atenei <- sqliteGetTable("atenei_db.sqlite", "atenei")
load("cineca_data_store.RData")
nrow(data_store$data)

# Important cleaning !!!
data_store$data$Ateneo <- gsub("\xe0", "à", data_store$data$Ateneo)

## Process surnames

surnameOnly <- function(x) {
  to <- min(which(unlist(strsplit(x, split = '')) %in% letters)) - 3
  return(substr(x, 1, to))
}

nameOnly <- function(x) {
  from <- min(which(unlist(strsplit(x, split = '')) %in% letters)) - 1
  possibleError <- tryCatch(
    {
      num_char <- nchar(x)
    },
    error=function(err) {
      print(paste("MY_ERROR:  ",err))
    }
  )
  if(inherits(possibleError, "error")) {
    return(NA)
  } else {
    if (!exists('num_char')) {
      return(NA)
    } else {
      return(substr(x, from, num_char))
    }
  }
}
require(stringr)
data_store$data[[2]] <- str_trim(data_store$data[[2]])
data_store$data$surname <- sapply(data_store$data[[2]], surnameOnly)
data_store$data$name <- sapply(data_store$data[[2]], nameOnly)
data_store$data$period <- gsub("ad oggi", "2016", data_store$data$period)
data_store$data$period <- as.numeric(str_extract(data_store$data$period, "\\d{4}"))

# Missing two ateneos
unique(data_store$data$Ateneo)[!( unique(data_store$data$Ateneo) %in% atenei$ateneo_name_cineca)]

require(plyr)
atenei <- rbind.fill(atenei, 
                     data.frame(ateneo_name_cineca = c("SUM - Ist. Italiano di SCIENZE UMANE FIRENZE",
                                                       "Univ. Telematica \"LEONARDO da VINCI\""),
                                ateneo_wikidata_name = c("Istituto italiano di scienze umane",
                                                         "Università Leonardo da Vinci"),
                                ateneo_wikidata_id = c("wd:Q3803772", "wd:Q4005946"),
                                stringsAsFactors = FALSE))


# TABLE: ateneo_mysql_table

ateneo_mysql_table <- 
  data.frame(wikidata_id = atenei$ateneo_wikidata_id,
             wikidata_label = NA,
             lon = NA,
             lat = NA,
             regione_id = NA,
             stringsAsFactors = FALSE)

## WIKIDATA
require(jsonlite)
require(stringr)
for (i in 1:nrow(ateneo_mysql_table)) {
  entity_id <- gsub("wd:", "", ateneo_mysql_table$wikidata_id[i])
  print(entity_id)
  base_url <- 'http://www.wikidata.org/wiki/Special:EntityData/'
  wikidata <- fromJSON(paste0(base_url, entity_id, ".json"))
  ateneo_mysql_table$wikidata_label[i] <- 
    wikidata$entities[[entity_id]]$labels$it$value
  lat <-
    wikidata$entities[[entity_id]]$claims$P625$mainsnak$datavalue$value$latitude
  lon <- 
    wikidata$entities[[entity_id]]$claims$P625$mainsnak$datavalue$value$longitude
  if (!is.null(lat)) {
    ateneo_mysql_table$lat[i] <- lat
    ateneo_mysql_table$lon[i] <- lon
  }
}

## REGION
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
xy <- cbind(ateneo_mysql_table$lon, ateneo_mysql_table$lat)
sp_points <-  SpatialPoints(xy)
proj4string(sp_points) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

in_region <- over(sp_points, italy_map)
ateneo_mysql_table$regione_id <- in_region$wid

# TABLE: regione_mysql_table

regione_mysql_table <- 
  data.frame(wikidata_id = unique(ateneo_mysql_table$regione_id),
             nome = NA,
             popolazione = NA,
             lon = NA,
             lat = NA,
             stringsAsFactors = FALSE)

# Add population
for (i in 1:nrow(regione_mysql_table)) {
  r <- gsub("wd:", "", regione_mysql_table$wikidata_id[i])
  print(r)
  base_url <- 'http://www.wikidata.org/wiki/Special:EntityData/'
  wikidata <- fromJSON(paste0(base_url, r, ".json"))
  pop <-
    as.integer(wikidata$entities[[r]]$claims$P1082$mainsnak$datavalue$value$amount)
  if (!is.null(pop)) {
    regione_mysql_table$popolazione[i] <- pop[1]
  }
  label <-
    wikidata$entities[[r]]$labels$it$value
  regione_mysql_table$nome[i] <- label
  
  lat <-
    wikidata$entities[[r]]$claims$P625$mainsnak$datavalue$value$latitude
  lon <- 
    wikidata$entities[[r]]$claims$P625$mainsnak$datavalue$value$longitude
  if (!is.null(lat)) {
    regione_mysql_table$lon[i] <- lon
      regione_mysql_table$lat[i] <- lat
  }
}

## Write table (in this order)
dbWriteTable(con, value = regione_mysql_table, name = "regione_wt_coord", append = TRUE, row.names=0) 
dbWriteTable(con, value = ateneo_mysql_table, name = "ateneo", append = TRUE, row.names=0) 

save(regione_mysql_table, file = 'regione_mysql_table.RData')
save(ateneo_mysql_table, file = 'ateneo_mysql_table.RData')

# Check duplicates
# tmp_duplicates <- data_store$data[,c("Cognome e Nome", "period")]
# tmp_duplicates$dup <-  duplicated(tmp_duplicates)


# TABLE: docente_ruolo_mysql_table
docente_ruolo_mysql_table <- 
  data.frame(nome = data_store$data$name,
             cognome = data_store$data$surname,
             anno = data_store$data$period,
             genere = data_store$data$Genere,
             fascia = data_store$data$Fascia, 
             ateneo = data_store$data$Ateneo,
             facolta = data_store$data$Facoltà,
             dipartimento  = data_store$data$`Struttura di afferenza`,
             ssd = data_store$data$S.S.D.,
             sc = data_store$data$S.C.,
             stringsAsFactors = FALSE)

keyvalue <- atenei$ateneo_wikidata_id
names(keyvalue) <- atenei$ateneo_name_cineca

docente_ruolo_mysql_table$ateneo_id <- keyvalue[docente_ruolo_mysql_table$ateneo]
docente_ruolo_mysql_table$ateneo <- NULL

list_of_df_chunks <- split(docente_ruolo_mysql_table, 
                           (seq(nrow(docente_ruolo_mysql_table))-1) %/% 50000)

for (df in list_of_df_chunks) {
  dbWriteTable(con, value = df, name = "docente_ruolo", append = TRUE, row.names=0)
}

save(docente_ruolo_mysql_table, file = 'docente_ruolo_mysql_table.RData')

# TABLE: join_cinceca_wd

join_cineca_wd_mysql_table <-
  data.frame(cineca_name = atenei$ateneo_name_cineca,
             wikidata_id = atenei$ateneo_wikidata_id,
             stringsAsFactors = FALSE)

dbWriteTable(con, value = join_cineca_wd_mysql_table, 
             name = "join_cineca_wd", append = TRUE, row.names=0)

save(join_cineca_wd_mysql_table, file = 'join_cineca_wd_mysql_table.RData')


# TABLE: faculty_wt_coordinates
require(data.table)
docente_ruolo_mysql_table <- data.table(docente_ruolo_mysql_table)
setkey(docente_ruolo_mysql_table, "ateneo_id")
ateneo_mysql_table <- data.table(ateneo_mysql_table)
names(ateneo_mysql_table)[1] <- "ateneo_id"
setkey(ateneo_mysql_table, "ateneo_id")
faculty_wt_coordinates <-
  merge(docente_ruolo_mysql_table[,.(ateneo_id, facolta)], 
        ateneo_mysql_table[,.(ateneo_id, wikidata_label, lon, lat)])
faculty_wt_coordinates <- faculty_wt_coordinates[facolta!="",]
setkeyv(faculty_wt_coordinates, c("facolta","ateneo_id"))
faculty_wt_coordinates <- unique(faculty_wt_coordinates)

dbWriteTable(con, value = faculty_wt_coordinates, 
             name = "faculty_wt_coordinates", append = TRUE, row.names=0)

# TABLE: department_wt_coordinates
require(data.table)
department_wt_coordinates <-
  merge(docente_ruolo_mysql_table[,.(ateneo_id, dipartimento)], 
        ateneo_mysql_table[,.(ateneo_id, wikidata_label, lon, lat)])
department_wt_coordinates <- department_wt_coordinates[dipartimento!="Dip. Non disponibile",]
setkeyv(department_wt_coordinates, c("dipartimento","ateneo_id"))
department_wt_coordinates <- unique(department_wt_coordinates)

dbWriteTable(con, value = department_wt_coordinates, 
             name = "department_wt_coordinates", append = TRUE, row.names=0)

dbDisconnect(con)
