setwd("~/ownCloud/docenti_univ_ita")
load("~/ownCloud/docenti_univ_ita/docente_ruolo_mysql_table.RData")
load("~/ownCloud/docenti_univ_ita/ateneo_mysql_table.RData")
load("~/ownCloud/docenti_univ_ita/regione_mysql_table.RData")

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

require(plyr)
regione_mysql_table <- 
  plyr::rename(regione_mysql_table, c("nome" = "nome_regione"))

docente_ruolo_mysql_table <-
  merge(docente_ruolo_mysql_table, ateneo_mysql_table,
        by.x = "ateneo_id", by.y = "wikidata_id")

docente_ruolo_mysql_table <-
  merge(docente_ruolo_mysql_table, regione_mysql_table,
        by.x = "regione_id", by.y = "wikidata_id")

require(dplyr)
docenti_by_year <-
  docente_ruolo_mysql_table %>%
  dplyr::group_by(anno) %>%
  dplyr::summarize(docenti = n())
ggplot(docenti_by_year, aes(x=anno, y=docenti)) + geom_line()

# dbWriteTable(con, value = as.data.frame(docenti_by_year), 
#              name = "count_docenti_by_year", append = TRUE, row.names=0)
save(docenti_by_year, file = "count/docenti_by_year.RData")

docenti_by_year_region <-
  docente_ruolo_mysql_table %>%
  dplyr::group_by(anno, nome_regione, regione_id) %>%
  dplyr::summarize(docenti = n())

# dbWriteTable(con, value = as.data.frame(docenti_by_year_region), 
#              name = "count_docenti_by_year_region", append = TRUE, row.names=0)
save(docenti_by_year_region, file = "count/docenti_by_year_region.RData")

docenti_by_year_ateneo <-
  docente_ruolo_mysql_table %>%
  dplyr::group_by(anno, wikidata_label, ateneo_id) %>%
  dplyr::summarize(docenti = n())

# dbWriteTable(con, value = as.data.frame(docenti_by_year_ateneo), 
#              name = "count_docenti_by_year_ateneo", append = TRUE, row.names=0)
save(docenti_by_year_ateneo, file = "count/docenti_by_year_ateneo.RData")

docenti_by_year_facolta <-
  docente_ruolo_mysql_table %>%
  dplyr::group_by(anno, wikidata_label, ateneo_id, facolta) %>%
  dplyr::summarize(docenti = n())
docenti_by_year_facolta <- docenti_by_year_facolta[docenti_by_year_facolta$facolta !="" ,]

# dbWriteTable(con, value = as.data.frame(docenti_by_year_facolta), 
#              name = "count_docenti_by_year_facolta", append = TRUE, row.names=0)
save(docenti_by_year_facolta, file = "count/docenti_by_year_facolta.RData")

docenti_by_year_dipartimento <-
  docente_ruolo_mysql_table %>%
  dplyr::group_by(anno, wikidata_label, ateneo_id, dipartimento) %>%
  dplyr::summarize(docenti = n())
docenti_by_year_dipartimento <- 
  docenti_by_year_dipartimento[docenti_by_year_dipartimento$dipartimento!="Dip. Non disponibile",]

# dbWriteTable(con, value = as.data.frame(docenti_by_year_dipartimento), 
#              name = "count_docenti_by_year_dipartimento", append = TRUE, row.names=0)
save(docenti_by_year_dipartimento, file = "count/docenti_by_year_dipartimento.RData")

# Summary for simulation

surname_by_region <-
  docente_ruolo_mysql_table %>%
  dplyr::group_by(anno, cognome, nome_regione, regione_id, popolazione) %>%
  dplyr::summarize(docenti_wt_surname = n())
surname_by_region <-
  surname_by_region %>%
  dplyr::group_by(regione_id, anno) %>%
  dplyr::mutate(unitpop = sum(docenti_wt_surname))
surname_by_region <- subset(surname_by_region, docenti_wt_surname > 1)
nrow(surname_by_region)
surname_by_region <- plyr::rename(surname_by_region, c("popolazione" = "largepop"))
save(surname_by_region, file = "count/surname_by_region.RData")
# [1] 136408
  
surname_by_ateneo <-
  docente_ruolo_mysql_table %>%
  dplyr::group_by(anno, cognome, ateneo_id, wikidata_label, 
                  nome_regione, regione_id, popolazione) %>%
  dplyr::summarize(docenti_wt_surname = n())
surname_by_ateneo <-
  surname_by_ateneo %>%
  dplyr::group_by(ateneo_id, anno) %>%
  dplyr::mutate(unitpop = sum(docenti_wt_surname))
surname_by_ateneo <- subset(surname_by_ateneo, docenti_wt_surname > 1)
nrow(surname_by_ateneo)
# [1] 94777
surname_by_ateneo <- plyr::rename(surname_by_ateneo, c("popolazione" = "largepop",
                                                       "wikidata_label" = "nome_ateneo"))
save(surname_by_ateneo, file = "count/surname_by_ateneo.RData")


surname_by_facolta <-
  docente_ruolo_mysql_table[docente_ruolo_mysql_table$facolta!="",] %>%
  dplyr::group_by(anno, cognome, ateneo_id, wikidata_label, 
                  nome_regione, regione_id, popolazione, facolta) %>%
  dplyr::summarize(docenti_wt_surname = n())
surname_by_facolta <-
  surname_by_facolta %>%
  dplyr::group_by(facolta, ateneo_id, anno) %>%
  dplyr::mutate(unitpop = sum(docenti_wt_surname))
surname_by_facolta <- subset(surname_by_facolta, docenti_wt_surname > 1)
nrow(surname_by_facolta)
# [1] 25744
surname_by_facolta <- plyr::rename(surname_by_facolta, c("popolazione" = "largepop",
                                                       "wikidata_label" = "nome_ateneo"))
save(surname_by_facolta, file = "count/surname_by_facolta.RData")

surname_by_dipartimento <-
  docente_ruolo_mysql_table[docente_ruolo_mysql_table$dipartimento!="Dip. Non disponibile",] %>%
  dplyr::group_by(anno, cognome, ateneo_id, wikidata_label, 
                  nome_regione, regione_id, popolazione, dipartimento) %>%
  dplyr::summarize(docenti_wt_surname = n())
surname_by_dipartimento <-
  surname_by_dipartimento %>%
  dplyr::group_by(dipartimento, ateneo_id, anno) %>%
  dplyr::mutate(unitpop = n())
surname_by_dipartimento <- subset(surname_by_dipartimento, docenti_wt_surname > 1)
nrow(surname_by_dipartimento)
# [1] 11175
surname_by_dipartimento <- plyr::rename(surname_by_dipartimento, c("popolazione" = "largepop",
                                                         "wikidata_label" = "nome_ateneo"))
save(surname_by_dipartimento, file = "count/surname_by_dipartimento.RData")

dbDisconnect(con)