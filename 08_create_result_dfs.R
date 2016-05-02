require(dplyr)

setwd("~/ownCloud/docenti_univ_ita")
load("sim/simulation_df.RData")
load("count/docenti_by_year_region.RData")
load("count/docenti_by_year_ateneo.RData")
load("count/docenti_by_year_dipartimento.RData")
load("count/docenti_by_year_facolta.RData")
load("ateneo_mysql_table.RData")
docenti_by_year_ateneo <- merge(docenti_by_year_ateneo, 
                                ateneo_mysql_table[,c("wikidata_id","regione_id")],
                                by.x = 'ateneo_id',
                                by.y = 'wikidata_id')
docenti_by_year_facolta <- merge(docenti_by_year_facolta, 
                                ateneo_mysql_table[,c("wikidata_id","regione_id")],
                                by.x = 'ateneo_id',
                                by.y = 'wikidata_id')
docenti_by_year_dipartimento <- merge(docenti_by_year_dipartimento, 
                                 ateneo_mysql_table[,c("wikidata_id","regione_id")],
                                 by.x = 'ateneo_id',
                                 by.y = 'wikidata_id')

simulation_long_df$suspect <- ifelse(simulation_long_df$p_of_observing < 0.05, 
                                     TRUE, FALSE)
units <- c("region","ateneo","facolta","dipartimento")

result_lists <- list()

require(dplyr)
for (a_level in units){
  result_lists[[a_level]] <- list()
  if (a_level == "region") {
    for (u_level in units) {
      if (u_level == 'region') {
        tmp_df <- 
          subset(simulation_long_df, unit_level %in% u_level) %>%
          dplyr::group_by(region, year) %>%
          dplyr::summarize(suspect_n = sum(docenti_wt_surname*suspect))
        tmp_df <- merge(tmp_df, 
                        docenti_by_year_region[,c("anno","regione_id","docenti")], 
                        by.x = c("region","year"),
                        by.y = c("regione_id","anno"),
                        all.y = TRUE)
        names(tmp_df)[names(tmp_df) == 'docenti'] <- 'n'
      } else if(u_level == 'ateneo') {
        tmp_df <- 
          subset(simulation_long_df, unit_level %in% u_level) %>%
          dplyr::group_by(region, year, ateneo) %>%
          dplyr::summarize(suspect_n = sum(docenti_wt_surname*suspect))
        tmp_df <- merge(tmp_df, 
                        docenti_by_year_ateneo[,c("anno","ateneo_id","docenti","regione_id")], 
                        by.x = c("ateneo","region","year"),
                        by.y = c("ateneo_id","regione_id","anno"),
                        all.y = TRUE)
        names(tmp_df)[names(tmp_df) == 'docenti'] <- 'n'
        tmp_df$suspect_n[is.na(tmp_df$suspect_n)] <- 0
        tmp_df <-
          tmp_df %>%
          dplyr::group_by(region, year) %>%
          dplyr::summarize(n = mean(n),
                           suspect_n = mean(suspect_n))
      } else if(u_level == 'facolta') {
        tmp_df <- 
          subset(simulation_long_df, unit_level %in% u_level) %>%
          dplyr::group_by(region, year, ateneo, unit) %>%
          dplyr::summarize(suspect_n = sum(docenti_wt_surname*suspect))
        tmp_df <- merge(tmp_df, 
                        docenti_by_year_facolta[,c("anno","ateneo_id","docenti","regione_id","facolta")], 
                        by.x = c("ateneo","region","year","unit"),
                        by.y = c("ateneo_id","regione_id","anno","facolta"),
                        all.y = TRUE)
        names(tmp_df)[names(tmp_df) == 'docenti'] <- 'n'
        tmp_df$suspect_n[is.na(tmp_df$suspect_n)] <- 0
        tmp_df <-
          tmp_df %>%
          dplyr::group_by(region, year) %>%
          dplyr::summarize(n = mean(n),
                           suspect_n = mean(suspect_n))
      } else if(u_level == 'dipartimento') {
        tmp_df <- 
          subset(simulation_long_df, unit_level %in% u_level) %>%
          dplyr::group_by(region, year, ateneo, unit) %>%
          dplyr::summarize(suspect_n = sum(docenti_wt_surname*suspect))
        tmp_df <- merge(tmp_df, 
                        docenti_by_year_dipartimento[,c("anno","ateneo_id","docenti","regione_id","dipartimento")], 
                        by.x = c("ateneo","region","year","unit"),
                        by.y = c("ateneo_id","regione_id","anno","dipartimento"),
                        all.y = TRUE)
        names(tmp_df)[names(tmp_df) == 'docenti'] <- 'n'
        tmp_df$suspect_n[is.na(tmp_df$suspect_n)] <- 0
        tmp_df <-
          tmp_df %>%
          dplyr::group_by(region, year) %>%
          dplyr::summarize(n = mean(n),
                           suspect_n = mean(suspect_n))
      }
      tmp_df$ratio <- tmp_df$suspect_n / tmp_df$n
      result_lists[[a_level]][[u_level]] <- tmp_df
    }
  } else if (a_level == 'ateneo') {
    for (u_level in units[2:4]) {
      if (u_level == 'ateneo') {
        tmp_df <- 
          subset(simulation_long_df, unit_level %in% u_level) %>%
          dplyr::group_by(region, year, ateneo) %>%
          dplyr::summarize(suspect_n = sum(docenti_wt_surname*suspect))
        tmp_df <- merge(tmp_df, 
                        docenti_by_year_ateneo[,c("anno","ateneo_id","docenti","regione_id")], 
                        by.x = c("ateneo","region","year"),
                        by.y = c("ateneo_id","regione_id","anno"),
                        all.y = TRUE)
        names(tmp_df)[names(tmp_df) == 'docenti'] <- 'n'
        tmp_df$suspect_n[is.na(tmp_df$suspect_n)] <- 0
        tmp_df <- merge(tmp_df, 
                        ateneo_mysql_table[c("wikidata_id","wikidata_label",
                                             "lon","lat")],
                        by.x = "ateneo",
                        by.y = "wikidata_id")
      } else if (u_level == 'dipartimento') {
        tmp_df <- 
          subset(simulation_long_df, unit_level %in% u_level) %>%
          dplyr::group_by(region, year, ateneo, unit) %>%
          dplyr::summarize(suspect_n = sum(docenti_wt_surname*suspect))
        tmp_df <- merge(tmp_df, 
                        docenti_by_year_dipartimento[,c("anno","ateneo_id","dipartimento","docenti","regione_id")], 
                        by.x = c("ateneo","region","year","unit"),
                        by.y = c("ateneo_id","regione_id","anno","dipartimento"),
                        all.y = TRUE)
        names(tmp_df)[names(tmp_df) == 'docenti'] <- 'n'
        tmp_df$suspect_n[is.na(tmp_df$suspect_n)] <- 0
        tmp_df <- merge(tmp_df, 
                        ateneo_mysql_table[c("wikidata_id","wikidata_label",
                                             "lon","lat")],
                        by.x = "ateneo",
                        by.y = "wikidata_id")
        tmp_df <-
          tmp_df %>%
          dplyr::group_by(ateneo, year, lon, lat, wikidata_label) %>%
          dplyr::summarize(n = mean(n),
                           suspect_n = mean(suspect_n))
      } else if (u_level == 'facolta') {
        tmp_df <- 
          subset(simulation_long_df, unit_level %in% u_level) %>%
          dplyr::group_by(region, year, ateneo, unit) %>%
          dplyr::summarize(suspect_n = sum(docenti_wt_surname*suspect))
        tmp_df <- merge(tmp_df, 
                        docenti_by_year_facolta[,c("anno","ateneo_id","facolta","docenti","regione_id")], 
                        by.x = c("ateneo","region","year","unit"),
                        by.y = c("ateneo_id","regione_id","anno","facolta"),
                        all.y = TRUE)
        names(tmp_df)[names(tmp_df) == 'docenti'] <- 'n'
        tmp_df$suspect_n[is.na(tmp_df$suspect_n)] <- 0
        tmp_df <- merge(tmp_df, 
                        ateneo_mysql_table[c("wikidata_id","wikidata_label",
                                             "lon","lat")],
                        by.x = "ateneo",
                        by.y = "wikidata_id")
        tmp_df <-
          tmp_df %>%
          dplyr::group_by(ateneo, year, lon, lat, wikidata_label) %>%
          dplyr::summarize(n = mean(n),
                           suspect_n = mean(suspect_n))
      } else {
        stop("u_level not accepted")
      }
      tmp_df$ratio <- tmp_df$suspect_n / tmp_df$n
      result_lists[[a_level]][[u_level]] <- tmp_df
    }
  } else if (a_level == 'facolta') {
    tmp_df <- 
      subset(simulation_long_df, unit_level %in% a_level) %>%
      dplyr::group_by(region, year, ateneo, unit) %>%
      dplyr::summarize(suspect_n = sum(docenti_wt_surname*suspect))
    tmp_df <- merge(tmp_df, 
                    docenti_by_year_facolta[,c("anno","ateneo_id","facolta","docenti","regione_id")], 
                    by.x = c("ateneo","region","year","unit"),
                    by.y = c("ateneo_id","regione_id","anno","facolta"),
                    all.y = TRUE)
    names(tmp_df)[names(tmp_df) == 'docenti'] <- 'n'
    tmp_df$suspect_n[is.na(tmp_df$suspect_n)] <- 0
    tmp_df <- merge(tmp_df, 
                    ateneo_mysql_table[c("wikidata_id","wikidata_label",
                                         "lon","lat")],
                    by.x = "ateneo",
                    by.y = "wikidata_id")
    tmp_df$ratio <- tmp_df$suspect_n / tmp_df$n
    result_lists[[a_level]][[a_level]] <- tmp_df
  } else if (a_level == 'dipartimento') {
    tmp_df <- 
      subset(simulation_long_df, unit_level %in% a_level) %>%
      dplyr::group_by(region, year, ateneo, unit) %>%
      dplyr::summarize(suspect_n = sum(docenti_wt_surname*suspect))
    tmp_df <- merge(tmp_df, 
                    docenti_by_year_dipartimento[,c("anno","ateneo_id","dipartimento","docenti","regione_id")], 
                    by.x = c("ateneo","region","year","unit"),
                    by.y = c("ateneo_id","regione_id","anno","dipartimento"),
                    all.y = TRUE)
    names(tmp_df)[names(tmp_df) == 'docenti'] <- 'n'
    tmp_df$suspect_n[is.na(tmp_df$suspect_n)] <- 0
    tmp_df <- merge(tmp_df, 
                    ateneo_mysql_table[c("wikidata_id","wikidata_label",
                                         "lon","lat")],
                    by.x = "ateneo",
                    by.y = "wikidata_id")
    tmp_df$ratio <- tmp_df$suspect_n / tmp_df$n
    result_lists[[a_level]][[a_level]] <- tmp_df
  }
}

save(result_lists, file = "count/result_list.RData")
