setwd("~/ownCloud/docenti_univ_ita")

load("surname_per_region_processed.RData")
load("regioni_spatial_df.RData")
load("regione_mysql_table.RData")

spatial_df <- 
  merge(italy_map, surname_per_region, 
        by.x = 'wid', by.y="regione_id", all.x = TRUE, all.y = TRUE)

# Add tot population
spatial_df <- 
  merge(spatial_df, 
        regione_mysql_table[,c('wikidata_id','popolazione,')],
        by.x = "wid", by.y = "wikidata_id")
spatial_df$pop_wt_surname <- 
  (spatial_df$hh_wt_surname * spatial_df$hh_mean_size) / spatial_df$perc_hh_wt_fixline
spatial_df$pop_wt_surname[is.na(spatial_df$pop_wt_surname)] <- 0
spatial_df$pop_wt_surname_perc <-
  spatial_df$pop_wt_surname / spatial_df$popolazione * 100