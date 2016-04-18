require(plyr)
require(data.table)

setwd("~/ownCloud/docenti_univ_ita")

load("count/surname_by_region.RData")
load("count/surname_by_ateneo.RData")
load("count/surname_by_facolta.RData")
load("count/surname_by_dipartimento.RData")

surname_by_region <- data.table(surname_by_region)
surname_by_facolta <- data.table(surname_by_facolta)
surname_by_dipartimento <- data.table(surname_by_dipartimento)

keycols <- c("regione_id", "cognome")

setkeyv(surname_by_region, keycols)
setkeyv(surname_by_ateneo, keycols)
setkeyv(surname_by_facolta, keycols)
setkeyv(surname_by_dipartimento, keycols)

load("~/ownCloud/docenti_univ_ita/surname_per_region.RData")
# Process paginebianche data
surname_per_region <-  subset(surname_per_region, region != "1")
keyvalue <-
  c("Lazio" = "wd:Q1282",
    "Emilia Romagna" = "wd:Q1263",
    "Marche" = "wd:Q1279",
    "Toscana" = "wd:Q1273",
    "Lombardia" = "wd:Q1210",
    "Puglia" = "wd:Q1447",
    "Abruzzo" = "wd:Q1284",
    "Umbria" = "wd:Q1280",
    "Piemonte" = "wd:Q1216",
    "Veneto" = "wd:Q1243",
    "Trentino Alto Adige" = "wd:Q1237",
    "Liguria" = "wd:Q1256",
    "Campania" = "wd:Q1438",
    "Friuli Venezia Giulia" = "wd:Q1250",
    "Sicilia" = "wd:Q1460",
    "Sardegna" = "wd:Q1462",
    "Valle D'Aosta" = "wd:Q1222",
    "Calabria" = "wd:Q1458",
    "Molise" = "wd:Q1443",
    "Basilicata"  = "wd:Q1452")
surname_per_region$regione_id <- keyvalue[surname_per_region$region]
surname_per_region <- plyr::rename(surname_per_region, c("surname" = "cognome"))
surname_per_region$cognome <- toupper(surname_per_region$cognome)
surname_per_region <- merge(surname_per_region, 
                            read.csv(file="hh_mean_size_by_region.csv"),
                            by = "regione_id")
surname_per_region <- plyr::rename(surname_per_region, c("n" = "hh_wt_surname"))
surname_per_region$hh_wt_surname <- as.numeric(surname_per_region$hh_wt_surname)

surname_per_region <- merge(surname_per_region, 
                            read.csv(file="hh_wt_fixline.csv")[,c("regione_id","perc_hh_wt_fixline")],
                            by = "regione_id")
source("06a_hh_fixline_decline_prediction.R")
surname_per_region$perc_hh_wt_fixline <- 
  surname_per_region$perc_hh_wt_fixline * hh_perc_correction

surname_per_region <- data.table(surname_per_region)
setkeyv(surname_per_region, keycols)

surname_per_region <- 
  surname_per_region[,.(regione_id, cognome, hh_wt_surname, hh_mean_size, perc_hh_wt_fixline)]

surname_by_region <- merge(surname_by_region, surname_per_region)
surname_by_ateneo <- merge(surname_by_ateneo, surname_per_region)
surname_by_facolta <- merge(surname_by_facolta, surname_per_region)
surname_by_dipartimento <- merge(surname_by_dipartimento, surname_per_region)

# Simulation
surname_by_list <- 
  list(region = surname_by_region,
       ateneo = surname_by_ateneo,
       facolta = surname_by_facolta,
       dipartimento = surname_by_dipartimento)

source("fun/runSimulation.R")

simulation_long_list <- list()

for (unit in names(surname_by_list)) {
  print(unit)
  tmp_df <- surname_by_list[[unit]]
  if (unit == "region") {
    unit_id <- "regione_id"
    ateneo_id_vec <- rep(NA, nrow(surname_by_list[[unit]]))
  } else if (unit == "ateneo") {
    unit_id <- "ateneo_id"
    ateneo_id_vec <- surname_by_list[[unit]][['ateneo_id']]
  } else {
    unit_id <- unit
    ateneo_id_vec <- surname_by_list[[unit]][['ateneo_id']]
  }
  
  
  simulation_long_list <- 
    c(simulation_long_list,
      mapply(runSimulation, 
             tmp_df$hh_mean_size,
             tmp_df$perc_hh_wt_fixline,
             tmp_df$hh_wt_surname,
             tmp_df$largepop, 
             tmp_df$docenti_wt_surname,
             tmp_df$unitpop,
             tmp_df$cognome,
             ateneo_id_vec,
             tmp_df[[unit_id]],
             rep(unit, nrow(tmp_df)),
             tmp_df$regione_id,
             tmp_df$anno,
             SIMPLIFY = FALSE))  
}

simulation_long_df <- 
  do.call(rbind.data.frame, simulation_long_list)

simulation_long_df$sim_id <- 1:nrow(simulation_long_df)

source('00_connect_to_db.R') # not in repo

dbWriteTable(con, value = simulation_long_df, 
             name = "simulation_long_df", append = TRUE, row.names=0)

dbDisconnect(con)

save(simulation_long_df, file = "sim/simulation_df.RData")
