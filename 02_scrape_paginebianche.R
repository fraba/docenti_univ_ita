require(R.utils)
require(rvest)

base_url <- 'http://www.paginebianche.it/contacognome?qs='

setwd("/mnt/ownCloud/docenti_univ_ita")

if (file.exists('surname_per_region.RData')) {
  print("Datafile already exists.")
  print("Loading it...")
  load("surname_per_region.RData")
} else {
  print("Creating new datafile")
  load("docente_ruolo_mysql_table.RData")
  
  # By ateneo
  require(dplyr)
  docenti_same_surname <-
    docente_ruolo_mysql_table %>%
    dplyr::group_by(ateneo_id, cognome, anno) %>%
    dplyr::summarize(n_surname = n())
  
  surnames_to_scrape <- tolower(subset(docenti_same_surname, n_surname > 1)$cognome)
  
  # By regione
  load("ateneo_mysql_table.RData")
  docente_ruolo_mysql_table <- merge(docente_ruolo_mysql_table, 
                                     ateneo_mysql_table,
                                     by.x = 'ateneo_id',
                                     by.y = 'wikidata_id')
  
  docenti_same_surname <-
    docente_ruolo_mysql_table %>%
    dplyr::group_by(regione_id, cognome, anno) %>%
    dplyr::summarize(n_surname = n())
  
  surnames_to_scrape <- c(surnames_to_scrape, 
                          tolower(subset(docenti_same_surname, n_surname > 1)$cognome))
  
  surnames_to_scrape <- unique(surnames_to_scrape)
  
  surname_per_region <- data.frame(surname = character(),
                                   region = character(), 
                                   n = numeric())
}

accentLastLetter <- function(x) {
  x <- gsub("a'", "à", x)
  x <- gsub("e'", "é", x)
  x <- gsub("o'", "ò", x)
  x <- gsub("u'", "ù", x)
  x <- gsub("i'", "ì", x)
  return(x)
}

replaceSpace <- function(x) {
  x <- gsub(" ", "%20", x)
  return(x)
}

for (s in surnames_to_scrape[!(surnames_to_scrape %in% surname_per_region$surname)]) {
  print(accentLastLetter(s))
  
  res <- NULL;
  res <- tryCatch({
    withTimeout({
      print("Requesting webpage...")
      read_html(paste0(base_url, replaceSpace(accentLastLetter(s))))
    }, timeout=30)
  }, TimeoutException=function(ex) {
    cat("Server is taking to long. Moving to next surname in a while...")
    return(NA)
  })
  
  if (length(res) < 2 | is.null(res)) {
    Sys.sleep(sample(30:60, 1))
    next
    }
  
  results <- 
    res %>%
    html_nodes(xpath = "//*[@id='container']/div[5]/div/div/div[1]/div[3]/div[2]/ul") %>%
    html_text()
  if (length(results) == 0) {
    print(paste0("Nothing parsed from the page with surname ", s, "..."))
    Sys.sleep(sample(30:60, 1))
    next
  }
  results <- gsub("\n\\s+", ";", results)
  results <- gsub("^;|;$", "", results)
  results <- strsplit(results, ";")[[1]]
  require(stringr)
  tmp_df <- data.frame(surname =s,
                       region = gsub(" \\d+$" , "", results), 
                       n = str_extract(results, "\\d+"),
                       stringsAsFactors = FALSE)
  if (nrow(tmp_df) > 0) {
    surname_per_region <- rbind(surname_per_region, tmp_df)
  }
  save(surnames_to_scrape, surname_per_region, file = "surname_per_region.RData")
  print('    Sleeping...')
  Sys.sleep(sample(30:60, 1))
}


