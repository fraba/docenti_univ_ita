require(RSelenium)

setwd("~/ownCloud/docenti_univ_ita")
output_file <- "cineca_data_store.RData"

# DATA STORE
if (file.exists(output_file)) {
  load(output_file)
} else {
  data_store <- list(data = data.frame(), 
                     log = data.frame(period = character(),
                                      letter = character()))
}

RSelenium::startServer()
remDr <- remoteDriver()
remDr$open()
base_url <- 'http://cercauniversita.cineca.it/php5/docenti/cerca.php'

periods <- c("ad oggi")

for (y in 2000:2015) {
  periods <- c(periods, paste0("al 31/12/", as.character(y)))
}


# PERIOD LOOP beginning
for (p in periods) {
  
  # LETTER LOOP beginning
  for (l in LETTERS){
    
    require(plyr)
    if (nrow(match_df(data_store$log, data.frame(period = p, letter = l),  on = c("period", "letter"))) > 0) {
      print("Data already parsed")
      next
    }
    print(paste0(p, ": ", l))
    
    remDr$navigate(base_url)
    
    webElem <- remDr$findElement(using = 'name', 'cognome')
    webElem$sendKeysToElement(list(l))
    
    webElem <- remDr$findElement(using = 'id', 'cognome-inizio')
    webElem$clickElement()
    
    webElem <- remDr$findElement(using = 'id', 'situazione_al')
    webElem$sendKeysToElement(list(p))
    
    webElem <- remDr$findElement(using = 'id', 'vai')
    webElem$clickElement()
    
    webElem <- remDr$findElement(using =  "xpath", "//*[@id='frmformsubmit']/input")
    webElem$clickElement()
    
    Sys.sleep(15)
    
    html_doc <- htmlParse('RICERCADOCENTI.xls', encoding = "UTF-8")
    tmp_df <- readHTMLTable(html_doc, stringsAsFactors = FALSE)[[1]]
    tmp_df$period <- p
    data_store$data <- rbind.fill(data_store$data, tmp_df)
    data_store$log <- rbind(data_store$log, 
                            data.frame(period = p,
                                       letter = l))
    save(data_store, file = output_file)
    
    file.remove('RICERCADOCENTI.xls')
    
    Sys.sleep(10)
    
  # LETTER LOOP end
  } 
  
# PERIOD LOOP end
}

remDr$close()
remDr$closeServer()
