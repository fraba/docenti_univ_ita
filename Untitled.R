base_url <- 'http://www.agcom.it/pluralismo-politico-sociale-in-televisione'

require(RSelenium)
require(stringr)

it_months <- setNames(formatC(1:12, width=2, digits=2, flag=0), 
                      c('gennaio','febbraio','marzo','aprile','maggio','giugno',
                        'luglio','agosto','settembre','ottobre','novembre','dicembre'))

RSelenium::startServer()
remDr <- remoteDriver()
remDr$open()
remDr$navigate(base_url)

option <- 
  remDr$findElement(using = 'xpath',
                    '//*[@id="numpageris"]/option[3]')
option$clickElement()

# PAGE loop
page_links <- 
  remDr$findElements(using = 'xpath', 
                    '//a[contains(text(),"pluralismo politico")]')
for (page_link in page_links) {
  
  element_text <- page_link$getElementText()[[1]]
  date_raw <- str_extract(element_text, "\\d{1,2} (.*) \\d{4}$")
  date_split <- strsplit(date_raw, " ")[[1]]
  date <- format(as.Date(paste(c(date_split[1], it_months[date_split[2]], date_split[3]), 
                collapse = " "), format = "%e %m %Y"), format = "%Y-%m%-%d")
  file_name <- paste0("agcom-", date, ".pdf")
  
  remDr$navigate(page_link$getElementAttribute('href')[[1]])
  a_to_pdf <-
    remDr$findElement(using = 'xpath', '//*[@id="left-column"]/div/h4/a')
  link_to_pdf <-
    a_to_pdf$getElementAttribute('href')[[1]]
  download.file(link_to_pdf, "~/Dropbox/Thesis_PhD/data/agcom/")
  remDr$goBack()
}




