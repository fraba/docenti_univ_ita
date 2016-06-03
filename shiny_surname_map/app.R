library(shiny)
library(leaflet)
require(viridis)

`%then%` <- shiny:::`%OR%`

ui <- shinyUI(fluidPage(
  
  # Application title
  # titlePanel("Surname distribution"),
  
  # Sidebar with a slider input for number of bins 
  
  # Show a plot of the generated distribution
  mainPanel(
    leafletOutput("surname_dist", height = 600)
  ),
  sidebarPanel(
    selectizeInput("surnameinput", label = "Search a surname",
                   choices = NULL,
                   options = list(create = TRUE)),
    textOutput("totsurname"),
    textOutput("tothh")
  ),
  fluidRow(tags$div(id='footer',
                    style='font-size: 70%; margin: auto; width: 90%; padding: 250px 0px 0px 0px;',
                    HTML(paste0(
                      "<p>Data: <a href='http://www.paginebianche.it/contacognome' target='_blank'>Pagine Bianche</a> 2016 ",
                      "| Design: Fracesco Bailo (<a href='https://twitter.com/FrBailo' target='_blank'>@FrBailo</a>) ",
                      "| Code: <a href='https://github.com/fraba/docenti_univ_ita/tree/master/shiny_surname_map' target='_blank'>GitHub</a> ",
                      "| Powered by: <a href='http://www.R-project.org/' target='_blank'>R</a> and <a href='http://shiny.rstudio.com/' target='_blank'>Shiny</a> ",
                      "| R packages: <a href='https://CRAN.R-project.org/package=DT' target='_blank'>DT</a>, <a href='https://CRAN.R-project.org/package=leaflet' target='_blank'>leaflet</a>, <a href='https://CRAN.R-project.org/package=shinyjs' target='_blank'>shinyjs</a>, <a href='http://ggplot2.org' target='_blank'>ggplot2</a>, <a href='scales' target='_blank'>scales</a>, <a href='https://CRAN.R-project.org/package=viridis'>viridis</a> ",
                      "| Version: 0.9 "))))
)
)

server <- shinyServer(function(input, output, session) {
  
  # Data prep
  
  setwd("~/ownCloud/docenti_univ_ita")
  
  load("surname_per_region_processed.RData")
  load("regioni_spatial_df.RData")
  
  source('00_connect_to_db.R') # not in repo
  # # require(RMySQL)
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
  
  regione_mysql_table <- dbReadTable(con, "regione")
  regione_mysql_table$popolazione <- as.numeric(regione_mysql_table$popolazione)
  
  dbDisconnect(con)
  
  accentLastLetter <- function(x) {
    x <- tolower(x)
    x <- gsub("a'", "à", x)
    x <- gsub("e'", "é", x)
    x <- gsub("o'", "ò", x)
    x <- gsub("u'", "ù", x)
    x <- gsub("i'", "ì", x)
    return(toupper(x))
  }
  
  apostropheLastLetter <- function(x) {
    x <- tolower(x)
    x <- gsub("à|á", "a'", x)
    x <- gsub("é|è", "e'", x)
    x <- gsub("ò|ò", "o'", x)
    x <- gsub("ù|ú", "u'", x)
    x <- gsub("ì|í", "i'", x)
    return(toupper(x))
  }
  
  updateSelectizeInput(session, 'surnameinput', 
                       choices = accentLastLetter(unique(surname_per_region$cognome)[order(unique(surname_per_region$cognome))]),
                       selected = 'ROSSI', server = TRUE)  
  
  output$totsurname <- renderText({paste("Surnames: ", 
                                         format(length(unique(surname_per_region$cognome)), big.mark = ","))})
  output$tothh <- renderText({paste("Households: ", 
                                    format(sum(surname_per_region$hh_wt_surname), big.mark = ","))})
  
  output$surname_dist <- renderLeaflet({
    
    validate(
      need(input$surnameinput !="", 'Enter a surname') %then%
      need(apostropheLastLetter(input$surnameinput) %in% surname_per_region$cognome,
           'No data on this surname')
    )
    
    spatial_df <- 
      merge(italy_map, subset(surname_per_region, cognome == apostropheLastLetter(input$surnameinput)), 
            by.x = 'wid', by.y="regione_id", all.x = TRUE)
    
    # Add tot population
    spatial_df <- 
      merge(spatial_df, 
            regione_mysql_table[,c('wikidata_id','popolazione')],
            by.x = "wid", by.y = "wikidata_id")
    spatial_df$pop_wt_surname <- 
      (spatial_df$hh_wt_surname * spatial_df$hh_mean_size) / spatial_df$perc_hh_wt_fixline
    spatial_df$pop_wt_surname[is.na(spatial_df$pop_wt_surname)] <- 0
    spatial_df$pop_wt_surname_perc <-
      spatial_df$pop_wt_surname / spatial_df$popolazione * 100
    
    pal <- colorNumeric(
      palette = viridis(12),
      domain = spatial_df$pop_wt_surname_perc
    )
    format_num <- function(x) format(round(x,digits=0), big.mark=",", scientific=FALSE)
    
    leaflet(spatial_df) %>%
      setView(lng = 12.5674, lat = 42, zoom = 5.8) %>%
      addPolygons(stroke = TRUE,  color = 'black', weight = 2, smoothFactor = 0.2, fillOpacity = 1,
                  fillColor = ~pal(pop_wt_surname_perc),
                  popup = ~paste0("<b>",NOME, ": ", round(pop_wt_surname_perc,digits=3),"%</b><br>Population: ", 
                                  format_num(popolazione), "<br>",
                                  accentLastLetter(input$surnameinput),"s: ", format_num(pop_wt_surname), " (est.)")) %>%
      addLegend("bottomleft", pal = pal, values = ~(pop_wt_surname_perc),
                title = paste0("Percentage of ", accentLastLetter(input$surnameinput)),
                opacity = 1,
                labFormat = labelFormat(digits=8, 
                                        suffix = "%"))
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

