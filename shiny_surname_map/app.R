library(shiny)
library(leaflet)
require(viridis)

`%then%` <- shiny:::`%OR%`

ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Surname distribution"),
  
  # Sidebar with a slider input for number of bins 
  
  # Show a plot of the generated distribution
  mainPanel(
    leafletOutput("surname_dist", height = 600)
  ),
  sidebarPanel(
    textInput("surnameinput", label = "Search a surname", value = "ROSSI")
  )
)
)

server <- shinyServer(function(input, output) {
  
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
      addPolygons(stroke = FALSE, , smoothFactor = 0.2, fillOpacity = 1,
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

