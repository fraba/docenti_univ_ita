#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(RColorBrewer)
library(htmltools)

# Define UI for application that draws a histogram
ui <- 
  shinyUI(navbarPage("",
                     tabPanel("Regions",
                              leafletOutput("region_map")),
                     tabPanel("Universities",
                              leafletOutput("university_map")),
                     tabPanel("Faculties",
                              leafletOutput("faculty_map")),
                     tabPanel("Departments",
                              leafletOutput("department_map"))
  ))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  base_url_region <- 
    "<a href = 'http://146.118.107.12/univapp/index.php?unit_level=region&unit="
  base_url_university <- 
    "<a href = 'http://146.118.107.12/univapp/index.php?unit_level=ateneo&unit="
  base_url_department <- 
    "<a href = 'http://146.118.107.12/univapp/index.php?unit_level=dipartimento&unit="
  base_url_facolta <- 
    "<a href = 'http://146.118.107.12/univapp/index.php?unit_level=facolta&unit="
  
  htmlEncodeAccents <- function(str) {
    str <- gsub("à", "&agrave;", str)
    return(str)
  }
  
  output$region_map <- renderLeaflet({
    leaflet(italy_map) %>%
      setView(lng = 12.5674, lat = 42, zoom = 5) %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addPolygons(stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,,
                  color = ~color, 
                  popup=~(paste0(base_url_region,
                                 wid,"'>",
                                 "go to <b>", NOME, "</b>", 
                                 "</a>")))
  })
  
  output$university_map <- renderLeaflet({
    leaflet(ateneo_coord_df) %>%
      setView(lng = 12.5674, lat = 42, zoom = 5) %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(lng = ~lon, lat = ~lat, 
                 popup=~(paste0(base_url_university,
                                id, "&ateneo=", id,"'>",
                                "go to <b>", name, "</b>", 
                                "</a>")))
  })
  
  output$department_map <- renderLeaflet({
    leaflet(department_coord_df) %>%
      setView(lng = 12.5674, lat = 42, zoom = 5) %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(lng = ~lon, lat = ~lat, 
                 clusterOptions = markerClusterOptions(),
                 popup=~(paste0(base_url_university,
                                dipartimento, "&ateneo=", ateneo_id,"'>",
                                "go to <b>", dipartimento, " (", wikidata_label, ")</b>", 
                                "</a>")))
  })
  
  output$faculty_map <- renderLeaflet({
    leaflet(faculty_coord_df) %>%
      setView(lng = 12.5674, lat = 42, zoom = 5) %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(lng = ~lon, lat = ~lat, 
                 clusterOptions = markerClusterOptions(),
                 popup=~(paste0(base_url_university,
                                facolta, "&ateneo=", ateneo_id,"'>",
                                "go to <b>", facolta, " (", wikidata_label, ")</b>", 
                                "</a>")))
  })
  
})

source('/Users/francesco/ownCloud/docenti_univ_ita/00_connect_to_db.R') # not in repo
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

require(sp)
load("/Users/francesco/ownCloud/docenti_univ_ita/regioni_spatial_df.RData")
italy_map$color <- 
  c(brewer.pal(9,"Pastel1"), brewer.pal(8,"Pastel1"), 
    brewer.pal(3,"Pastel1"))

q <- 'SELECT wikidata_id AS id, wikidata_label AS name, lat, lon FROM ateneo'
dbGetQuery(con, "SET NAMES 'utf8'")
response <- dbSendQuery(con, q)
ateneo_coord_df <- fetch(response, n = -1) 

q <- 'SELECT wikidata_id AS id, wikidata_label AS name, lat, lon FROM ateneo'
dbGetQuery(con, "SET NAMES 'utf8'")
response <- dbSendQuery(con, q)
ateneo_coord_df <- fetch(response, n = -1) 

q <- 'SELECT * FROM faculty_wt_coordinates'
dbGetQuery(con, "SET NAMES 'utf8'")
response <- dbSendQuery(con, q)
faculty_coord_df <- fetch(response, n = -1) 

q <- 'SELECT * FROM department_wt_coordinates'
dbGetQuery(con, "SET NAMES 'utf8'")
response <- dbSendQuery(con, q)
department_coord_df <- fetch(response, n = -1) 

dbDisconnect(con)

# Run the application 
shinyApp(ui = ui, server = server)

