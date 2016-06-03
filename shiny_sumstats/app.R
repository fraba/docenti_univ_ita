# MAIN APP

url_unit_app <- 'http://127.0.0.1:7359/'

library(shiny)
library(leaflet)
require(sp)
require(RColorBrewer)
require(viridis)
require(DT)
require(scales)
require(ggplot2)
require(ggrepel)

ui <- shinyUI(fluidPage(
  
  shinyUI(navbarPage("Surnames in Italian universities",
                     tabPanel("Data",
                              column(6,
                                     includeHTML("intro.html"))),
                     tabPanel("Region",
                              column(4,
                                     selectInput(
                                       "Region.year", label = NULL,
                                       choices = 2016:2000, selected = 2016),
                                     selectInput(
                                       'Region.unitlevel', label = NULL, 
                                       choices = c("Region" = "region", 
                                                   "University (mean)" = "ateneo", 
                                                   "Faculty (mean)" = "facolta",
                                                   "Department (mean)" = "dipartimento")),
                                     dataTableOutput('Region.table')),
                              column(8,
                                     leafletOutput("Region.map", height = 640))
                     ),
                     tabPanel("University",
                              column(4,
                                     selectInput(
                                       "University.year", label = NULL,
                                       choices = 2016:2000, selected = 2016),
                                     selectInput(
                                       'University.unitlevel', label = NULL, 
                                       choices = c("University" = "ateneo", 
                                                   "Faculty (mean)" = "facolta",
                                                   "Department (mean)" = "dipartimento")),
                                     dataTableOutput('University.table')),
                              column(8,
                                     leafletOutput("University.map", height = 640))),
                     tabPanel("Faculty",
                              column(4,
                                     selectInput(
                                       "Faculty.year", label = NULL,
                                       choices = 2016:2000, selected = 2016)),
                              column(8,
                                     dataTableOutput('Faculty.table'))),
                     tabPanel("Department",
                              column(4,
                                     selectInput(
                                       "Department.year", label = NULL,
                                       choices = 2016:2000, selected = 2016)),
                              column(8,
                                     dataTableOutput('Department.table'))),
                     tabPanel("Time analysis",
                              column(4,
                                     selectInput("Time.level", label = "Select 'region' or 'university'",
                                                 choices = setNames(c("regione", "ateneo"),
                                                                    c("Region", "University"))),
                                     uiOutput('Time.unit')
                              ),
                              column(8,
                                     plotOutput("Time.plot"))),
                     tabPanel("Geo analysis",
                              column(6,
                                     plotOutput("Geo.plot.university"),
                                     plotOutput("Geo.plot.faculty"),
                                     plotOutput("Geo.plot.department"))))),
  fluidRow(tags$div(id='footer',
                    style='font-size: 70%; text-align: center; width: 90%; padding: 10px;',
                    HTML(paste0(
                      "<p>Data: <a href='http://cercauniversita.cineca.it/' target='_blank'>CINECA</a> and <a href='http://www.paginebianche.it/contacognome' target='_blank'>Pagine Bianche</a> 2016 ",
                      "| Design: Fracesco Bailo (<a href='https://twitter.com/FrBailo' target='_blank'>@FrBailo</a>) ",
                      "| Code: <a href='https://github.com/fraba/docenti_univ_ita/tree/master/shiny_unitpage' target='_blank'>GitHub</a> ",
                      "| Powered by: <a href='http://www.R-project.org/' target='_blank'>R</a> and <a href='http://shiny.rstudio.com/' target='_blank'>Shiny</a> ",
                      "| R packages: <a href='https://CRAN.R-project.org/package=DT' target='_blank'>DT</a>, <a href='https://CRAN.R-project.org/package=leaflet' target='_blank'>leaflet</a>, <a href='https://CRAN.R-project.org/package=shinyjs' target='_blank'>shinyjs</a>, <a href='http://ggplot2.org' target='_blank'>ggplot2</a>, <a href='scales' target='_blank'>scales</a>, <a href='https://CRAN.R-project.org/package=viridis'>viridis</a> ",
                      "| Version: 0.9 "))))
  
))

server <- shinyServer(function(input, output, session) {
  
  format_num <- function(x) format(round(x,digits=0), big.mark=",", scientific=FALSE)
  
  output$Region.table <- DT::renderDataTable({
    unitlevel <- input$Region.unitlevel
    yr <- input$Region.year
    data_df <- subset(result_lists[['region']][[unitlevel]],
                      year == yr)
    tbl <- merge(as.data.frame(italy_map), as.data.frame(data_df), by.x = 'wid', by.y = 'region',
                 all.x = TRUE)
    tbl$n[is.na(tbl$n)] <- 0
    tbl$ratio[is.na(tbl$ratio)] <- 0
    tbl$suspect_n[is.na(tbl$suspect_n)] <- 0
    
    tbl <- tbl[,c("NOME","n","suspect_n","ratio")]
    tbl$suspect_n <- round(tbl$suspect_n,0)
    tbl$n <- round(tbl$n,0)
    tbl$ratio <- round(tbl$ratio*100,1)
    names(tbl) <- c("Region", "Staff", "with suspect relations", "%")
    return(DT::datatable(tbl, rownames = FALSE, extensions = 'Scroller',
                         options = list(order = list(list(3, 'desc')),
                                        pageLength = 10, 
                                        searching = FALSE,
                                        deferRender = TRUE,
                                        scrollY = 420,
                                        scroller = TRUE,
                                        bInfo = FALSE,
                                        bLengthChange = FALSE,
                                        columnDefs = list(list(className = 'dt-center', targets = 1:3)))))
  })
  
  output$Region.map <- renderLeaflet({
    
    # Subset and merge
    unitlevel <- input$Region.unitlevel
    yr <- input$Region.year
    data_df <- subset(result_lists[['region']][[unitlevel]],
                      year == yr)
    italy_map <- sp::merge(italy_map, as.data.frame(data_df), by.x = 'wid', by.y = 'region',
                           all.x = TRUE)
    italy_map$ratio[is.na(italy_map$ratio)] <- 0
    italy_map$suspect_n[is.na(italy_map$suspect_n)] <- 0
    
    pal <- colorNumeric(
      palette = viridis(12),
      domain = italy_map$ratio*100
    )
    
    addConditionalText <- function(unitlevel) ifelse (unitlevel == 'region', "", " (mean)")
    
    leaflet(italy_map) %>%
      setView(lng = 12.5674, lat = 42, zoom = 6) %>%
      addProviderTiles("OpenStreetMap.BlackAndWhite",
                       options = providerTileOptions(noWrap = TRUE, opacity = 0.5)) %>%
      addPolygons(stroke = TRUE,  color = 'black', weight = 2, smoothFactor = 0.2, fillOpacity = 0.9, 
                  fillColor = ~pal(ratio*100),
                  popup = ~paste0("<b>",NOME, addConditionalText(unitlevel),": ", round(ratio*100, digits=1),"%</b><br>Staff",
                                  addConditionalText(unitlevel),": ",
                                  format_num(n), "<br>",
                                  "with suspect family relations",addConditionalText(unitlevel),": ",  format_num(suspect_n), "<br>",
                                  "<a href='",url_unit_app,"?unit_year=",yr,"&unit_level=region&unit_id=",wid,"'  target='_blank'>See data</a>")) %>%
      addLegend("topright", pal = pal, values = ~(ratio*100),
                title = "With suspect relations",
                opacity = 1,
                labFormat = labelFormat(digits=8,
                                        suffix = "%"))
  })
  
  output$University.table <- DT::renderDataTable({
    unitlevel <- input$University.unitlevel
    yr <- input$University.year
    tbl <- subset(result_lists[['ateneo']][[unitlevel]],
                  year == yr)
    tbl$n[is.na(tbl$n)] <- 0
    tbl$ratio[is.na(tbl$ratio)] <- 0
    tbl$suspect_n[is.na(tbl$suspect_n)] <- 0
    
    tbl <- tbl[,c("wikidata_label","n","suspect_n","ratio")]
    tbl$suspect_n <- round(tbl$suspect_n,0)
    tbl$n <- round(tbl$n,0)
    tbl$ratio <- round(tbl$ratio*100,1)
    names(tbl) <- c("University", "Staff", "with suspect relations", "%")
    return(DT::datatable(tbl, rownames = FALSE, extensions = 'Scroller',
                         options = list(order = list(list(3, 'desc')),
                                        pageLength = 20, 
                                        searching = TRUE,
                                        deferRender = TRUE,
                                        scrollY = 420,
                                        scroller = TRUE,
                                        bInfo = TRUE,
                                        bLengthChange = TRUE,
                                        columnDefs = list(list(className = 'dt-center', targets = 1:3)))))
  })
  
  output$University.map <- renderLeaflet({
    
    # Subset and merge
    unitlevel <- input$University.unitlevel
    yr <- input$University.year
    tbl <- subset(result_lists[['ateneo']][[unitlevel]],
                  year == yr)
    tbl$n[is.na(tbl$n)] <- 0
    tbl$ratio[is.na(tbl$ratio)] <- 0
    tbl$suspect_n[is.na(tbl$suspect_n)] <- 0
    tbl$suspect_n <- round(tbl$suspect_n,0)
    tbl$n <- round(tbl$n,0)
    tbl$size <- rescale(tbl$n, to = c(10,30))
    
    pal <- colorNumeric(
      palette = brewer.pal(9, "YlOrRd"),
      domain = tbl$ratio*100
    )
    
    addConditionalText <- function(unitlevel) ifelse (unitlevel == 'ateneo', "", " (mean)")
    
    leaflet(tbl) %>%
      setView(lng = 12.5674, lat = 42, zoom = 6) %>%
      addProviderTiles("OpenStreetMap.BlackAndWhite",
                       options = providerTileOptions(noWrap = TRUE, opacity = 0.2)) %>%
      clearShapes() %>%
      addCircleMarkers(radius = ~size, weight = 1, stroke = FALSE, color = "black", 
                       fillColor = ~pal(ratio*100), fillOpacity = 0.6,
                       popup = ~paste0("<b>", wikidata_label, addConditionalText(unitlevel),": ", round(ratio*100, digits=1),"%</b><br>Staff",
                                       addConditionalText(unitlevel),": ",
                                       format_num(n), "<br>",
                                       "with suspect family relations",addConditionalText(unitlevel),": ",  format_num(suspect_n), "<br>",
                                       "<a href='",url_unit_app,"?unit_year=",yr,"&unit_level=ateneo&unit_id=",ateneo,"'  target='_blank'>See data</a>"
                                       )) %>%
      addLegend("topright", pal = pal, values = ~(ratio*100),
                title = "With suspect relations",
                opacity = 1,
                labFormat = labelFormat(digits=8,
                                        suffix = "%"))
  })
  
  output$Faculty.table <- DT::renderDataTable({
    yr <- input$Faculty.year
    tbl <- subset(result_lists[['facolta']][['facolta']],
                  year == yr)
    tbl$n[is.na(tbl$n)] <- 0
    tbl$ratio[is.na(tbl$ratio)] <- 0
    tbl$suspect_n[is.na(tbl$suspect_n)] <- 0
    
    tbl <- tbl[,c("unit","facolta","wikidata_label","n","suspect_n","ratio")]
    tbl$suspect_n <- round(tbl$suspect_n,0)
    tbl$n <- round(tbl$n,0)
    tbl$ratio <- round(tbl$ratio*100,1)
    names(tbl) <- c("faculta_id","Faculty","University", "Staff", "with suspect relations", "%")
    return(DT::datatable(tbl, rownames = FALSE, extensions = 'Scroller',
                         options = list(order = list(list(4, 'desc')),
                                        pageLength = 20, 
                                        searching = TRUE,
                                        deferRender = TRUE,
                                        scrollY = 620,
                                        scroller = TRUE,
                                        bInfo = TRUE,
                                        bLengthChange = TRUE,
                                        columnDefs = list(list(visible=FALSE, targets=0),
                                                          list(className = 'dt-center', targets = 1:3)))))
  })
  
  output$Department.table <- DT::renderDataTable({
    yr.dep <- input$Department.year
    tbl <- subset(result_lists[['dipartimento']][['dipartimento']],
                  year == yr.dep)
    tbl$n[is.na(tbl$n)] <- 0
    tbl$ratio[is.na(tbl$ratio)] <- 0
    tbl$suspect_n[is.na(tbl$suspect_n)] <- 0
    
    tbl <- tbl[,c("unit","dipartimento","wikidata_label","n","suspect_n","ratio")]
    tbl$suspect_n <- round(tbl$suspect_n,0)
    tbl$n <- round(tbl$n,0)
    tbl$ratio <- round(tbl$ratio*100,1)
    names(tbl) <- c("dipartimento_id","Department", "University", "Staff", "with suspect relations", "%")
    tbl$Department <- iconv(tbl$Department, "utf-8", "ASCII", sub=" ")
    return(DT::datatable(tbl, rownames = FALSE, extensions = 'Scroller',
                         options = list(order = list(list(4, 'desc')),
                                        pageLength = 20, 
                                        searching = TRUE,
                                        deferRender = TRUE,
                                        scrollY = 620,
                                        scroller = TRUE,
                                        bInfo = TRUE,
                                        bLengthChange = TRUE,
                                        columnDefs = list(list(visible=FALSE, targets=0),
                                                          list(className = 'dt-center', targets = 1:3)))))
  })
  
  output$Time.unit <- renderUI({
    if (input$Time.level == "regione") {
      selectInput("Time.unit.region", label = "Select one region or more", 
                  choices  = setNames(as.character(italy_map$wid), 
                                      italy_map$NOME),
                  multiple = TRUE)
    } else if (input$Time.level == "ateneo") {
      selectizeInput("Time.unit.university", label = "Select one university or more",
                     choices  = setNames(ateneo_mysql_table$wikidata_id,
                                         ateneo_mysql_table$wikidata_label)[order(ateneo_mysql_table$wikidata_label)],
                     multiple = TRUE)
    }
  })
  
  output$Time.plot <- renderPlot({
    if (input$Time.level == "regione") {
      tmp_df <- subset(result_lists$region$ateneo, region %in% input$Time.unit.region)
      tmp_df <- merge(tmp_df, as.data.frame(italy_map)[,c("NOME","wid")],
                      by.x = 'region', by.y = "wid")
      tmp_df$ratio[is.na(tmp_df$ratio)] <- 0
      ggplot(tmp_df, aes(x=year, y=ratio, color=NOME)) +
        geom_line() +
        labs(x=NULL, y="Staff with suspect family relations") +
        coord_cartesian(xlim = c(min(tmp_df$year), max(tmp_df$year) + 2)) +
        scale_y_continuous(labels = percent) +
        geom_text_repel(
          data = subset(tmp_df, year == max(year)),
          aes(label=NOME),
          size = 4,
          nudge_x = .5,
          segment.color = NA
        ) +
        theme_bw() +
        guides(colour=FALSE)
    } else if (input$Time.level == "ateneo") {
      tmp_df <- subset(result_lists$ateneo$ateneo, ateneo %in% input$Time.unit.university)
      tmp_df$ratio[is.na(tmp_df$ratio)] <- 0
      ggplot(tmp_df, aes(x=year, y=ratio, color=ateneo)) +
        geom_line() +
        labs(x=NULL, y="Staff with suspect family relations") +
        coord_cartesian(xlim = c(min(tmp_df$year), max(tmp_df$year) + 2)) +
        scale_y_continuous(labels = percent) +
        geom_text_repel(
          data = subset(tmp_df, year == max(year)),
          aes(label=wikidata_label),
          size = 4,
          nudge_x = .5,
          segment.color = NA
        ) +
        theme_bw() +
        guides(colour=FALSE)
    }
  })
  
  output$Geo.plot.university <- renderPlot({
    ggplot(result_lists$ateneo$ateneo,
           aes(x=ratio,y=lat)) +
      geom_point(colour = 'red', size = 3, alpha = 0.4) +
      scale_x_continuous(labels = percent) +
      geom_smooth(se=FALSE, method = 'lm', colour = 'black') +
      geom_smooth(se=FALSE) +
      theme_bw() +
      labs(y="Latitude", x="Staff with suspect family relations",
           title=paste0("University (n=",nrow(result_lists$ateneo$ateneo),")"))
  })
  
  output$Geo.plot.faculty <- renderPlot({
    tmp_df <-
      result_lists$ateneo$facolta %>%
      dplyr::group_by(ateneo,lat)
    ggplot(tmp_df,
           aes(x=ratio,y=lat)) +
      geom_point(colour = 'red', size = 3, alpha = 0.4) +
      scale_x_continuous(labels = percent) +
      geom_smooth(se=FALSE, method = 'lm', colour = 'black') +
      geom_smooth(se=FALSE) +
      theme_bw() +
      labs(y="Latitude", x="Staff with suspect family relations",
           title=paste0("Faculty (n=",nrow(tmp_df),")"))
  })
  
  output$Geo.plot.department <- renderPlot({
    tmp_df <-
      result_lists$ateneo$dipartimento %>%
      dplyr::group_by(ateneo,lat)
    ggplot(tmp_df,
           aes(x=ratio,y=lat)) +
      geom_point(colour = 'red', size = 3, alpha = 0.4) +
      scale_x_continuous(labels = percent) +
      geom_smooth(se=FALSE, method = 'lm', colour = 'black') +
      geom_smooth(se=FALSE) +
      theme_bw() +
      labs(y="Latitude", x="Staff with suspect family relations",
           title=paste0("Department (n=",nrow(tmp_df),")"))
  })
  
})

# source('00_connect_to_db.R') # not in repo
# require(RMySQL)
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
# 
# ateneo_mysql_table <- dbReadTable(con, "ateneo")
# 
# dbDisconnect(con)

load("~/ownCloud/docenti_univ_ita/regioni_spatial_df.RData")
load("~/ownCloud/docenti_univ_ita/count/result_list.RData")
shinyApp(ui = ui, server = server)

