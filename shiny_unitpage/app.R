library(shiny)
library(DT)
library(leaflet)
library(shinyjs)

# Load stuff
setwd("~/ownCloud/docenti_univ_ita")
load("~/ownCloud/docenti_univ_ita/count/result_list.RData")
load("~/ownCloud/docenti_univ_ita/regioni_spatial_df.RData")
source("~/ownCloud/docenti_univ_ita/fun/plotSimProbDistr.R")
source("~/ownCloud/docenti_univ_ita/fun/runSimulation.R")

source('00_connect_to_db.R') # not in repo
dbSendQuery(con, "SET NAMES utf8")

res <- dbSendQuery(con,
                   paste0("SELECT wikidata_id, nome FROM regione"))
list_regions <- fetch(res, n = -1)
res <- dbSendQuery(con,
                   paste0("SELECT wikidata_id, wikidata_label FROM ateneo"))
list_universities <- fetch(res, n = -1)
res <- dbSendQuery(con,
                   paste0("SELECT ateneo_id, wikidata_label, facolta, facolta_id FROM faculty_wt_coordinates"))
list_facs <- fetch(res, n = -1)
res <- dbSendQuery(con,
                   paste0("SELECT ateneo_id, wikidata_label, dipartimento, dipartimento_id FROM department_wt_coordinates"))
list_deps  <- fetch(res, n = -1)
dbDisconnect(con)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  tags$head(
    tags$style(HTML("
                    #surname_table {
                        font-size: 80%;
                    }
                    #surname_table table td {
                        line-height:80%;
                    }
                    #unit_results_table table {
                        font-size: 70%;
                        background-color: #e5f2e5;
                    }
                    #unit_results_table table {
                        border:1px solid #CCC;
                        border-collapse:collapse;
                    }
                    #unit_results_table td {
                        border:none;
                    }
                    #unit_results_table tr > td:first-child { 
                          font-weight: bold;
                    }
                    #unit_results_table tr:nth-child(2) td,
                    #unit_results_table tr:nth-child(4) td,
                    #unit_results_table tr:nth-child(6) td {
                          border-bottom: 1px solid;
                    }
                    label.control-label {
                          font-size: 70%;
                    }
                    .selectize-input, .selectize-dropdown {
                          font-size: 70%;
                    }
                    
                    "))
  ),
  fluidRow(
    useShinyjs(),
    column(12,
           div(style = 'padding: 5px;',
               tags$b(style="text-align: center; font-size: 150%;", 
                      textOutput('title')),
               hr(style = 'margin: 0px')),
           fluidRow(
             column(2,
                    br(),
                    br(),
                    selectInput("level_choice", label = NA, choices = c("region","ateneo","facolta","dipartimento"),
                                selected = 'ateneo'),
                    selectizeInput("year_choice", label = "Year",
                                   choices = 2016:2000,
                                   selected = 2016),
                    hr(),
                    selectizeInput("region_choice", label = "Search a region",
                                   choices = setNames(list_regions$wikidata_id, 
                                                      list_regions$nome),
                                   selected = "",
                                   options = list(create = TRUE,
                                                  onInitialize = I('function() { this.setValue(""); }'))),
                    hr(),
                    selectizeInput("university_choice", label = "Search a university",
                                   choices = setNames(list_universities$wikidata_id, 
                                                      list_universities$wikidata_label),
                                   selected = "wd:Q1245318",
                                   options = list(create = TRUE)),
                    hr(),
                    selectizeInput("fac_choice", label = "Search a faculty",
                                   choices = NULL,
                                   options = list(create = TRUE)),
                    selectizeInput("dep_choice", label = "or a department",
                                   choices = NULL,
                                   options = list(create = TRUE))
             ),
             column(6,
                    dataTableOutput('surname_table')),
             column(4,
                    div(align='center', 
                        tableOutput('unit_results_table'),
                        span(title=paste0("The density plot presents the results from 10,000 ",
                                          "simulations where a population that equals the staff ",
                                          "of the unit (region, university, faculty or department) ",
                                          "is randomly created to measure the chances that a ",
                                          "certain number of people have the same surname."),
                             strong(style = "font-size: 80%", 
                                    "Simulating probability distributions of surnames"),
                             p(style = "font-size: 70%", "(based on 10,000 simulations)"),
                             plotOutput('prob_plot', height = '110px',
                                        width = "100%"),
                             p(style = "font-size: 65%; 
                                        width: 60%;
                                        border-radius: 5px; 
                                        border: 1px solid black;",
                               paste0("Times the number of staff observed in reality ",
                                      "with the same surname is obtained in a simulation (%)"))),
                        span(title = 'Slide to run a new simulation and change the percentage of highlighted results.',
                             sliderInput("qRight",
                                         "Percentage of simulation results highlighted in the plot",
                                         min = 0,
                                         max = 100,
                                         value = 80)),
                        hr(),
                        leafletOutput("unitmap", width = '90%', height = 150)))
           )
    )
  ),
  fluidRow(tags$div(id='footer',
                    style='font-size: 70%; text-align: center; width: 90%; padding: 10px;',
                    HTML(paste0(
                      "<p>Data: <a href='http://cercauniversita.cineca.it/' target='_blank'>CINECA</a> and <a href='http://www.paginebianche.it/contacognome' target='_blank'>Pagine Bianche</a> 2016 ",
                      "| Design: Francesco Bailo (<a href='https://twitter.com/FrBailo' target='_blank'>@FrBailo</a>) ",
                      "| Code: <a href='https://github.com/fraba/docenti_univ_ita/tree/master/shiny_unitpage' target='_blank'>GitHub</a> ",
                      "| Powered by: <a href='http://www.R-project.org/' target='_blank'>R</a> and <a href='http://shiny.rstudio.com/' target='_blank'>Shiny</a> ",
                      "| R packages: <a href='https://CRAN.R-project.org/package=DT' target='_blank'>DT</a>, <a href='https://CRAN.R-project.org/package=leaflet' target='_blank'>leaflet</a>, <a href='https://CRAN.R-project.org/package=shinyjs' target='_blank'>shinyjs</a>, <a href='http://ggplot2.org' target='_blank'>ggplot2</a>, <a href='scales' target='_blank'>scales</a>, <a href='https://CRAN.R-project.org/package=viridis'>viridis</a> ",
                      "| Hosted by: <a href='https://nectar.org.au/research-cloud/'>Nectar Cloud</a> ",
                      "| Version: 0.9 "))))
)
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  # Helper functions
  escapeSQL <- function(x) {
    return(gsub("'","''",x))
  }
  
  updateRegionChoiceBlank <- 
    function() {
      updateSelectizeInput(session, "region_choice", 
                           choices = setNames(list_regions$wikidata_id, 
                                              list_regions$nome),
                           selected = "")
    }
  updateRegionChoiceTo <- 
    function(unit_id) {
      updateSelectizeInput(session, "region_choice", 
                           choices = setNames(list_regions$wikidata_id, 
                                              list_regions$nome),
                           selected = unit_id)
    }
  updateUniversityChoiceBlank <-
    function() {
      updateSelectizeInput(session, 'university_choice',
                           choices = setNames(list_universities$wikidata_id,
                                              list_universities$wikidata_label),
                           selected = "")
    }
  updateUniversityChoiceTo <-
    function(unit_id) {
      updateSelectizeInput(session, 'university_choice',
                           choices = setNames(list_universities$wikidata_id,
                                              list_universities$wikidata_label),
                           selected = unit_id)
    }
  updateFacChoiceBlank <-
    function() {
      updateSelectizeInput(session, 'fac_choice',
                           choices = NULL,
                           selected = "")
    }
  updateDepChoiceBlank <-
    function() {
      updateSelectizeInput(session, 'dep_choice',
                           choices = NULL,
                           selected = "")
    }
  updateLevelChoiceTo <- function(x) {
    updateSelectInput(session, "level_choice", choices = c("region","ateneo","facolta","dipartimento"),
                      selected = x)
  }
  updateFacChoice <-
    function(at_id, yr) {
      print(at_id)
      print(yr)
      this_lst_facs <-
        as.character(unique(subset(result_lists[['facolta']][['facolta']], suspect_n > 0 & year == yr & ateneo == at_id)$unit))
      names(this_lst_facs) <-
        as.character(unique(subset(result_lists[['facolta']][['facolta']], suspect_n > 0 & year == yr & ateneo == at_id)$facolta))
      updateSelectizeInput(session, 'fac_choice',
                           choices = this_lst_facs,
                           selected = "")
    }
  updateDepChoice <-
    function(at_id, yr) {
      this_lst_deps <-
        as.character(unique(subset(result_lists[['dipartimento']][['dipartimento']], suspect_n > 0 & year == yr & ateneo == at_id)$unit))
      names(this_lst_deps) <-
        as.character(unique(subset(result_lists[['dipartimento']][['dipartimento']],  suspect_n > 0 & year == yr & ateneo == at_id)$dipartimento))
      updateSelectizeInput(session, 'dep_choice',
                           choices = this_lst_deps,
                           selected = "")
    }
  updateFacChoiceTo <-
    function(at_id, unit_id, yr) {
      this_lst_facs <-
        as.character(unique(subset(result_lists[['facolta']][['facolta']], suspect_n > 0 & year == yr & ateneo == at_id)$unit))
      names(this_lst_facs) <-
        as.character(unique(subset(result_lists[['facolta']][['facolta']], suspect_n > 0 & year == yr & ateneo == at_id)$facolta))
      updateSelectizeInput(session, 'fac_choice',
                           choices = this_lst_facs,
                           selected = unit_id)
    }
  updateDepChoiceTo <-
    function(at_id, unit_id, yr) {
      this_lst_deps <-
        as.character(unique(subset(result_lists[['dipartimento']][['dipartimento']], suspect_n > 0 & year == yr & ateneo == at_id)$unit))
      names(this_lst_deps) <-
        as.character(unique(subset(result_lists[['dipartimento']][['dipartimento']], suspect_n > 0 & year == yr & ateneo == at_id)$dipartimento))
      updateSelectizeInput(session, 'dep_choice',
                           choices = this_lst_deps,
                           selected = unit_id)
    }
  
  hide("level_choice")
  
  observe({
    if (input$level_choice == "region") {
      updateUniversityChoiceBlank()
      updateDepChoiceBlank()
      updateFacChoiceBlank()
    } 
  })
  
  observe({
    if (input$level_choice == "ateneo") {
      updateRegionChoiceBlank() 
    }
  })
  
  observe({
    if (input$university_choice != "") {
      updateLevelChoiceTo('ateneo')
      updateDepChoice(input$university_choice, input$year_choice)
      updateFacChoice(input$university_choice, input$year_choice)
    }
  })
  
  observe({
    if (input$region_choice != "") {
      updateLevelChoiceTo('region')
    }
  })
  
  observe({
    if (input$region_choice != "") {
      updateLevelChoiceTo('region')
    }
  })
  
  observe({
    if (input$dep_choice != "") {
      updateFacChoiceBlank()
      updateLevelChoiceTo('dipartimento')
    }
  })
  
  observe({
    if (input$fac_choice != "") {
      updateDepChoiceBlank()
      updateLevelChoiceTo('facolta')
    }
  })
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if(!is.null(query[['unit_year']])) {
      updateSelectizeInput(session, "year_choice", 
                           label = "Year",
                           choices = 2016:2000,
                           selected = query[['unit_year']])
    }
    if (!is.null(query[['unit_level']])) {
      updateLevelChoiceTo(query[['unit_level']])
      if (query[['unit_level']] == 'region') {
        if(!is.null(query[['unit_id']])) {
          updateRegionChoiceTo(query[['unit_id']])
        }
      }
      if (query[['unit_level']] == 'ateneo') {
        if(!is.null(query[['unit_id']])) {
          updateUniversityChoiceTo(query[['unit_id']])
        }
      }
    }
    
  })
  
  unit_data <-
    reactive({
      source('00_connect_to_db.R') # not in repo
      dbSendQuery(con, "SET NAMES utf8")
      
      if(input$level_choice == 'region') {
        unit_id <- input$region_choice
      }
      if(input$level_choice %in% c('ateneo','dipartimento','facolta')) {
        unit_id <- input$university_choice
      }
      res <- dbSendQuery(con,
                         paste0("SELECT name FROM ((SELECT wikidata_id AS id, wikidata_label AS name ",
                                "FROM ateneo) UNION (SELECT wikidata_id AS id, nome AS name FROM ",
                                "regione)) AS alias WHERE id = '", unit_id, "'"))
      data <- fetch(res, n = -1)
      unit_name <- data[['name']]
      if(input$level_choice == 'dipartimento') {
        unit_id <- input$dep_choice
        unit_name <- paste0(unique(list_deps$dipartimento[list_deps$dipartimento_id == unit_id]), " (", unit_name, ")")
        print(unit_name)
      }
      if(input$level_choice == 'facolta') {
        unit_id <- input$fac_choice
        unit_name <- paste0(unique(list_facs$facolta[list_facs$facolta_id == unit_id]), " (", unit_name, ")")
      }
      
      if (!input$level_choice %in% c('facolta','dipartimento')) {
        res <- dbSendQuery(con,
                           paste0("SELECT * FROM `simulation_long_df` WHERE unit = '",
                                  unit_id, "' AND year = '", input$year_choice, "'"))
        unit_simulation <- fetch(res, n = -1)
      } 
      
      if (input$level_choice  == 'facolta') {
        res <- dbSendQuery(con,
                           paste0("SELECT * FROM `simulation_long_df` WHERE unit = '",
                                  unit_id, "' AND ateneo = '",
                                  input$university_choice,
                                  "' AND year = '", input$year_choice, "'"))
        unit_simulation <- fetch(res, n = -1)
      }
      
      if (input$level_choice  == 'dipartimento') {
        print(unit_id)
        print(input$university_choice)
        print(input$year_choice)
        res <- dbSendQuery(con,
                           paste0("SELECT * FROM `simulation_long_df` WHERE unit = '",
                                  unit_id, "' AND ateneo = '",
                                  input$university_choice,
                                  "' AND year = '", input$year_choice, "'"))
        unit_simulation <- fetch(res, n = -1)
      }
      
      if (input$level_choice == 'region') {
        
        res <- dbSendQuery(con,
                           paste0("SELECT lon, lat FROM `regione` WHERE wikidata_id = '",
                                  unit_id, "'"))
        unit_lonlat <- fetch(res, n = -1)
        
        for (l in c("region","ateneo","facolta","dipartimento")) {
          unit_results_region <-
            subset(result_lists[[input$level_choice]][['region']], year == input$year_choice & region == unit_id)
          unit_results_ateneo <-
            subset(result_lists[[input$level_choice]][['ateneo']], year == input$year_choice & region == unit_id)
          unit_results_fac <-
            subset(result_lists[[input$level_choice]][['facolta']], year == input$year_choice & region == unit_id)
          unit_results_dep <-
            subset(result_lists[[input$level_choice]][['dipartimento']], year == input$year_choice & region == unit_id)
        }
      } else if (input$level_choice == 'ateneo') {
        
        res <- dbSendQuery(con,
                           paste0("SELECT lon, lat FROM `ateneo` WHERE wikidata_id = '",
                                  unit_id, "'"))
        unit_lonlat <- fetch(res, n = -1)
        
        unit_results_region <- NULL
        unit_results_ateneo <-
          subset(result_lists[[input$level_choice]][['ateneo']], year == input$year_choice & ateneo == unit_id)
        unit_results_fac <-
          subset(result_lists[[input$level_choice]][['facolta']], year == input$year_choice & ateneo == unit_id)
        unit_results_dep <-
          subset(result_lists[[input$level_choice]][['dipartimento']], year == input$year_choice & ateneo == unit_id)
      } else if (input$level_choice == 'facolta') {
        
        res <- dbSendQuery(con,
                           paste0("SELECT lon, lat FROM `ateneo` WHERE wikidata_id = '",
                                  input$university_choice, "'"))
        unit_lonlat <- fetch(res, n = -1)
        
        unit_results_region <- NULL
        unit_results_ateneo <- NULL
        unit_results_dep <- NULL
        unit_results_fac <-
          subset(result_lists[[input$level_choice]][['facolta']], year == input$year_choice &
                   unit == input$fac_choice & ateneo == input$university_choice)
      } else if (input$level_choice == 'dipartimento') {
        
        res <- dbSendQuery(con,
                           paste0("SELECT lon, lat FROM `ateneo` WHERE wikidata_id = '",
                                  input$university_choice, "'"))
        unit_lonlat <- fetch(res, n = -1)
        
        unit_results_region <- NULL
        unit_results_ateneo <- NULL
        unit_results_fac <- NULL
        unit_results_dep <-
          subset(result_lists[[input$level_choice]][['dipartimento']], year == input$year_choice &
                   unit == input$dep_choice & ateneo == input$university_choice)
      }
      
      dbDisconnect(con)
      
      return(list('unit_name' = unit_name, 
                  'unit_simulation' = unit_simulation,
                  'unit_lonlat' = unit_lonlat,
                  'unit_results_region' = unit_results_region,
                  'unit_results_ateneo' = unit_results_ateneo,
                  'unit_results_fac' = unit_results_fac,
                  'unit_results_dep' = unit_results_dep))
    })
  
  
  output$title <- renderText({
    unit_data()$unit_name
  })
  
  output$unit_results_table <- renderTable({
    if (input$level_choice == 'region') {
      output_table <- data.frame(matrix(ncol=3,nrow=8))
      output_table[[1]] <-
        c("region", NA,
          "ateneo (mean)", NA,
          "facolta (mean)", NA,
          "dipartimento (mean)", NA)
      output_table[[2]] <-
        c(rep(c("staff", "with suspect relations"), 4))
      
      output_table[[3]] <-
        c(c(round(unit_data()$unit_results_region$n[1],0),
            paste0(round(unit_data()$unit_results_region$suspect_n[1],0),
                   " (", round(unit_data()$unit_results_region$ratio[1]*100,2),"%)")),
          c(round(unit_data()$unit_results_ateneo$n[1],0),
            paste0(round(unit_data()$unit_results_ateneo$suspect_n[1],0),
                   " (", round(unit_data()$unit_results_ateneo$ratio[1]*100,2),"%)")),
          c(round(unit_data()$unit_results_fac$n[1],0),
            paste0(round(unit_data()$unit_results_fac$suspect_n[1],0),
                   " (", round(unit_data()$unit_results_fac$ratio[1]*100,2),"%)")),
          c(round(unit_data()$unit_results_dep$n[1],0),
            paste0(round(unit_data()$unit_results_dep$suspect_n[1],0),
                   " (", round(unit_data()$unit_results_dep$ratio[1]*100,2),"%)"))
        )
      
    } else if (input$level_choice == 'ateneo') {
      output_table <- data.frame(matrix(ncol=3,nrow=6))
      output_table[[1]] <-
        c("ateneo", NA,
          "facolta (mean)", NA,
          "dipartimento (mean)", NA)
      output_table[[2]] <-
        c(rep(c("staff", "with suspect relations"), 3))
      
      output_table[[3]] <-
        c(
          c(round(unit_data()$unit_results_ateneo$n[1],0),
            paste0(round(unit_data()$unit_results_ateneo$suspect_n[1],0),
                   " (", round(unit_data()$unit_results_ateneo$ratio[1]*100,2),"%)")),
          c(round(unit_data()$unit_results_fac$n[1],0),
            paste0(round(unit_data()$unit_results_fac$suspect_n[1],0),
                   " (", round(unit_data()$unit_results_fac$ratio[1]*100,2),"%)")),
          c(round(unit_data()$unit_results_dep$n[1],0),
            paste0(round(unit_data()$unit_results_dep$suspect_n[1],0),
                   " (", round(unit_data()$unit_results_dep$ratio[1]*100,2),"%)"))
        )
      
    } else if (input$level_choice == 'facolta') {
      output_table <- data.frame(matrix(ncol=3,nrow=2))
      output_table[[1]] <-
        c("facolta", NA)
      output_table[[2]] <-
        c(rep(c("staff", "with suspect relations"), 1))
      output_table[[3]] <-
        c(
          c(round(unit_data()$unit_results_fac$n[1],0),
            paste0(round(unit_data()$unit_results_fac$suspect_n[1],0),
                   " (", round(unit_data()$unit_results_fac$ratio[1]*100,2),"%)"))
        )
    } else if (input$level_choice == 'dipartimento') {
      output_table <- data.frame(matrix(ncol=3,nrow=2))
      output_table[[1]] <-
        c("dipartimento", NA)
      output_table[[2]] <-
        c(rep(c("staff", "with suspect relations"), 1))
      output_table[[3]] <-
        c(
          c(round(unit_data()$unit_results_dep$n[1],0),
            paste0(round(unit_data()$unit_results_dep$suspect_n[1],0),
                   " (", round(unit_data()$unit_results_dep$ratio[1]*100,2),"%)"))
        )
    }
    return(output_table)
  }, include.rownames = FALSE, include.colnames = FALSE)
  
  
  output$surname_table <- DT::renderDataTable({
    
    dt <- data.frame(Surname = unit_data()$unit_simulation$surname,
                     stringsAsFactors = F)
    dt[['Staff with this surname']] <- unit_data()$unit_simulation$docenti_wt_surname
    dt[['Staff with this surname perc']] <-
      round((unit_data()$unit_simulation$docenti_wt_surname / unit_data()$unit_simulation$unitpop) * 100, 4)
    dt[['Population with this surname']] <-
      prettyNum(round(unit_data()$unit_simulation$hh_mean_size * 
                        unit_data()$unit_simulation$hh_wt_surname / 
                        unit_data()$unit_simulation$perc_hh_wt_fixline, 0),
                big.mark=",",scientific=FALSE)
    dt[['Population with this surname perc']] <-
      round((unit_data()$unit_simulation$hh_mean_size * unit_data()$unit_simulation$hh_wt_surname) /
              (unit_data()$unit_simulation$largepop * unit_data()$unit_simulation$perc_hh_wt_fixline) *100, 4)
    dt[['Probability of observing by chance %']] <- unit_data()$unit_simulation$p_of_observing*100
    dt[['sim_id']] <- unit_data()$unit_simulation$sim_id
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Surname'),
          th(colspan = 2, 'Staff with this surname', title = 'Number of people employed in the region, university, faculty or department with this surname and as percentage of total number of employees.'),
          th(colspan = 2, 'Population with this surname', title = 'Estimate of people with this surname living in the region based on data collected from the telephone register and as percentage of the total population.'),
          th(rowspan = 2, 'Probability of observing by chance %', title = 'Percentage of simulations that returned equal or higher number of employees with the same surname.')
        ),
        tr(
          lapply(rep(c('n', '%'), 2), th)
        )
      )
    ))
    print(sketch)
    
    return(DT::datatable(dt, rownames = FALSE, extensions = 'Scroller',
                         selection = 'single', container = sketch,
                         options = list(order = list(list(5, 'asc')),
                                        autoWidth = TRUE,
                                        pageLength = 20,
                                        searching = TRUE,
                                        deferRender = TRUE,
                                        # scrollY = 620,
                                        scroller = TRUE,
                                        bInfo = TRUE,
                                        bLengthChange = FALSE,
                                        columnDefs = list(list(className = 'dt-center', targets = 1:5),
                                                          list(targets = 6,
                                                               visible = FALSE),
                                                          list(width = '10%', targets = 0),
                                                          list(width = '20%', targets = 1:4),
                                                          list(width = '10%', targets = 5))))
    )
  })
  
  output$prob_plot <- renderPlot({
    
    try({
      s <- input$surname_table_rows_selected
      
      if (length(s)) {
        s <- s[length(s)]
      } else {
        s <- unit_data()$unit_simulation$surname[
          which.min(unit_data()$unit_simulation$p_of_observing)
          ]
      }
      
      this_surname_data <- subset(unit_data()$unit_simulation, surname == s)
      
      sim <- with(this_surname_data,
                  runSimulation(hh_mean_size[1],
                                perc_hh_wt_fixline[1],
                                hh_wt_surname[1],
                                largepop[1],
                                docenti_wt_surname[1],
                                unitpop[1],
                                return_df = TRUE))
      
      return(plotSimProbDistr(sim,
                              this_surname_data$docenti_wt_surname[1],
                              this_surname_data$surname[1],
                              input$qRight))
      
    }, silent = TRUE)
    
  })
  
  output$unitmap <- renderLeaflet({
    
    if (input$region_choice == 'region') {
      
      leaflet() %>%
        addProviderTiles("OpenStreetMap.BlackAndWhite",
                         options = providerTileOptions(noWrap = TRUE, opacity = 0.3)) %>%
        addPolygons(data = subset(italy_map, wid == input$region_choice)) %>%
        setView(lng =  unit_data()$unit_lonlat[,'lon'], 
                lat =  unit_data()$unit_lonlat[,'lat'], zoom = 5)
      
    } else {
      
      leaflet() %>%
        addProviderTiles("OpenStreetMap.BlackAndWhite",
                         options = providerTileOptions(noWrap = TRUE, opacity = 0.3)) %>%
        addCircleMarkers(data =  unit_data()$unit_lonlat, lat = ~ as.numeric(lat), lng = ~ as.numeric(lon),
                         radius = 3) %>%
        setView(lng = unit_data()$unit_lonlat[,'lon'], lat = unit_data()$unit_lonlat[,'lat'], zoom = 5)
      
    }
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

