library(shiny)
library(DT)
library(leaflet)

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
                    
                    "))
  ),
  fluidRow(
    column(12,
           div(style = 'padding: 5px;',
               tags$b(style="text-align: center; font-size: 150%;", 
                      textOutput('title')),
               hr(style = 'margin: 0px')),
           fluidRow(
             column(2,
                    br(),
                    br(),
                    selectizeInput("year_choice", label = "Year",
                                   choices = 2016:2000,
                                   options = list(onInitialize = I('function() { this.setValue(""); }'))),
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
                                   options = list(create = TRUE,
                                                  onInitialize = I('function() { this.setValue(""); }'))),
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
  )
)
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  # Default values
  unit_id <- 'wd:Q1245318'
  unit_level <- 'ateneo'
  unit_year <- '2016'
  unit_ateneo <- NULL
  
  unit_lonlat <- data.frame()
  unit_name <- character()
  unit_data <- data.frame()
  unit_results_region <- data.frame()
  unit_results_ateneo <- data.frame()
  unit_results_fac <- data.frame()
  unit_results_dep <- data.frame()
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    # input_university <-input$university_choice
    # input_region <-input$region_choice
    # input_fac <-input$fac_choice
    # input_dep <-input$dep_choice
    
    if (!is.null(query[['unit_id']])) {
      unit_id <<- query[['unit_id']]
    }
    if (!is.null(query[['unit_id']])) {
      unit_level <<-  query[['unit_level']]
    }
    
    # Year
    if (!is.null(query[['unit_year']])) {
      unit_year <<- query[['unit_year']]
    }
    updateSelectizeInput(session, 'year_choice',
                         choices = 2016:2000,
                         selected = unit_year)
    
    source('00_connect_to_db.R') # not in repo
    dbSendQuery(con, "SET NAMES utf8")
    
    res <- dbSendQuery(con, 
                       paste0("SELECT name FROM ((SELECT wikidata_id AS id, wikidata_label AS name ",
                              "FROM ateneo) UNION (SELECT wikidata_id AS id, nome AS name FROM ",
                              "regione)) AS alias WHERE id = '", unit_id, "'"))
    data <- fetch(res, n = -1)
    unit_name <<- data[['name']]
    
    res <- dbSendQuery(con, 
                       paste0("SELECT * FROM `simulation_long_df` WHERE unit = '",
                              unit_id, "' AND year = '", unit_year, "'"))
    unit_data <<- fetch(res, n = -1)
    
    if (unit_level == 'region') {
      
      updateSelectizeInput(session, 'region_choice', 
                           choices = setNames(list_regions$wikidata_id, 
                                              list_regions$nome),
                           selected = list_regions$wikidata_id[match(unit_name,list_regions$nome)])
      
      res <- dbSendQuery(con, 
                         paste0("SELECT lon, lat FROM `regione_wt_coord` WHERE wikidata_id = '",
                                unit_id, "'"))
      unit_lonlat <<- fetch(res, n = -1)
      
      for (l in c("region","ateneo","facolta","dipartimento")) {
        unit_results_region <<- 
          subset(result_lists[[unit_level]][['region']], year == unit_year & region == unit_id)
        unit_results_ateneo <<- 
          subset(result_lists[[unit_level]][['ateneo']], year == unit_year & region == unit_id)
        unit_results_fac <<- 
          subset(result_lists[[unit_level]][['facolta']], year == unit_year & region == unit_id)
        unit_results_dep <<- 
          subset(result_lists[[unit_level]][['dipartimento']], year == unit_year & region == unit_id)
      }
    } else if (unit_level == 'ateneo') {
      
      updateSelectizeInput(session, 'university_choice', 
                           choices = setNames(list_universities$wikidata_id, 
                                              list_universities$wikidata_label),
                           selected = list_universities$wikidata_id[match(unit_name,list_universities$wikidata_label)])
      
      res <- dbSendQuery(con, 
                         paste0("SELECT lon, lat FROM `ateneo` WHERE wikidata_id = '",
                                unit_id, "'"))
      unit_lonlat <<- fetch(res, n = -1)
      
      unit_results_ateneo <<- 
        subset(result_lists[[unit_level]][['ateneo']], year == unit_year & ateneo == unit_id)
      unit_results_fac <<- 
        subset(result_lists[[unit_level]][['facolta']], year == unit_year & ateneo == unit_id)
      unit_results_dep <<- 
        subset(result_lists[[unit_level]][['dipartimento']], year == unit_year & ateneo == unit_id)
    } else if (unit_level == 'facolta') {
      
      res <- dbSendQuery(con, 
                         paste0("SELECT lon, lat FROM `ateneo` WHERE wikidata_id = '",
                                unit_ateneo, "'"))
      unit_lonlat <<- fetch(res, n = -1)
      
      unit_results_fac <<- 
        subset(result_lists[[unit_level]][['facolta']], year == unit_year & 
                 facolta.id == unit_id & facolta.ateneo == unit_ateneo)
    } else {
      
      res <- dbSendQuery(con, 
                         paste0("SELECT lon, lat FROM `ateneo` WHERE wikidata_id = '",
                                unit_ateneo, "'"))
      unit_lonlat <<- fetch(res, n = -1)
      
      unit_results_dep <<- 
        subset(result_lists[[unit_level]][['dipartimento']], year == unit_year & 
                 dipartimento.id == unit_id & dipartimento.ateneo == unit_ateneo)
    }
    
    dbDisconnect(con)
    
  })
  
  
  output$title <- renderText({
    unit_name
  })
  
  output$unit_results_table <- renderTable({
    if (unit_level == 'region') {
      output_table <- data.frame(matrix(ncol=3,nrow=8))
      output_table[[1]] <- 
        c("region", NA, 
          "ateneo (mean)", NA, 
          "facolta (mean)", NA, 
          "dipartimento (mean)", NA)
      output_table[[2]] <-
        c(rep(c("staff", "with suspect relations"), 4))
      
      output_table[[3]] <- 
        c(c(round(unit_results_region$n[1],0),
            paste0(round(unit_results_region$suspect_n[1],0), 
                   " (", round(unit_results_region$ratio[1]*100,2),"%)")),
          c(round(unit_results_ateneo$n[1],0),
            paste0(round(unit_results_ateneo$suspect_n[1],0), 
                   " (", round(unit_results_ateneo$ratio[1]*100,2),"%)")),
          c(round(unit_results_fac$n[1],0),
            paste0(round(unit_results_fac$suspect_n[1],0), 
                   " (", round(unit_results_fac$ratio[1]*100,2),"%)")),
          c(round(unit_results_dep$n[1],0),
            paste0(round(unit_results_dep$suspect_n[1],0), 
                   " (", round(unit_results_dep$ratio[1]*100,2),"%)"))
        )
      
    } else if (unit_level == 'ateneo') {
      output_table <- data.frame(matrix(ncol=3,nrow=6))
      output_table[[1]] <- 
        c("ateneo", NA, 
          "facolta (mean)", NA, 
          "dipartimento (mean)", NA)
      output_table[[2]] <-
        c(rep(c("staff", "with suspect relations"), 3))
      
      output_table[[3]] <- 
        c(
          c(round(unit_results_ateneo$n[1],0),
            paste0(round(unit_results_ateneo$suspect_n[1],0), 
                   " (", round(unit_results_ateneo$ratio[1]*100,2),"%)")),
          c(round(unit_results_fac$n[1],0),
            paste0(round(unit_results_fac$suspect_n[1],0), 
                   " (", round(unit_results_fac$ratio[1]*100,2),"%)")),
          c(round(unit_results_dep$n[1],0),
            paste0(round(unit_results_dep$suspect_n[1],0), 
                   " (", round(unit_results_dep$ratio[1]*100,2),"%)"))
        )
      
    } else if (unit_level == 'facolta') {
      output_table <- data.frame(matrix(ncol=3,nrow=2))
      output_table[[1]] <- 
        c("facolta", NA)
      output_table[[2]] <-
        c(rep(c("staff", "with suspect relations"), 1))
      output_table[[3]] <- 
        c(
          c(round(unit_results_fac$n[1],0),
            paste0(round(unit_results_fac$suspect_n[1],0), 
                   " (", round(unit_results_fac$ratio[1]*100,2),"%)"))
        )
    } else {
      output_table <- data.frame(matrix(ncol=3,nrow=2))
      output_table[[1]] <- 
        c("dipartimento", NA)
      output_table[[2]] <-
        c(rep(c("staff", "with suspect relations"), 1))
      output_table[[3]] <- 
        c(
          c(round(unit_results_dep$n[1],0),
            paste0(round(unit_results_dep$suspect_n[1],0), 
                   " (", round(unit_results_dep$ratio[1]*100,2),"%)"))
        )
    }
    return(output_table)
  }, include.rownames = FALSE, include.colnames = FALSE)
  
  
  output$surname_table <- DT::renderDataTable({
    
    dt <- data.frame(Surname = unit_data$surname,
                     stringsAsFactors = F)
    dt[['Staff with this surname']] <- unit_data$docenti_wt_surname
    dt[['Staff with this surname perc']] <-
      round((unit_data$docenti_wt_surname / unit_data$unitpop) * 100, 4)
    dt[['Population with this surname']] <- 
      prettyNum(round(unit_data$hh_mean_size * unit_data$hh_wt_surname / unit_data$perc_hh_wt_fixline, 0),
                big.mark=",",scientific=FALSE)
    dt[['Population with this surname perc']] <-
      round((unit_data$hh_mean_size * unit_data$hh_wt_surname) /
              (unit_data$largepop * unit_data$perc_hh_wt_fixline) *100, 4)
    dt[['Probability of observing by chance %']] <- unit_data$p_of_observing*100
    dt[['sim_id']] <- unit_data$sim_id
    
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
    
    s <- input$surname_table_rows_selected
    
    if (length(s)) {
      s <- s[length(s)]
    } else {
      s <- unit_data$surname[which.min(unit_data$p_of_observing)]
    }
    
    this_surname_data <- subset(unit_data, surname == s)
    
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
    
    
  })
  
  output$unitmap <- renderLeaflet({
    
    if (unit_level == 'region') {
      
      leaflet() %>%
        addProviderTiles("OpenStreetMap.BlackAndWhite",
                         options = providerTileOptions(noWrap = TRUE, opacity = 0.3)) %>%
        addPolygons(data = subset(italy_map, wid == unit_id)) %>%
        setView(lng = unit_lonlat[,'lon'], lat = unit_lonlat[,'lat'], zoom = 5)
      
    } else {
      
      leaflet() %>%
        addProviderTiles("OpenStreetMap.BlackAndWhite",
                         options = providerTileOptions(noWrap = TRUE, opacity = 0.3)) %>%
        addCircleMarkers(data =  unit_lonlat, lat = ~ as.numeric(lat), lng = ~ as.numeric(lon),
                         radius = 3) %>%
        setView(lng = unit_lonlat[,'lon'], lat = unit_lonlat[,'lat'], zoom = 5)
      
    }
  })
  
})

# Load stuff
setwd("~/ownCloud/docenti_univ_ita")
load("~/ownCloud/docenti_univ_ita/count/result_list.RData")
load("~/ownCloud/docenti_univ_ita/regioni_spatial_df.RData")
source("~/ownCloud/docenti_univ_ita/fun/plotSimProbDistr.R")
source("~/ownCloud/docenti_univ_ita/fun/runSimulation.R")

source('00_connect_to_db.R') # not in repo
dbSendQuery(con, "SET NAMES utf8")

res <- dbSendQuery(con, 
                   paste0("SELECT wikidata_id, nome FROM `regione`"))
list_regions <- fetch(res, n = -1)
res <- dbSendQuery(con, 
                   paste0("SELECT wikidata_id, wikidata_label FROM `ateneo`"))
list_universities <- fetch(res, n = -1)
res <- dbSendQuery(con, 
                   paste0("SELECT ateneo_id, wikidata_label, facolta FROM `faculty_wt_coordinates`"))
list_facs <- fetch(res, n = -1)
res <- dbSendQuery(con, 
                   paste0("SELECT ateneo_id, wikidata_label, dipartimento FROM `department_wt_coordinates`"))
list_deps  <- fetch(res, n = -1)
dbDisconnect(con)


# Run the application 
shinyApp(ui = ui, server = server)

