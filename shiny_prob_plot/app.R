#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "http://146.118.107.12/univapp/styles/style.css")),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("qRight",
                  "Percentage of simulations",
                  min = 1,
                  max = 100,
                  value = 80)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("probPlot", height = '95px')
    ),
    
    position = 'right'
  )
))


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  row <- 0
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['simid']])) {
     row <<- query[['simid']]
     print(row)
    } else {
      row <<- 1
    }
  })
  
  
  # sim <- with(simulation_long_df,
  #             runSimulation(hh_mean_size[row], 
  #                           perc_hh_wt_fixline[row],
  #                           hh_wt_surname[row], 
  #                           largepop[row], 
  #                           docenti_wt_surname[row], 
  #                           unitpop[row],
  #                           return_df = TRUE))
  # 
  # output$probPlot <- renderPlot({
  #   
  #   plotSimProbDistr(sim, 
  #                    simulation_long_df$docenti_wt_surname[row], 
  #                    simulation_long_df$surname[row], 
  #                    input$qRight)
    
  # })
  
})

# source('/Users/francesco/ownCloud/docenti_univ_ita/00_connect_to_db.R') # not in repo
## require(RMySQL)
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


# simulation_long_df <- dbReadTable(con, "simulation_long_df")
load("/Users/francesco/ownCloud/docenti_univ_ita/sim/simulation_df.RData")

# dbDisconnect(con)

source("https://raw.githubusercontent.com/fraba/docenti_univ_ita/master/fun/plotSimProbDistr.R")
source("https://raw.githubusercontent.com/fraba/docenti_univ_ita/master/fun/runSimulation.R")

# Run the application 
shinyApp(ui = ui, server = server)

