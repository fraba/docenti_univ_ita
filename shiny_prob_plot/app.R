library(shiny)

ui <- shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "http://146.118.107.12/univapp/styles/style.css")),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("qRight",
                  "Percentage of simulations",
                  min = 0,
                  max = 100,
                  value = 80)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("probPlot", height = '200px')
    ),
    
    position = 'right'
  )
))


server <- shinyServer(function(input, output, session) {
  
  row <- 177506
  
  sim <- with(simulation_long_df,
              runSimulation(hh_mean_size[row],
                            perc_hh_wt_fixline[row],
                            hh_wt_surname[row],
                            largepop[row],
                            docenti_wt_surname[row],
                            unitpop[row],
                            return_df = TRUE))

  output$probPlot <- renderPlot({

    plotSimProbDistr(sim,
                     simulation_long_df$docenti_wt_surname[row],
                     simulation_long_df$surname[row],
                     input$qRight)

  })
  
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

