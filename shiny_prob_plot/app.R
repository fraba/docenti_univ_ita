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
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("qRight",
                  "Percentage of simulation",
                  min = 1,
                  max = 100,
                  value = 80)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("probPlot")
    ),
    
    position = 'right'
  )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  n <- sample(1:nrow(simulation_long_df),1)
  sim <- with(simulation_long_df,
              runSimulation(hh_mean_size[n], 
                            perc_hh_wt_fixline[n],
                            hh_wt_surname[n], 
                            largepop[n], 
                            docenti_wt_surname[n], 
                            unitpop[n],
                            return_df = TRUE))
  
  output$probPlot <- renderPlot({
    
    plotSimProbDistr(sim, 
                     simulation_long_df$docenti_wt_surname[n], 
                     simulation_long_df$surname[n], 
                     input$qRight)
    
  })
  
})

source("/Users/francesco/ownCloud/docenti_univ_ita/fun/plotSimProbDistr.R")
source("/Users/francesco/ownCloud/docenti_univ_ita/fun/runSimulation.R")
load("/Users/francesco/ownCloud/docenti_univ_ita/sim/simulation_df.RData")

# Run the application 
shinyApp(ui = ui, server = server)

