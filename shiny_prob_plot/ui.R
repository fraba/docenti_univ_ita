require(shiny)

shinyUI(fluidPage(

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
