require(shiny)

shinyServer(function(input, output) {
  
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
    
    source("/Users/francesco/ownCloud/docenti_univ_ita/fun/plotSimProbDistr.R")
    source("/Users/francesco/ownCloud/docenti_univ_ita/fun/runSimulation.R")
    load("/Users/francesco/ownCloud/docenti_univ_ita/sim/simulation_df.RData")
    
    plotSimProbDistr(sim, 
                     simulation_long_df$docenti_wt_surname[n], 
                     simulation_long_df$surname[n], 
                     input$qRight)

  })

})
