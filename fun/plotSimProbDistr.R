plotSimProbDistr <- function(sim, n_in_unitpop, surname, qRight = .8)  {
  require(ggplot2)
  require(scales)
  dens <- density(sim$result, adjust = 5)
  dens_df <- data.frame(x = dens$x, y = dens$y)
  
  observed_in_sim <- sum(sim >= n_in_unitpop) / nrow(sim)
  
  observed_df <- 
    data.frame(x = n_in_unitpop,
               y = subset(dens_df, 
                          x == x[which.min(abs(x - n_in_unitpop))])$y,
               label = scales::percent(observed_in_sim))
  
  q0 <- quantile(sim$result,0)
  qRight <- quantile(sim$result, qRight / 100)
  p <- ggplot(data = sim) + 
    geom_density(aes(x=result, y = ..density..), 
                 color = NA,
                 fill = 'red',
                 adjust = 5,
                 alpha = .15) +
    geom_area(data = subset(dens_df, x >= q0 & x <= qRight), 
              aes(x=x,y=y), fill = 'red') +
    geom_point(data = observed_df, 
               aes(x=x, y=y)) +
    geom_text(data = observed_df, 
              aes(x=x, y=y, label=label), hjust=0, vjust=-1) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "gainsboro")) +
    labs(y = 'probabilità', 
         x = paste0("docenti con cognome '", surname,"'")) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = seq(0, max(c(sim$result, n_in_unitpop)), by = 1))
  
  return(p)
}