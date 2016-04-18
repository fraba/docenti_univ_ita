runSimulation <- function(hh_mean_size, 
                          perc_hh_wt_fixline,
                          hh_wt_surname, 
                          largepop, 
                          docenti_wt_surname, 
                          unitpop, 
                          surname = NULL,
                          ateneo = NULL,
                          unit = NULL, unit_level = NULL, region = NULL, year = NULL,
                          return_df = FALSE,
                          loops = 10000) {
  n_in_largepop <- hh_mean_size * hh_wt_surname
  n_in_univ <- docenti_wt_surname
  p <- n_in_largepop / (largepop * perc_hh_wt_fixline)
  expected_in_univ <- p * unitpop
  
  sim <- data.frame(result=rbinom(loops, size = unitpop, prob = p))
  
  p_of_observing <- sum(sim$result >= n_in_univ) / length(sim$result)
  surname <- toupper(surname)
  output_lst <-
    list(docenti_wt_surname = docenti_wt_surname,
         p_of_observing = p_of_observing, 
         surname = surname,
         ateneo = ateneo,
         unit = unit,
         unitpop = unitpop,
         unit_level = unit_level,
         region = region,
         year = year,
         hh_mean_size = hh_mean_size,
         perc_hh_wt_fixline = perc_hh_wt_fixline,
         hh_wt_surname = hh_wt_surname,
         largepop = largepop)
  if (return_df == TRUE) {
    return(sim)
  }
  return(output_lst)
}