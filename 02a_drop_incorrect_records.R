load("~/ownCloud/docenti_univ_ita/surname_per_region.RData")
load("~/ownCloud/docenti_univ_ita/sim/simulation_df.RData")

surnames_wt_zero_pop <-
  as.character(unique(subset(simulation_long_df, hh_wt_surname == 0)$surname))

surname_per_region <-
  subset(surname_per_region, !(tolower(surname) %in% tolower(surnames_wt_zero_pop)))

save(surnames_to_scrape, surname_per_region, file = "~/ownCloud/docenti_univ_ita/surname_per_region.RData")
