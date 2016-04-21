setwd("~/ownCloud/docenti_univ_ita")
surnames_bergamo <- read.csv("data_validation/surnames_bergamo.csv")
surnames_firenze <- read.csv("data_validation/surnames_firenze.csv")
surnames_matera <- read.csv("data_validation/surnames_matera.csv")

surnames_list <- 
  list(Bergamo = data.frame(surname = subset(surnames_bergamo, Anno == 2014)$Cognome,
                       freq = subset(surnames_bergamo, Anno == 2014)$Frequenza),
       Firenze = data.frame(surname = surnames_firenze$cognome,
                       freq = surnames_firenze$frequenza),
       Matera = data.frame(surname = surnames_matera$COGNOME,
                           freq = surnames_matera$NUMEROSITA))

save(surnames_list, file = "data_validation/surname_3cities.RData")
