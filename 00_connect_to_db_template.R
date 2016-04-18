require(RMySQL)

pw <- {
  "password"
}

con <- dbConnect(RMySQL::MySQL(), 
                 host = "999.999.999.99",
                 port = 3306,
                 dbname = "db_name", 
                 username = "user", 
                 password = pw)
rm(pw)
