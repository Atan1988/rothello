con <- DBI::dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "games",
  host = "othelloai.ccz20a6chtws.us-east-2.rds.amazonaws.com",
  port = 3306,
  username = "atan",
  password = "gyjltzw3813"
)


con %>% DBI::dbDisconnect()
