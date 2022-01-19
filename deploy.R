library(rsconnect)
library(jsonlite)

creds <- jsonlite::fromJSON("D:/Shiny/credsFiktitious.json")
rsconnect::setAccountInfo(name   = creds$name,
                          token  = creds$token,
                          secret = creds$secret)
rm(creds)

deployApp("C:/Users/Alexis/Desktop/exerciseGenerator", account = "fiktitious", appName = "exerciseGenerator", forceUpdate = TRUE)
