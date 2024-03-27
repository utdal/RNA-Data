library(googlesheets4)

gs4_auth()

ss <- gs4_get("https://docs.google.com/spreadsheets/d/1Ma-ZYlGw-U0haJw2fStVYxyT99buWj4WMCKePfqvaww/edit#gid=0")
sheet_append(ss, data.frame(time=Sys.time()))
