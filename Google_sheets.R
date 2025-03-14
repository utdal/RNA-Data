library(googlesheets4)

gs4_auth()

ss <- gs4_get("link")
sheet_append(ss, data.frame(time=Sys.time()))
