library(rjson)

f <- "http://beta.datavaalit.fi/api/v1/municipality/?format=json&limit=500"

dat <- fromJSON(paste(readLines(f), collapse=""))


