library(rjson)


# Read election info
f.elections <- "http://beta.datavaalit.fi/api/v1/election/?format=json&limit=500"
election.data <- fromJSON(paste(readLines(f.elections), collapse=""))

# Read municipality info
f.municipalities <- "http://beta.datavaalit.fi/api/v1/municipality/?format=json"
municipality.data <- fromJSON(paste(readLines(f.municipalities), collapse=""))

# Extract voting percentages and corresponding municipality ids
voting.percentages <- as.numeric(sapply(election.data$objects[[2]]$voting_percentage, function (x) {x$value}))
municipality.id <- sapply(election.data$objects[[2]]$voting_percentage, function (x) {x$municipality})


