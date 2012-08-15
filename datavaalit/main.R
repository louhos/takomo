library(sorvi)
library(rjson)

ReadDatavaalit <- function (data.id) {

  # Read election info
  if (data.id ==  "election.data") { 

    f <- "http://beta.datavaalit.fi/api/v1/election/?format=json&limit=500"
    dat <- fromJSON(paste(readLines(f), collapse = ""))
    
  } else if (data.id == "municipality.data") {

    f <- "http://beta.datavaalit.fi/api/v1/municipality/?format=json&limit=500"
    dat <- fromJSON(paste(readLines(f), collapse = ""))

  }

  dat  
}


election.data <- ReadDatavaalit("election.data")
municipality.data <- ReadDatavaalit("municipality.data")



# Extract voting percentages and corresponding municipality ids
voting.percentages <- as.numeric(sapply(election.data$objects[[2]]$voting_percentage, function (x) {x$value}))
municipality.id <- sapply(election.data$objects[[2]]$voting_percentage, function (x) {x$municipality})
municipality.id <- sapply(strsplit(municipality.id, "/"), function (x) {x[[5]]})

# Load Maanmittauslaitos data (C) Maanmittauslaitos 2011
LoadData("MML")
sp <- MML[["1_milj_Shape_etrs_shape"]]$kunta1_p

# Match MML and Datavaalit data sets
mml.id <- as.numeric(as.character(as.data.frame(sp)$Kunta))
sp$voting.percentage <- voting.percentages[match(mml.id, municipality.id)]

# Visualize
tmp <- PlotShape(sp, "voting.percentage")


#sapply(municipality.data$objects, function (x) {x$resource_uri})
#sapply(municipality.data$objects, function (x) {x$id})
#sapply(municipality.data$objects, function (x) {x$name})
#names <- sapply(municipality.data$objects, function (x) {x$name})    
#match(as.data.frame(sp)$Kunta.FI, names)
#setdiff(municipality.id, sp$Kunta)
#sapply(municipality.data$objects, function (x) {x$name})
#setdiff(sapply(municipality.data$objects, function (x) {x$name}), sp$Kunta.FI)

