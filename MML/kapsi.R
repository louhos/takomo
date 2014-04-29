library(sorvi)

# Define url from http://kartat.kapsi.fi
url <- "http://kartat.kapsi.fi/files/kuntajako/kuntajako_4500k/etrs89/gml/TietoaKuntajaosta_2013_4500k.zip"

# Define temporary data directory
data.dir <- gsub(" ", "-", paste("tmp.data.", date(), sep = ""))
system(paste("mkdir ", data.dir))
local.zip <- paste(data.dir, "/tmp.zip", sep = "")

# Download the file:
download.file(url, destfile = local.zip)

# Unzip the downloaded zip file
data.dir <- file.path(data.dir)
# system(paste('unzip', local.zip))
unzip(local.zip, exdir = data.dir)

# In the above we have this file:
my.file <- "TietoaKuntajaosta_2013_4500k/SuomenKuntajako_2013_4500k.xml"

# List available map layers
library(rgdal)
ogrListLayers(my.file)
boundaries <- readOGR(my.file, layer = "AdministrativeBoundary")
units <- readOGR(my.file, layer = "AdministrativeUnit")

# Plotting
PlotShape(boundaries, "nationalLevel")
PlotShape(units, "nationalLevel")

# -----------------------------------------------
