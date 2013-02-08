# Script for reading HSL bus stop location data

data.folder <- "/Users/juusoparkkinen/Documents/workspace/data/HSL_Navigator/"

# Read data
temp <- read.csv(paste(data.folder, "hastusgps20130114-20130120.rdm", sep=""), sep=";", header=FALSE)

# Read stops data
stops <- read.csv(paste(data.folder, "google_transit/stops.txt", sep=""))

# Plot then on map, using ggmap
library(ggmap)

map.center <- geocode("Helsinki")
Hel.map <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom=10)
Hel2.map <- Hel.map + geom_point(data=stops, aes(x=stop_lon, y=stop_lat))