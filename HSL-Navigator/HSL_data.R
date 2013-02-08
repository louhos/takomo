# Script for reading HSL bus stop location data

data.folder <- "/Users/juusoparkkinen/Documents/workspace/data/HSL_Navigator/"

# Read data
dat <- read.csv(paste(data.folder, "hastusgps20130114-20130120.rdm", sep=""), sep=";", header=FALSE)
# Read column info
temp <- scan(paste(data.folder, "hastus_kuvaus.txt", sep=""), what="character", sep=";", n=19)
names(dat) <- temp

# Read stops data
stops <- read.csv(paste(data.folder, "google_transit/stops.txt", sep=""))

# Plot then on map, using ggmap
library(ggmap)

map.center <- geocode("Helsinki")
Hel
Hel.map <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom=10)
Hel2.map <- Hel.map + geom_point(data=stops, aes(x=stop_lon, y=stop_lat))

# file.remove("ggmapTemp.png")