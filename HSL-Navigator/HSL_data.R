# Script for reading HSL bus stop location data

data.folder <- "/Users/juusoparkkinen/Documents/workspace/data/HSL_Navigator/"

# Read data
dat <- read.csv(paste(data.folder, "hastusgps20130114-20130120.rdm", sep=""), sep=";", header=FALSE)
# Read column info
temp <- scan(paste(data.folder, "hastus_kuvaus.txt", sep=""), what="character", sep=";", n=19)
temp <- gsub(" ", "_", temp)
names(dat) <- temp

# Read stops data
stops <- read.csv(paste(data.folder, "google_transit/stops.txt", sep=""))
# any(is.na(stops$stop_lat))
# [1] FALSE
# => no missing location info in stops

# Combine data
Locs <- unique(dat$Location)
length(which(Locs %in% stops$stop_id)) / length(Locs)
# [1] 0.9967404
# => almost all stops in data have location information

dat2 <- merge(dat, stops, by.x="Location", by.y="stop_id")
save(dat2, file="HSL-Navigator/HSL_data_combined_20130208.RData")

# Plot then on map, using ggmap
library(ggmap)

Hel.center <- geocode("Helsinki")
Hel.center$lat <- Hel.center$lat + 0.1
# Hel.map <- qmap(c(lon=Hel.center$lon, lat=Hel.center$lat), source="google", zoom=10)
Hel.map <- get_googlemap(center=c(lon=Hel.center$lon, lat=Hel.center$lat), zoom=10)
Hel2.map <- ggmap(Hel.map) + geom_point(data=stops, aes(x=stop_lon, y=stop_lat))
Hel2.map
#stamen <- get_stamenmap()
# file.remove("ggmapTemp.png")