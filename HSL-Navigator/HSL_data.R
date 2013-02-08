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
# install.packages(c("ggmap", "mapproj"))
library(ggmap)

# Locate Helsinki
Hel.center <- geocode("Helsinki")
Hel.center$lat <- Hel.center$lat + 0.1
# Hel.bbox <- unlist(attributes(Hel.map)$bb[c(2, 1, 4, 3)])
# names(Hel.bbox) <- c("left", "bottom", "right", "top")

# Use googlemap
Hel.googlemap <- get_map(location=c(lon=Hel.center$lon, lat=Hel.center$lat), zoom=10, source="google")
hmap1 <- ggmap(Hel.googlemap) + geom_point(data=stops, aes(x=stop_lon, y=stop_lat))
hmap1

# Use stamen
Hel.stamen <- get_map(location=c(lon=Hel.center$lon, lat=Hel.center$lat), zoom=10, source="stamen", maptype="watercolor")
hmap2 <- ggmap(Hel.stamen) + geom_point(data=stops, aes(x=stop_lon, y=stop_lat))
# Hel.osm <- get_map(location=c(lon=Hel.center$lon, lat=Hel.center$lat), zoom=10, source="osm")
# hmap3 <- ggmap(Hel.osm) + geom_point(data=stops, aes(x=stop_lon, y=stop_lat))


# file.remove("ggmapTemp.png")