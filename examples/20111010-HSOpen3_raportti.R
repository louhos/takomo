# This script is part of the Louhos-project (http://louhos.github.com/)

# Copyright (C) 2010-2013 Juuso Parkkinen and Leo Lahti.
# Contact: <http://louhos.github.com/contact>. 
# All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# Install and load sorvi package
# Instructions in http://louhos.github.com/sorvi/asennus.html
# This script is tested with sorvi version 0.2.27
library(sorvi)

# Load required packages
# Remember to install required packages (e.g. 'install.packages("ggplot2")')
library(ggplot2)


# Get Oikotie myynnit data
Oikotie <- sorvi::GetOikotie()
hr.myynnit <- Oikotie$hr.myynnit

# Get Lukio data
Lukiot <- sorvi::GetLukiot()
hr.lukiot <- Lukiot$hr.lukiot

# Compute mean and median prices per square meter for each street - zip code combination
means <- aggregate(hr.myynnit$Price.per.square, list(hr.myynnit$Zip.code, hr.myynnit$Street), mean)
medians <- aggregate(hr.myynnit$Price.per.square, list(hr.myynnit$Zip.code, hr.myynnit$Street), median)
hri.dat <- cbind(means, medians[,3])
names(hri.dat) <- c("Postinumero", "Katu", "Hinta.keskiarvo", "Hinta.mediaani")

## Get Helsinki Map (use function 'ggooglemap' from earlier HS open 3 blogpost)
Helsinki.center <- c(lon=24.93, lat = 60.20)
HelsinkiMap <- sorvi::GetStaticmapGoogleMaps(center = Helsinki.center, zoom = 11, maptype="Map", scale=1)
theme_set(sorvi::GetThemeMap())
hplot <- ggplot(HelsinkiMap, aes(x=lon, y=lat))
hplot <- hplot + geom_tile(aes(fill=fill)) + scale_fill_identity(guide="none")
hplot <- hplot + scale_x_continuous('Longitude') + scale_y_continuous('Latitude')
hplot <- hplot + ggtitle("Map of Helsinki")

##########################
## Geocode street names ##
##########################

# Get geocodes for the address combinations using GoogleMaps API
# NOTE! Can query only 2500 times per day
# Could use OpenStreetMap as well, see get.geocode.OpenStreetMap
message("Reading geocodes from dropbox...")
con <- url("http://dl.dropbox.com/u/792906/data/HR_geocodes_20111023.RData")
load(con)
close(con)
# hr.geo.codes <- list()
# for (i in 1:2500) { #First day
# # Second day: for (i in (2501:nrow(combs))[-c(3699:3701-2499)]) { # Remove Tarkk'ampujankatu because it causes an error
#   if (i %% 100 == 0)
#     cat(i, ".")
#   temp <- GetGeocodeGoogleMaps(paste(hri.dat$Katu[i], hri.dat$Postinumero[i], "FI", sep=", "))
#   hr.geo.codes[[i]] <- as.numeric(temp)
# }

# Filter out missing geocodes
totake <- which(sapply(hr.geo.codes, length)==2)
lons <- as.numeric(sapply(hr.geo.codes[totake], function(x) x[2]))
lats <- as.numeric(sapply(hr.geo.codes[totake], function(x) x[1]))

# Filter our locations outside the map in use
totake2 <- which(lons >= min(HelsinkiMap$lon) & lons <= max(HelsinkiMap$lon) & lats >= min(HelsinkiMap$lat) & lats <= max(HelsinkiMap$lat))

# Add locations to price data
hri.dat2 <- cbind(hri.dat[totake[totake2],], Lon=lons[totake2], Lat=lats[totake2])

# Filter out extreme values in median prices
hri.dat2 <- subset(hri.dat2, (Hinta.mediaani >= 2000 & Hinta.mediaani <= 6000))

# Filter out schools not in map range
hr.lukiot2 <- subset(hr.lukiot, min(HelsinkiMap$lat) <= lat & lat <= max(HelsinkiMap$lat) & min(HelsinkiMap$lon) <= lon & lon <= max(HelsinkiMap$lon))

# Plot prices and high schools
hplot2 <- hplot + geom_point(data=hri.dat2, aes(x=Lon, y=Lat, colour=Hinta.mediaani), size=2.5, alpha=0.8)
hplot2 <- hplot2 + scale_colour_gradient(low="blue", high="red")
hplot2 <- hplot2 + geom_point(data=hr.lukiot2, aes(x=lon, y=lat, size=Keskiarvo))
hplot2 <- hplot2 + scale_size_area(range=c(3,6), breaks=seq(15, 21, 2))
hplot2 <- hplot2 + geom_text(data=hr.lukiot2, aes(x=lon, y=lat, label=Ranking), colour="white", size=2)
hplot2 <- hplot2 + ggtitle("Pääkaupunkiseudun asuntojen neliöhinnat ja lukioiden paremmuus kartalla")
ggsave("Helsinki_price_highschools_20111010_final.png", plot=hplot2, width=10, height=9)
