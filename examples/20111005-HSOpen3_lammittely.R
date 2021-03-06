# This script is part of the Louhos-project (http://louhos.github.com/)

# Copyright (C) 2010-2013 Juuso Parkkinen.
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
library(XML)
library(gdata)  

# Get map of Helsinki (takes some time)
Helsinki.center <- c(lon=24.93, lat = 60.20)
HelsinkiMap <- sorvi::GetStaticmapGoogleMaps(center = Helsinki.center, zoom = 11, maptype="Map", scale=1)
library(ggplot2)
theme_set(sorvi::GetThemeMap())
hplot <- ggplot(HelsinkiMap, aes(x=lon, y=lat))
hplot <- hplot + geom_tile(aes(fill=fill)) + scale_fill_identity(guide="none")
hplot <- hplot + scale_x_continuous('Longitude') + scale_y_continuous('Latitude')
hplot <- hplot + ggtitle("Map of Helsinki")

# Read coordinates for places in Helsinki Region
# KML files downloaded from: http://www.hri.fi/fi/data/paakaupunkiseudun-aluejakokartat/
# KML to CSV conversion with http://choonchernlim.com/kmlcsv/
message("Reading PKS_pienalue data from dropbox...")
pienalue <- read.csv("http://dl.dropbox.com/u/792906/data/PKS_pienalue_piste.csv", header=F)
names(pienalue) <- c("lon", "lat", "Alue", "")

# Get Oikotie myynnit data
Oikotie <- sorvi::GetOikotie()
hr.myynnit <- Oikotie$hr.myynnit

# Compute average prices per square meter for Helsinki zip codes
# Helsinki.m2.prices <- aggregate(Helsinki.myynnit$Price.per.square, list(Helsinki.myynnit$Zip.code), mean)
# names(Helsinki.m2.prices) <- c("Zip.code", "Price")
Helsinki.m2.prices <- aggregate(hr.myynnit$Price.per.square, list(hr.myynnit$Zip.code), mean)
names(Helsinki.m2.prices) <- c("Zip.code", "Price")

################################################
## Connect Helsinki zip codes to region names ##
################################################

# Load older housing pricing data to get Helsinki Zip codes (link to data obtained from Helsinki Region Infoshare)
data.url <- "http://www.hel2.fi/tietokeskus/data/helsinki/helsingin_kaupungin_tilastollinen_vuosikirja_2009/3asuminen/3.24.xls"
asuntodata <- gdata::read.xls(data.url, skip=5, header=T, fileEncoding="ISO-8859-1")

# Use only area and zip code information
asuntodata <- asuntodata[-c(1,75,76),-(3:8)]
names(asuntodata) <- c("Postinumero", "Alue")
asuntodata$Postinumero <- paste("00", as.vector(asuntodata$Postinumero), "0", sep="")

# Extract Finnish area names
# Need to fix Scandinavian characters, as I can't get fileEncoding to work with read.xls...
temp <- gsub("\xe4", "ä", as.vector(asuntodata$Alue))
temp <- gsub("\xf6", "ö", temp)
temp <- sapply(strsplit(temp, split=" - "), function(x) x[1])
asuntodata$Alue <- temp

#################################
## Combine everything together ##
#################################

# Get those zip codes for which we have prices
Postinumerot <- as.vector(Helsinki.m2.prices$Zip.code[Helsinki.m2.prices$Zip.code %in% asuntodata$Postinumero])

# Get area names for those zip codes
Alueet <- as.vector(asuntodata$Alue)[match(Postinumerot, asuntodata$Postinumero)]

# Fixe some area names manually
fixed.areas <- c("Kamppi", "NA", "Etu-Töölö", "Meilahti", "NA",
                 "Vanha Munkkiniemi", "Kuusisaari", "Munkkivuori", "Vallila", "NA",
                 "Hermanni", "Vanhakaupunki", "Metsälä", "Maunula", "NA",
                 "Ala-Malmi", "Pukinmäki", "NA", "Latokartano", "NA",
                 "NA", "NA", "Itäkeskus", "NA", "Mellunmäki", 
                 "Keski-Vuosaari")
Alueet[!(Alueet %in% pienalue$Alue)] <- fixed.areas

# Remove still missing values
Postinumerot <- Postinumerot[-which(Alueet=="NA")]
Alueet <- Alueet[-which(Alueet=="NA")]
Pituuspiiri <- pienalue$lon[match(Alueet, pienalue$Alue)]
Leveyspiiri <- pienalue$lat[match(Alueet, pienalue$Alue)]
Neliöhinta <- Helsinki.m2.prices$Price[match(Postinumerot, Helsinki.m2.prices$Zip.code)]

# Construct a dataframe for plotting
df <- data.frame(Postinumero=Postinumerot, Alue=Alueet, lon=Pituuspiiri, lat=Leveyspiiri, Neliöhinta=Neliöhinta)

# Add region names and prices on top of plain Helsinki map
hplot2 <- hplot + geom_point(data=df, aes(x=lon, y=lat, size=Neliöhinta))
hplot2 <- hplot2 + geom_text(data=df, aes(x=lon, y=lat, label=Alue), size=1.5, hjust=1, vjust=2)
ggsave("Helsinki_prices_20111005.png", plot=hplot2, width=8, height=8)
