# This script is part of the Louhos-project (http://louhos.github.com/)

# Copyright (C) 2010-2013 Leo Lahti and Juuso Parkkinen.
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
# Remember to install required packages (e.g. 'install.packages("rgdal")')
library(rgdal)
library(gpclib)
library(ggplot2)
library(gridExtra) 

################# 
## VOTING DATA ## 
################# 

## Read voting results from HS Next 
votes.url <- "http://www2.hs.fi/extrat/hsnext/presidentti1-tulos.csv" 
votes <- read.csv(votes.url, sep=";", fileEncoding="UTF-8")

# Fix column names ("osuus" and "aania" are mixed with each other) 
names(votes) <- gsub("osuus", "temp", names(votes)) 
names(votes) <- gsub("ääniä", "osuus", names(votes)) 
names(votes) <- gsub("temp", "ääniä", names(votes)) 

## Read voting area data from HKK (Helsingin kaupungin kiinteistovirasto) 

areas <- sorvi::GetHKK(which.data="Aanestysaluejako", data.dir="TEMP") 

# Create new Aluenumero code from TKTUNNUS for areas data (discard the first 2
# digits)
areas@data$Aluenumero <- sapply(areas@data$TKTUNNUS, function(x) substr(as.character(x), 3, nchar(as.character(x))))

# Check that Aluenumero is found also in the voting data
if(!all(areas@data$Aluenumero %in% votes$Aluenumero))
  stop("DAMN!")

# Merge voting data to area data based on Aluenumero
areas@data <- merge(areas@data, votes, by="Aluenumero")

# Set the projection right and reproject to WS84
areas@proj4string <- sp::CRS("+init=epsg:2392")
areas <- spTransform(areas, CRS("+proj=longlat +datum=WGS84"))

# Map the cities from code to name
city.codes  <- list("091"="Helsinki", "049"="Espoo", "235"="Kauniainen", "092"="Vantaa")
areas[["Kuntanimi"]] <- sapply(as.character(areas[["KUNTA"]]), function(x) city.codes[[x]])
areas@data$Kuntanimi <- factor(areas@data$Kuntanimi)

# Split the spatial data into respective cities
areas.cities <- sorvi::SplitSpatial(areas, "Kuntanimi")


############## 
## MAP PLOT ## 
############## 

# Get the data frame
maptools::gpclibPermit()
areas.df <- ggplot2::fortify(areas, region="Aluenumero")

# Add support for Pekka and Sauli 
areas.df$Pekka.Haavisto.osuus <- votes$Pekka.Haavisto.osuus[match(areas.df$id, votes$Aluenumero)] 
areas.df$Sauli.Niinistö.osuus <- votes$Sauli.Niinistö.osuus[match(areas.df$id, votes$Aluenumero)] 

# Use sorvi's map theme
theme_set(sorvi::GetThemeMap()) 

# Get map of Helsinki for background 
Helsinki.center <- c(lon=24.93, lat = 60.20) 
HelsinkiMap <- sorvi::GetStaticmapGoogleMaps(center = Helsinki.center, zoom = 10, GRAYSCALE=TRUE, maptype="Map", scale=1) 
hplot <- ggplot(HelsinkiMap, aes(x=lon, y=lat)) 
hplot <- hplot + geom_tile(aes(fill=fill)) + scale_fill_identity(guide="none") 
hplot <- hplot + xlab(NULL) + ylab(NULL) 

# Create first a common colour scale 
min.val <- min(areas.df$Pekka.Haavisto.osuus, areas.df$Sauli.Niinistö.osuus) 
max.val <- max(areas.df$Pekka.Haavisto.osuus, areas.df$Sauli.Niinistö.osuus) 
col.scale <- scale_colour_gradient(low = 'blue', high = 'red', limits=c(min.val, max.val)) 
fill.scale <- scale_fill_gradient(low = 'blue', high = 'red', limits=c(min.val, max.val)) 

# Make map for Pekka with custom colour scale
den_fill_scale <- col.scale 
ggplot2:::scale_train(den_fill_scale, areas.df$Pekka.Haavisto.osuus)
areas.df$Pekka <- ggplot2:::scale_map(den_fill_scale, areas.df$Pekka.Haavisto.osuus)
hplot.pekka <- hplot + geom_polygon(data=areas.df, aes(x=long, y=lat, group=id, fill=Pekka), colour="white", alpha=0.7, size=0.2) + ggtitle("Pekka Haavisto")

# Make map for Sauli with custom colour scale
den_fill_scale <- col.scale 
ggplot2:::scale_train(den_fill_scale, areas.df$Sauli.Niinistö.osuus)
areas.df$Sauli <- ggplot2:::scale_map(den_fill_scale, areas.df$Sauli.Niinistö.osuus)
hplot.sauli <- hplot + geom_polygon(data=areas.df, aes(x=long, y=lat, group=id, fill=Sauli), colour="white", alpha=0.7, size=0.2) + ggtitle("Sauli Niinistö")

# Add legend using an auxiliary ggplot object 
p <- ggplot(data=areas.df) + geom_polygon(data=areas.df, aes(x=long, y=lat, group=id, fill=Pekka.Haavisto.osuus)) + fill.scale + labs(fill="Osuus äänistä (%)") 
tmp <- ggplot_gtable(ggplot_build(p))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]

# Plot together
png("Presidentti2012_PKS_Haavisto-Niinisto_20120207.png", width=1000, height=450)
grid.arrange(arrangeGrob(hplot.pekka + theme(legend.position="none"), hplot.sauli + theme(legend.position="none"), nrow=1), legend, widths=c(1, 0.1), nrow=1)
dev.off()

