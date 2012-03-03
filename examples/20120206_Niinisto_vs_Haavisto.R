# This script is posted to the Louhos-blog # http://louhos.wordpress.com 
# Copyright (C) 2008-2012 Juuso Parkkinen &lt;juuso.parkkinen@gmail.com&gt;. All rights reserved. 
# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses 
# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
# Install soRvi package # Instructions in http://sorvi.r-forge.r-project.org/asennus.html 
# This script was implemented with soRvi version 0.1.45 

library(sorvi) 

################# 
## VOTING DATA ## 
################# 

## Read voting results from HS Next 
votes.url <- "http://www2.hs.fi/extrat/hsnext/presidentti1-tulos.csv" 
votes <- read.csv(votes.url, sep=";") 

# Fix column names ("osuus" and "aania" are mixed with each other) 
names(votes) <- gsub("osuus", "temp", names(votes)) 
names(votes) <- gsub("ääniä", "osuus", names(votes)) 
names(votes) <- gsub("temp", "ääniä", names(votes)) 

## Read voting area data from HKK (Helsingin kaupungin kiinteistovirasto) 
library(rgdal) 
areas <- GetHKK(which.data="Aanestysaluejako", data.dir="TEMP") 

# Create new Aluenumero code from TKTUNNUS for areas data (discard the first 2
# digits)
areas@data$Aluenumero <- sapply(areas@data$TKTUNNUS, function(x) substr(as.character(x), 3, nchar(as.character(x))))

# Check that Aluenumero is found also in the voting data
if(!all(areas@data$Aluenumero %in% votes$Aluenumero))
  stop("DAMN!")

# Merge voting data to area data based on Aluenumero
areas@data <- merge(areas@data, votes, by="Aluenumero")

# Set the projection right and reproject to WS84
areas@proj4string <- CRS("+init=epsg:2392")
areas <- spTransform(areas, CRS("+proj=longlat +datum=WGS84"))

# Map the cities from code to name
city.codes  <- list("091"="Helsinki", "049"="Espoo", "235"="Kauniainen", 
                    "092"="Vantaa")
areas[["Kuntanimi"]] <- sapply(as.character(areas[["KUNTA"]]), function(x) city.codes[[x]])
areas@data$Kuntanimi <- factor(areas@data$Kuntanimi)

# Split the spatial data into respective cities
areas.cities <- SplitSpatial(areas, "Kuntanimi")

############## 
## MAP PLOT ## 
############## 

library(ggplot2) 
library(gridExtra) 

# Get the data frame
areas.df <- fortify(areas, region="Aluenumero")

# Add support for Pekka and Sauli 
areas.df$Pekka.Haavisto.osuus <- votes$Pekka.Haavisto.osuus[match(areas.df$id, votes$Aluenumero)] 
areas.df$Sauli.Niinistö.osuus <- votes$Sauli.Niinistö.osuus[match(areas.df$id, votes$Aluenumero)] 

# Create a blank background for the maps 
theme_map <- theme_bw() 
theme_map$panel.background <-  theme_blank() 
theme_map$panel.grid.major <- theme_blank() 
theme_map$panel.grid.minor <- theme_blank() 
theme_map$axis.ticks <- theme_blank() 
theme_map$axis.text.x <- theme_blank() 
theme_map$axis.text.y <- theme_blank() 
theme_map$axis.title.x <- theme_blank() 
theme_map$axis.title.y <- theme_blank() 
theme_set(theme_map) 

# Get map of Helsinki for background 
Helsinki.center <- c(lon=24.93, lat = 60.20) 
HelsinkiMap <- GetStaticmapGoogleMaps(center = Helsinki.center, zoom = 10, 
                                      GRAYSCALE=TRUE, maptype="Map", scale=1) 
hplot <- ggplot(HelsinkiMap, aes(x=lon, y=lat)) 
hplot <- hplot + geom_tile(aes(fill=fill)) + scale_fill_identity(legend=FALSE) 
hplot <- hplot + xlab(NULL) + ylab(NULL) 

# Create first a common colour scale 
min.val <- min(areas.df$Pekka.Haavisto.osuus, areas.df$Sauli.Niinistö.osuus) 
max.val <- max(areas.df$Pekka.Haavisto.osuus, areas.df$Sauli.Niinistö.osuus) 
col.scale <- scale_colour_gradient(low = 'blue', high = 'red', 
                                   limits=c(min.val, max.val)) 
fill.scale <- scale_fill_gradient(low = 'blue', high = 'red', 
                                  limits=c(min.val, max.val)) 

# Make map for Pekka 
den_fill_scale <- col.scale 
den_fill_scale$train(areas.df$Pekka.Haavisto.osuus, T) 
areas.df$Pekka <- den_fill_scale$map(areas.df$Pekka.Haavisto.osuus) 
hplot.pekka <- hplot + geom_polygon(data=areas.df, aes(x=long, y=lat, group=id, 
                                                       fill=Pekka), colour="white", 
                                    alpha=0.7, size=0.2) 

# Add legend using an auxiliary ggplot object 
p <- ggplot(data=areas.df) + geom_polygon(data=areas.df, 
                                          aes(x=long, y=lat, group=id, 
                                                             fill=Pekka.Haavisto.osuus)) + 
                                                               fill.scale + labs(fill="Osuus äänistä (%)") 
leg <- ggplotGrob(p + opts(keep="legend_box")) 
legend <- gTree(children=gList(leg), cl="legendGrob") 
widthDetails.legendGrob <- function(x) unit(3, "cm") 
hplot.pekka <- arrangeGrob(hplot.pekka, legend=legend, main="Pekka Haavisto") 

# Make map for Sauli and use the same legend
den_fill_scale <- col.scale 
den_fill_scale$train(areas.df$Sauli.Niinistö.osuus, T) 
areas.df$Sauli <- den_fill_scale$map(areas.df$Sauli.Niinistö.osuus) 
hplot.sauli <- hplot + geom_polygon(data=areas.df, aes(x=long, y=lat, group=id, 
                                                       fill=Sauli), 
                                    colour="white", alpha=0.7, size=0.2) 
hplot.sauli <- arrangeGrob(hplot.sauli, legend=legend, main="Sauli Niinistö") 

# Save together 
both.plot <- arrangeGrob(hplot.pekka, hplot.sauli, nrow=1) 
ggsave(both.plot, file="vaalit/Presidentti2012_PKS_Haavisto-Niinisto_20120207.png", 
       width=20, height=9)