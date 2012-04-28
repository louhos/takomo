# Script for processing OpenStreetMap data
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
# Copyright 2012 Juuso Parkkinen, juuso.parkkinen@gmail.com. All rights reserved.

# This script takes raw OpenStreetMap shapefiles and produces a subset of the data for
# a given region, defined by a bounding box. The output is given in ggplot2-compatible
# data frames, but you can also use the shape objects

library(sorvi)

# Functions for preprocessing the OSM data
source("OSM_functions_20120310.R")

# Define path for your data folder
data.folder <- "~/tmp/"

# Set bounding box
# NOTE! The larger the box, the longer the script will run!
bbox.Helsinki.center <- rbind("x"=c("min"=24.895, "max"=24.98),
                              "y"=c("min"=60.145, "max"=60.19))
#bbox.Helsinki.mid <- rbind("x"=c("min"=24.80, "max"=25.00),
#                              "y"=c("min"=60.10, "max"=60.25))
#bbox.Helsinki.region <- rbind("x"=c("min"=24.45, "max"=25.30),
#                              "y"=c("min"=60.10, "max"=60.40))


## Read and preprocess coastline
# Southern Finland coastline shape file produced by Joona Lehtomäki
# File available in http://109.74.199.6:8080/louhos/SK_rantaviiva.zip
# Originally in: http://latuviitta.org/documents/PK_seutukartta_2012_Spatialite.zip
# License: http://dl.dropbox.com/u/599386/Seutukartan%20avoimen%20datan%20lisenssi_1_11_2011.pdf
coastline.filename <- paste(data.folder, "SK_meren_rantaviiva_poly/SK_meren_rantaviiva_poly.shp",sep="")
coastline.df <- PreprocessCoastline(coastline.filename, bbox.Helsinki.center, fix.land=T)
# Check that it's ok! Try also with 'fix.land=F'
ggplot(hel.coastline.df, aes(x=long, y=lat)) + geom_polygon(aes(group=group), fill="grey60")


## Read OpenStreetMap objects
# OSM data dumps available in http://download.geofabrik.de/osm/europe/

# Fairly light ones to test the function
landuse.filename <- paste(data.folder, "finland.shp/landuse.shp", sep="")
landuse.df <- PreprocessOpenStreetMap(landuse.filename, bbox.Helsinki.center)

railways.filename <- paste(data.folder, "finland.shp/railways.shp", sep="")
railways.df <- PreprocessOpenStreetMap(railways.filename, bbox.Helsinki.center)

# Heavy stuff, takes tens of minutes to process
buildings.filename <- paste(data.folder, "finland.shp/buildings.shp", sep="")
buildings.df <- PreprocessOpenStreetMap(buildings.filename, bbox.Helsinki.center)

roads.filename <- paste(data.folder, "finland.shp/roads.shp", sep="")
roads.df <- PreprocessOpenStreetMap(roads.filename, bbox.Helsinki.center)

load("roads_temp.df")

# Save together for later plotting
save(bbox.Helsinki.center, coastline.df, buildings.df, roads.df, natural.df, landuse.df, 
     file=paste(data.folder, "OSM_preprocessed_Helsinki_center_20120310.RData", sep=""))

## Construct an example map plot
library(ggplot2)
library(sorvi)
theme_set(GetThemeMap())
map.plot <- ggplot(coastline.df, aes(x=long, y=lat)) + geom_polygon(aes(group=group), fill="grey70")
map.plot <- map.plot + geom_polygon(data=buildings.df, aes(group=group), fill="grey30")
map.plot <- map.plot + geom_line(data=subset(roads.df, (!type %in% c("footway", "cycleway", "path"))), 
                                 aes(x=long, y=lat, group=group), colour="grey20", size=0.3)
map.plot <- map.plot + geom_polygon(data=subset(natural.df, type %in% c("park")), 
                                      aes(x=long, y=lat, group=group), fill="darkgreen")
# Set the limits to match the bounding box
# map.plot <- map.plot + xlim(Hbox["x","min"], Hbox["x","max"]) + ylim(Hbox["y","min"], Hbox["y","max"])


