# Functions for processing OpenStreetMap data
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
# Copyright 2012 Juuso Parkkinen, juuso.parkkinen@gmail.com. All rights reserved.

library(maptools)
library(rgdal)
library(rgeos)
library(ggplot2)

# Preprocess coastline shape file
# - Take subset based on given boundings box
# - Coordinate transformation
# - Fix continent polygon
PreprocessCoastline <- function(filename, bbox, fix.land=TRUE) {

  # Read file if valid
  message(paste("Reading coastline data from '", filename, "'",sep=""))
  coastline <- readShapeSpatial(filename)
  
  # Transform coordinates from KKJ2 to WGS84
  message("Transforming coordinates...")
  coastline@proj4string <- CRS("+init=epsg:3047")
  coastline <- spTransform(coastline, CRS("+proj=longlat +datum=WGS84"))
  
  # Fortify and take subset
  message("Fortifying...")
  coastline.df <- fortify(coastline)
  message("Taking subset within the bounding box...")
  coastline.df.subset <- droplevels(subset(coastline.df, long >= bbox["x","min"] & long <= bbox["x","max"] & lat >= bbox["y","min"] & lat <= bbox["y","max"]))
  
  if (fix.land) {
    message("Fixing the land polygon...")
    # Separate the largest polygon object (=continent), and get it's boundaries
    land.id <- names(sort(table(coastline.df.subset$group), decreasing=T))[1]
    land.df <- subset(coastline.df.subset, group==land.id)
    min.id <- which.min(land.df$long)
    max.id <- which.max(land.df$long)
    
    # Add the corner points of the bounding box to the continent object
    aux.df <- data.frame(long=bbox["x",], lat=bbox["y","max"], order=NA,
                         hole=FALSE, piece="1", group=land.id, id="208")
    if (min.id==nrow(land.df) | max.id==nrow(land.df))
      land.df <- rbind(land.df, aux.df)
    else if (min.id < max.id)
      land.df <- rbind(land.df[1:min.id,], aux.df, land.df[(min.id+3):nrow(land.df),])
    else 
      land.df <- rbind(land.df[1:max.id,], aux.df, land.df[(max.id+3):nrow(land.df),])
    
    # Replace the old continent polygon with the new one
    coastline.df.subset <- coastline.df.subset[-which(coastline.df.subset$group==land.id),]
    coastline.df.subset <- rbind(coastline.df.subset, land.df)
  }
  message("DONE")
  return(coastline.df.subset)
}

# Preprocess OpenStreetMap shape file
# - Take subset based on given boundings box
PreprocessOpenStreetMap <- function(filename, bbox) {

  # Read file if valid
  if (file.exists(filename))
    message(paste("Reading OpenStreetMap data from '", filename, "'",sep=""))
  else
    stop(paste("File '", filename, "' does not exist.\nYou can download the data from 'http://download.geofabrik.de/osm/europe/'",sep=""))
  shape.dat <- readShapeSpatial(filename)
  
  # Determine object type
  if  (length(grep("Polygons", class(shape.dat)))>0)
    shape.type <- "Polygons"
  else if (length(grep("Lines", class(shape.dat)))>0)
    shape.type <- "Lines"
  else if (length(grep("Points", class(shape.dat)))>0)
    stop("Shape type 'points' not implemented yet!")
  else
    stop("Uknown shape type!")
  
  # Extract coordinate information for the polygons
  message("Taking subset within the bounding box...")
  if (shape.type=="Polygons")
    coords.all <- lapply(shape.dat@polygons, function(x) x@Polygons[[1]]@coords)
  if (shape.type=="Lines")
    coords.all <- lapply(shape.dat@lines, function(x) x@Lines[[1]]@coords)
  
  # Get objects within the given bounding box (based on starting point)
  coords <- sapply(coords.all, function(x) x[1,])  
  coords.in.bbox <- which(coords[1,] > bbox["x","min"] & coords[1,] < bbox["x","max"] & coords[2,] > bbox["y","min"] & coords[2,] < bbox["y","max"])
  shape.subset <- shape.dat[coords.in.bbox,]
  shape.subset@bbox <- bbox
  
  # Fortify and add OSM type info to the df
  message("Fortifying...")
  shape.df <- fortify(shape.subset)
  if (shape.type=="Polygons")
    shape.df$type <- shape.subset@data$type[match(shape.df$id, shape.subset@data$osm_id)]
  if (shape.type=="Lines") 
    shape.df$type <- shape.subset@data$type[match(shape.df$id, sapply(shape.subset@lines, function(x) x@ID))]
  
  message("DONE")
  return(shape.df)
}


