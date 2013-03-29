# This script is posted to the Louhos-blog
# http://louhos.wordpress.com
# Copyright (C) 2008-2013 Louhos <louhos@googlegroups.com>. All rights reserved.

# Tested with sorvi version 0.2.13
# http://louhos.github.com/sorvi/asennus.html

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# Install soRvi package
# Instructions in http://louhos.github.com/sorvi/asennus.html
# NOTE! This script has been udpated 3.3.2013 to use latest sorvi version!
library(sorvi)

a <- try(library(rworldmap)) 
if (a == "try-error") {
  install.packages("rworldmap")
  library(rworldmap)
}

a <- try(library(rworldxtra)) 
if (a == "try-error") {
  install.packages("rworldxtra")
  library(rworldxtra)
}


library(rworldxtra)

# Load rgl library
a <- try(library("rgl")); 
if (a == "try-error") {install.packages("rgl"); library("rgl")}

# Load migration data for Finland
migration.dat <- GetWorldbankMigration("Finland")

# Load worldmap
a <- try(library("rworldmap")); 
if (a == "try-error") {install.packages("rworldmap"); library("rworldmap")}
#worldmap <- getMap(resolution="high")
worldmap <- getMap(resolution="li")

###############################################
# Plot migration data for Finland on worldmap #
###############################################

# Compute the total amount of migration in to and out from Finland
fin.in <- migration.dat$Finland$In
fin.out <- migration.dat$Finland$Out
fin.in[is.na(fin.in)] <- 0
fin.out[is.na(fin.out)] <- 0
migration.total <- rowSums(cbind(fin.in, fin.out), na.rm=T)

# Compute ratio indicating the direction of the migration in to (1) or out from (0) Finland
migration.ratio <- (migration.total - fin.out)/migration.total
migration.ratio[migration.ratio==Inf] <- 1

# Total amount of migration is mapped to the strength of the colours
# Use logarithmic scale to reduce the effect of very large numbers (that is, Sweden)
migration.total.log <- log(migration.total)
migration.total.log[migration.total.log==-Inf] <- 0
alpha <- migration.total.log/max(migration.total.log)

# Direction of migration is mapped to red and blue colours
in.ratio <- migration.ratio
out.ratio <- 1 - migration.ratio
in.ratio[is.nan(migration.ratio)] <- out.ratio[is.nan(migration.ratio)] <- 0

# Construct a rgb color scheme based on these values
cols.rgb <- rgb(red=in.ratio, green=0, blue=out.ratio, alpha=alpha)
cols.rgb[cols.rgb=="#00000000"] <- "grey90"
names(cols.rgb) <- migration.dat$CountryAlternative

# Initialize colours for all countries in the worldmap to light grey
names <- as.character(worldmap@data$NAME)
names[is.na(names)] <- "Unknown"
worldmap@data$NAME <- factor(names)
cols.countries <- rep("grey90", nrow(worldmap@data))
names(cols.countries) <- as.character(worldmap@data$NAME)

# Map countries in the migration data set to the worldmap and 
# update country colours
mapping <- match(names(cols.rgb), names(cols.countries))
cols.countries[mapping[!is.na(mapping)]] <- cols.rgb[!is.na(mapping)]
cols.countries["Finland"] <- "black"

# Plot the map with the final visualization
q <- spplot(worldmap, "NAME", col.regions = cols.countries, main = NULL, colorkey = FALSE, lwd = .4, col = "black")
#png("Finland_migration_20111116.png", width=2000, height=1000)
#par(mar=c(0,0,0,0))
print(q)
#dev.off()

##########################################
## Plot the map on an interactive globe ##
##########################################

# This produces 3D worldmap that can be rotated with mouse
# Ceased working, something has changed. Investigate later.

skip <- TRUE
if (!skip) {

  # Script copied from http://www.r-ohjelmointi.org/?p=906
  # Construct globe
  lat <- matrix(seq(90,-90, len=100)*pi/180, 100, 100, byrow=TRUE)
  long <- matrix(seq(-180, 180, len=100)*pi/180, 100, 100)
  r <- 6378.1 # radius of Earth in km
  x <- r*cos(lat)*cos(long)
  y <- r*cos(lat)*sin(long)
  z <- r*sin(lat)

  # Plot globe with rgl function using the migration statistics as texture
  # If rgl install failes, try the following first:
  # sudo apt-get -y install freeglut3 freegult3-dev
  library(rgl)
  open3d(windowRect=c(100, 100, 300, 300))
  clear3d("all")
  light3d()
  persp3d(x, y, z, col="white",
  texture="Finland_migration_20111116.png",
  specular="black", axes=FALSE, box=FALSE, xlab="", ylab="", zlab="",
  normal_x=x, normal_y=y, normal_z=z)
  par3d(userMatrix=rotationMatrix(-pi/2, 1, 0, 0)%*%
  rotationMatrix(-30/180*pi, 0, 0, 1)%*%
  rotationMatrix(45/180*pi, 1, 0, 0),
  zoom=2/3)
}
