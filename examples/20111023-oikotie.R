# This script is posted to the Louhos-blog
# http://louhos.wordpress.com
# Copyright (C) 2008-2012 Juuso Parkkinen <juuso.parkkinen@gmail.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# Install and load necessary packages
install.packages(c("ggplot2", "gridExtra"))
library(ggplot2)
library(gridExtra)

# sorvi installation instructions: http://louhos.github.com/sorvi/asennus.html
library(sorvi)


###############
## Load data ##
###############


# Get Oikotie myynnit data
Oikotie <- GetOikotie()
hr.myynnit <- Oikotie$hr.myynnit

# Get Lukio data
Lukiot <- GetLukiot()
hr.lukiot <- Lukiot$hr.lukiot

# Load Helsinki region area data
HRI.aluejakokartat <- GetHRIaluejakokartat()
pks.df <- HRI.aluejakokartat$pienalue.df
pks.pienalue <- HRI.aluejakokartat$pienalue

######################
## Combine datasets ##
######################

# Get unique address combinations (street, zip code)
combs <- aggregate(hr.myynnit$Price.per.square, list(hr.myynnit$Zip.code, hr.myynnit$Street), median)
names(combs) <- c("Postinumero", "Katu", "Mediaanihinta")

# Get geocodes for the address combinations using GoogleMaps API
# NOTE! Can query only 2500 times per day
# Could use OpenStreetMap as well, see GetGeocodeOpenStreetMapmessage("Reading geocodes from dropbox...")
con <- url("http://dl.dropbox.com/u/792906/data/HR_geocodes_20111023.RData")
load(con)
close(con)

# hr.geo.codes <- list()
# for (i in 1:2500) { #First day
#   # Second day: for (i in (2500:nrow(combs))[-c(3699:3701-2499)]) { # Remove Tarkk'ampujankatu because it causes an error
#   if (i %% 100 == 0)
#     cat(i, ".")
#   temp <- get.geocode.GoogleMaps(paste(combs$Katu[i], combs$Postinumero[i], "FI", sep=", "))
#   hr.geo.codes[[i]] <- as.numeric(temp)
# }

# Add geocodes to combinations (use only clear geocodes where exactly two coordinates values given)
combs$Lat <- combs$Lon <- rep(NA, nrow(combs))
clears <- which(sapply(hr.geo.codes, length)==2)
combs$Lat[clears] <- sapply(hr.geo.codes[clears], function(x) x[1])
combs$Lon[clears] <- sapply(hr.geo.codes[clears], function(x) x[2])

# Get Helsinki region area boundaries (areas are represented as polygons in PKS aluejako data)
coords.list <- lapply(pks.pienalue@polygons, function(x) x@Polygons[[1]]@coords)

# Map address combinations to Helsinki region areas
area.inds <- rep(NA, length(clears))
message("Mapping address combinations to regions...")
for (i in 1:length(clears)) {
  if (i %% 100 == 0) message(round(i/length(clears), digits=2))
  gc <- hr.geo.codes[clears][[i]]
  temp <- sapply(coords.list, function(x) point.in.polygon(gc[2], gc[1], x[,1], x[,2]))
  if (any(temp==1))
    area.inds[i] <- which(temp==1)
}

# Compute median prices for each area in PKS aluejakodata
# Note! Can't use precomputed prices from combs, as the medians wouldn't end up right
area.median.prices <- area.mean.prices <- rep(NA, nrow(pks.pienalue))
message("Computing medien prices for regions...")
for (a in 1:length(area.median.prices)) {
  if (a %% 10 == 0) message(round(a/length(area.median.prices), digits=2))
  # Get all combinations that belong to current area (if any)
  if (any(area.inds==a, na.rm=T)) {
    comb.inds <- which(area.inds==a)
    temp.prices <- c()
    # Get all rows from original hr.myynnit that match to current area
    for (ci in comb.inds) {
      rows <- which(hr.myynnit$Street == combs$Katu[clears][ci] & hr.myynnit$Zip.code == combs$Postinumero[clears][ci])
      temp.prices <- c(temp.prices, hr.myynnit$Price.per.square[rows])
    }
    area.median.prices[a] <- median(temp.prices)
    area.mean.prices[a] <- mean(temp.prices)
  }
}

# Add price info to pks.df
pks.df$Mediaanihinta <- pks.df$Keskihinta <- NA
for (a in 1:length(area.median.prices)) {
  rows <- which(pks.df$id==as.character(a))
  if (length(rows) > 0) {
    pks.df$Mediaanihinta[rows] <- area.median.prices[a]
    pks.df$Keskihinta[rows] <- area.mean.prices[a]
  }
}

###################
## Plot on a map ##
###################

# Load map of Helsinki region from GoogleMaps
center <- c(lon=24.90, lat = 60.20)
hr.map <- GetStaticmapGoogleMaps(center = center, zoom = 10, GRAYSCALE=TRUE, maptype="map", scale=1)

# Construct plain map plot
theme_set(theme_bw())
hplot <- ggplot(hr.map)
hplot <- hplot + geom_tile(aes(x=lon, y=lat, fill=fill)) + scale_fill_identity(guide="none")
hplot <- hplot + scale_x_continuous('Longitude') + scale_y_continuous('Latitude')
hplot <- hplot + ggtitle("Map of Helsinki")
# ggsave("HR_map_20111023.png", plot=hplot, width=8, height=8)

# Add region boundaries (filter out sea regions)
pks.df2 <- subset(pks.df, !(Nimi %in% c("Ulkosaaret", "Länsisaaret", "Itäsaaret")))
hplot2 <- hplot + geom_path(data=pks.df2, aes(x=long, y=lat, group=id))
# ggsave("HR_map_areas_20111023.png", plot=hplot2,  width=8, height=8)

# Add apartment price info
# Need to train fill scale separately for price info, because static map already uses fill
den_fill_scale <- scale_colour_gradient(low = 'blue', high = 'red')
ggplot2:::scale_train(den_fill_scale, pks.df2$Mediaanihinta)
pks.df2$Mediaanihinta2 <- ggplot2:::scale_map(den_fill_scale, pks.df2$Mediaanihinta)

hplot3 <- hplot2 + geom_polygon(data=pks.df2, aes(x=long, y=lat, group=id, fill=Mediaanihinta2), colour="white", alpha=0.7, size=0.2)
# ggsave("Helsinki_map_areas_prices_20111023.png", plot=hplot3,  width=8, height=8)

# Add high school info (filter out schools not in map range)
hr.lukiot2 <- subset(hr.lukiot, min(hr.map$lat) <= lat & lat <= max(hr.map$lat) & min(hr.map$lon) <= lon & lon <= max(hr.map$lon))
hplot4 <- hplot3 + geom_point(data=hr.lukiot2, aes(x=lon, y=lat, colour=Keskiarvo), size=3)
hplot4 <- hplot4 + scale_colour_gradient2(low = 'yellow1', mid='greenyellow', high = 'green3', midpoint=mean(hr.lukiot2$Keskiarvo, guide="none"))
hplot4 <- hplot4 + ggtitle("Pääkaupunkiseudun asuntojen hinnat ja lukioiden paremmuus")
hplot4 <- hplot4 + geom_text(data=hr.lukiot2, aes(x=lon, y=lat, label=Ranking), size=1)
# ggsave("Helsinki_map_areas_prices_schools_prel_20111023.png", plot=hplot4,  width=8, height=8)

# Create a separate legend for the price scale (a bit tricky!)
p <- ggplot(data=pks.df2) + geom_polygon(data=pks.df2, aes(x=long, y=lat, group=id, fill=Mediaanihinta))
p <- p + geom_point(data=hr.lukiot2, aes(x=lon, y=lat, colour=Keskiarvo), size=3)
p <- p + scale_colour_gradient2(low = 'yellow1', mid='greenyellow', high = 'green3', midpoint=mean(hr.lukiot2$Keskiarvo))
p <- p + scale_fill_gradient(low="blue", high="red")
tmp <- ggplot_gtable(ggplot_build(p))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]

# Save final plot with custom legend (takes some time)
png("Helsinki_map_areas_prices_schools_20111023.png")
grid.arrange(arrangeGrob(hplot4 + theme(legend.position="none")), legend, widths=c(1, 0.2), nrow=1)
dev.off()
