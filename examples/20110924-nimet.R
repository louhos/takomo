# This script is posted to the Louhos-blog
# http://louhos.wordpress.com
# Copyright (C) 2008-2011 Juuso Parkkinen <juuso.parkkinen@gmail.com>. All rights reserved.

# Tested with soRvi version 0.1.42
# http://sorvi.r-forge.r-project.org/asennus.html

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


###############
## Read data ##
###############

# Use the XML library to load the name data from Väestörekisterikeskus
library(XML)
nametables <- list()
for (year in 1:14) {
theurl <- paste("http://verkkopalvelu.vrk.fi/Nimipalvelu/nimipalvelu_etunimitop.asp?vuosi=",year,"&L=1",sep="")
tables <- readHTMLTable(theurl)
year.val <- unlist(strsplit(as.character(as.vector(tables[[2]]$V2)), split=" "))[3]
nametables[[year]] <- tables[[3]][2:11,1:5]
names(nametables)[year] <- year.val
}

#####################
## Preprocess data ##
#####################

library(ggplot2)
library(gdata)

# Melt to a data frame
temp <- melt(nametables, id.vars=c("V1", "V4"), measure.vars=c("V2", "V5"))

# Change name counts and years to numeric form, merge years 2010 and 2011
temp$value <- as.numeric(as.vector(temp$value))
temp$year <- c(seq(1890, 2010, 10), 2010)[match(temp$L1, unique(temp$L1))]

# Get subsets for men and women
men <- subset(temp, variable=="V2")[,c(1,4,5,6)]
women <- subset(temp, variable=="V5")[,c(2,4,5,6)]
names(men) <- names(women) <- c("Nimi", "Maara", "Vuosi.char", "Vuosi")
men.sum <- aggregate(men$Maara, by=list(men$Nimi), sum)
women.sum <- aggregate(women$Maara, by=list(women$Nimi), sum)

# Get top 10 names for both genders
men.top10 <- men[men$Nimi %in% men.sum[order(men.sum[,2], decreasing=T),1][1:10],]
men.top10$Nimi <- drop.levels(men.top10$Nimi)
women.top10 <- women[women$Nimi %in% women.sum[order(women.sum[,2], decreasing=T),1][1:10],]
women.top10$Nimi <- drop.levels(women.top10$Nimi)

# Add missing zeros (run for either men.top10 or women.top10)
add.zeros <- function(dat) {
Names <- levels(dat$Nimi)
Years <- unique(dat$Vuosi)
# For each, add zeros for missing names
for (y in Years) {
to.add <- which(!(Names %in% dat$Nimi[dat$Vuosi==y]))
temp.data <- data.frame(Nimi=Names[to.add], Maara=rep(0, length(to.add)),
Vuosi.char=rep(NA, length(to.add)), Vuosi=rep(y, length(to.add)))
dat <- rbind(dat, temp.data)
}
return(dat)
}

###############
## Plot data ##
###############

# Plot with geom_area
p <- ggplot(add.zeros(men.top10), aes(x=Vuosi, y=Maara, fill=Nimi))
p <- p + geom_area(position="Stack", colour="black") + ylab("Määrä")
#ggsave("Pojat_top10_area_20110919.png", plot=p)
png("Pojat_top10_area_20110919.png")
print(p)
dev.off()

# Plot for girls as well
p <- ggplot(add.zeros(women.top10), aes(x=Vuosi, y=Maara, fill=Nimi))
p <- p + geom_area(position="Stack", colour="black") + ylab("Määrä")
#ggsave("Tytöt_top10_area_20110919.png", plot=p)
png("Tytöt_top10_area_20110919.png")
print(p)
dev.off()

# Plot with geom_density. Looks nice, but the y-axis isn't exactly intuitive...
p <- ggplot(men.top10, aes(x=Vuosi, weight=Maara, y=..count.., fill=Nimi))
p <- p + geom_density(position="stack") + ylab("Määrä")
#ggsave("Pojat_top10_density_20110919.png", plot=p)
png("Pojat_top10_density_20110919.png")
print(p)
dev.off()

