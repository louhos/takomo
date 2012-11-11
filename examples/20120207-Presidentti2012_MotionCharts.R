# This script is posted to the Louhos-blog
# http://louhos.wordpress.com
# Copyright (C) 2008-2012 Juuso Parkkinen <juuso.parkkinen@gmail.com>. 
# All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

####################
## Load libraries ##
####################

# Plot Motion Chart using googleVis -package
if (!try(require(googleVis))) {
  install.packages("googleVis")
  library(googleVis)
}

#################
## VOTING DATA ##
#################

## Read 1st round votes from HS Next
votes1.url <- "http://www2.hs.fi/extrat/hsnext/presidentti1-tulos.csv"
votes1 <- read.csv(votes1.url, sep=";", fileEncoding="UTF-8")

# Fix column names ("osuus" and "aania" are mixed with each other)
names(votes1) <- gsub("osuus", "temp", names(votes1))
names(votes1) <- gsub("ääniä", "osuus", names(votes1))
names(votes1) <- gsub("temp", "ääniä", names(votes1))
votes1$Ääniä.yhteensä <- as.numeric(as.vector(gsub("None", "0", votes1$Ääniä.yhteensä)))

# Read 2nd round votes from HS Next
votes2.url <- "http://www2.hs.fi/extrat/hsnext/presidentti2.csv"
votes2 <- read.csv(votes2.url, sep=";", fileEncoding="ISO-8859-15")

# Here the names are ok, but ',' has been used as the decimal separator
bad.cols <- c(3,4,7,9,11,13,15)
votes2[,bad.cols] <- apply(votes2[,bad.cols], 2, function(x) as.numeric(gsub(",", ".", x)))

# Rows in votes1 and votes2 match perfectly with one exception:
# votes1 is missing row 1995: 499021 Köklot
# As we are now not interested in it, we simply remove it from 
# votes2 to make merging these two easier
votes2 <- droplevels(votes2[-1995,])

# Refine variable names
names(votes1) <- gsub("\\.", " ", names(votes1))
names(votes2) <- gsub("\\.", " ", names(votes2))
names(votes1)[3:39] <- paste("1.K", names(votes1)[3:39], sep=" ")
names(votes2)[3:15] <- paste("2.K", names(votes2)[3:15], sep=" ")


#####################
## Helsinki Region ##
#####################

# Combine data for Helsinki region (Helsinki, Espoo, Vantaa, include Kauniainen to Espoo)
helsinki.inds <- 4:160
espoo.inds <- c(169:236, 355)
vantaa.inds <- 245:309
pks.rows <- c(helsinki.inds, espoo.inds, vantaa.inds)
pks1 <- droplevels(votes1[pks.rows,])
pks2 <- droplevels(votes2[pks.rows,])
# Check that the areas are the same
all(pks1$Alue==pks2$Alue)

# Merge based on Alue
pks.votes <- merge(pks1[2:length(pks1)], pks2[2:length(pks2)], by="Alue")

# Keep only the total percentage to keep the number of variables sensible
pks.votes <- pks.votes[-c(grep("ennakko", names(pks.votes)))]
pks.votes <- pks.votes[-c(grep("ääniä", names(pks.votes)))]

# Add auxiliary variable Aika as the second column 
# NOTE! Time variable needs to be the second columnd for googleVis
pks.votes <- cbind(pks.votes[1], Aika=2012, pks.votes[2:length(pks.votes)])

# Add city for colouring (note that merge has alphabetized the Alue names, updated 8.2.2012)
pks.votes$Kaupunki <- "Helsinki"
pks.votes$Kaupunki[pks.votes$Alue %in% pks1$Alue[match(espoo.inds, pks.rows)]] <- "Espoo"
pks.votes$Kaupunki[pks.votes$Alue %in% pks1$Alue[match(vantaa.inds, pks.rows)]] <- "Vantaa"

mchart.pks <- gvisMotionChart(pks.votes, idvar="Alue", timevar="Aika", options=list(height=600, width=700))
# Plot immediately (opens in browser)
plot(mchart.pks)
# Save as html (needs javascript to open!)
print(mchart.pks, file="Presidentti2012_MotionChart_PKS_20120207.html")


####################
## Municipalities ##
####################

# Read info of municipalities and election areas from Tilastoteskus
library(XML)
url <- "http://www.stat.fi/meta/luokitukset/vaalipiiri/001-2012/luokitusavain_kunta.html"
temp <- readHTMLTable(url)

# Extract info that we want
municipalities <- temp[[1]][-1,]
municipalities$Vaalipiiri <- paste(as.vector(municipalities[,1]), as.vector(municipalities[,2]))
municipalities <- municipalities[3:5]
names(municipalities) <- c("Aluenumero", "Alue", "Vaalipiiri")

# Fill missing Vaalipiiri info
current.piiri <- NA
for (i in 1:nrow(municipalities)) {
  # If vaalipiiri given, save it as current
  if (municipalities[i,"Vaalipiiri"]!=" ")
    current.piiri <- as.vector(municipalities[i,"Vaalipiiri"])
  # Else add current vaalipiiri
  else
    municipalities[i,"Vaalipiiri"] <- current.piiri
}

# Have to rename two places to match the HS Next data
levels(municipalities$Alue)[levels(municipalities$Alue)=="Parainen"] <- "Länsi-Turunmaa"
levels(municipalities$Alue)[levels(municipalities$Alue)=="Maarianhamina - Mariehamn"] <- "Maarianhamina"

# municipalities and votes1 match perfectly based on Alunumero, 
# but votes2 doesn't, so we can't use the merge() -function
# Instead, we first get the rows of the municipalities based on votes1 and Aluenumero
mun.rows <- match(municipalities$Aluenumero, votes1$Aluenumero)
# Then check that the rows match for votes1 and votes2
all(as.vector(votes1$Alue[mun.rows])== as.vector(votes2$Alue[mun.rows]))

# Now we can manually merge the three datasets
mun.votes <- droplevels(cbind(municipalities[2:3], votes1[mun.rows,3:length(votes1)], votes2[mun.rows,3:length(votes2)]))

# Keep only the total percentage to keep the number of variables sensible
mun.votes <- mun.votes[-c(grep("ennakko", names(mun.votes)))]
mun.votes <- mun.votes[-c(grep("ääniä", names(mun.votes)))]

# Add auxiliary variable Aika as the second column 
# NOTE! Time variable needs to be the second columnd for googleVis
mun.votes <- cbind(mun.votes[1], Aika=2012, mun.votes[2:length(mun.votes)])

# Plot a Motion Chart using googleVis -package
mchart.mun <- gvisMotionChart(mun.votes, idvar="Alue", timevar="Aika", options=list(height=600, width=700))
# Plot immediately (opens in browser)
plot(mchart.mun)
# Save as html (needs javascript to open!)
print(mchart.mun, file="Presidentti2012_MotionChart_Municipalities_20120207.html")
