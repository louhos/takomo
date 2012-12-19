# This script is posted to Louhos blog: http://louhos.wordpress.com
# Copyright (C) 2008-2012 Juuso Parkkinen and Leo Lahti
# Contact: <louhos@googlegroups.com>
# All rights reserved.

# This program is open source software; you can redistribute it 
# and/or modify it under the terms of the FreeBSD License 
# (keep this notice): http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# Install sorvi R package
# http://louhos.github.com/sorvi
# This script was implemented with soRvi version 0.2.13

# This script was inspired by the visualization at:
# https://sites.google.com/site/tiedonlouhintaa/


library(sorvi)

a <- try(library(XML)) 
if (a == "try-error") {
  install.packages("XML")
  library(XML)
}

a <- try(library(reshape)) 
if (a == "try-error") {
  install.packages("reshape")
  library(reshape)
}


# Retrieve the data / Lataa aineistot

# Read voting results for the first and second election round
# Lue kuntatason aanestystulokset 1. ja 2. vaalikierrokselle
votes1 <- GetElectionResultsPresidentti2012(election.round = 1, 
       	  				    level = "municipalities")
votes2 <- GetElectionResultsPresidentti2012(election.round = 2, 
       	  				    level = "municipalities")

# Get municipality information from Tilastokeskus
# Hae kuntatason perustilastot Tilastokeskukselta
municipality.info <- GetPXTilastokeskus("http://pxweb2.stat.fi/Database/Kuntien%20perustiedot/Kuntien%20perustiedot/Kuntaportaali.px")

# Process and match the data sets / Yhdista datat

# Modifications required to match Tilastokeskus and Election data
municipality.info$Alue <- sapply(strsplit(as.character(municipality.info$Alueluokitus.2012), " - "), function (x) {x[[1]]})
municipality.info[municipality.info$Alue == "Hämeenkyrö-Tavastkyro", "Alue"] <- "Hämeenkyrö"
municipality.info[municipality.info$Alue == "Mantta", "Alue"] <- "Mänttä-Vilppula"
municipality.info$Alue <- factor(municipality.info$Alue)
municipality.info$value <- municipality.info$dat
municipality.info <- reshape::cast(municipality.info[, c("Alue", "Alueluokitus.2012", "Tunnusluku", "value")], Alue ~ Tunnusluku) # Convert to wide format

# Combine voting results from both election rounds
votes <- cbind(votes1, votes2[, -c(1,2,3)])
# Remove pre-voting counts and absolute votes to focus on voting percentages
votes <- votes[-c(grep("ennakko", names(votes)))]
votes <- votes[-c(grep("aania",   names(votes)))]

# Merge Election and Municipality statistics by regions
tab <- merge(votes, municipality.info, by.x = "Alue")

# Add time variable, required as the second column by googleVis
tab <- cbind(tab[1], Aika = 2012, tab[-1])
# Remove region id number
tab[["Aluenumero"]] <- NULL

# Visualization
# Plot Motion Chart using googleVis package

a <- try(library(googleVis)) 
if (a == "try-error") {
  install.packages("googleVis")
  library(googleVis)
}

mchart.mun <- gvisMotionChart(tab, idvar = "Alue", 
	      			   timevar = "Aika", 
				   options = list(height = 600, width = 700))

# Plot 
plot(mchart.mun)

# Save as html (needs javascript to open!)
# print(mchart.mun, file = "Presidentti2012_MotionChart_20120212.html")

