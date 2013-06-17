# Suomen kuntien yhdistäminen ja visualisointi uuden kuntajaon mukaisesti

# This script is part of the Louhos-project (http://louhos.github.com/)

# Copyright (C) 2010-2013 Leo Lahti and Joona Lehtomäki.
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
# Remember to install required packages (e.g. 'install.packages("rgeos")')
library(XML)
library(rgeos)
library(rgdal)
library(maptools)
if (!maptools::gpclibPermit()) { maptools::gpclibPermit() }



# ----------------------------------------------

# Lue nykyiset kuntarajat Maanmittauslaitoksen aineistosta
# (C) MML 2011
fi.kunnat <- sorvi::LoadMML(data.id = "kunta1_p", resolution = "1_milj_Shape_etrs_shape") 

# Länsi-Turunmaan nimi vaihtui Paraisiksi vuoden 2012 alusta,
# lisää muutos dataan
# EDIT: Not needed anymore
# kunnat <- as.character(fi.kunnat$Kunta.FI)
# kunnat[[which(kunnat == "Länsi-Turunmaa")]] <- "Parainen"
# fi.kunnat$Kunta.FI <- factor(kunnat)

# ----------------------------------------------

# Lue uusi kuntajako tekstitiedostosta uusi.kuntajako.tab
# tai luo vaihtoehtoinen määrittelytiedosto
kuntajako.file <- "uusi.kuntajako.tab"

# Hae uusi kuntajako verkosta, ellei sitä ole määritelty työhakemistossa
if (!kuntajako.file %in% dir()) {
  download.file("http://antagomir.github.com/louhos/files/uusi.kuntajako.tab",
  destfile = kuntajako.file)
}

lines <- readLines(kuntajako.file)

uudet.kunnat <- list()
for (li in lines) {
  if (!li == "") {
    uusi.kunta <- strsplit(li, "\\:")[[1]][[1]]
    yhdistyvat <- Strip(strsplit(strsplit(li, "\\:")[[1]][[2]], "\\,")[[1]])
    uudet.kunnat[[uusi.kunta]] <- yhdistyvat
  }
}

map <- NULL
for (i in 1:length(uudet.kunnat)) {
  uusi <- names(uudet.kunnat)[[i]]
  vanhat <- uudet.kunnat[[i]]
  map <- rbind(map, cbind(rep(uusi, length(vanhat)), vanhat))
}
map <- as.data.frame(map)
colnames(map) <- c("Uusi", "Vanha")

# ----------------------------------------------

# Maarita uusi kunta kullekin nykyisen jaon mukaiselle kunnalle

# Listaa erikseen kunnat, joita ei ole lueteltu uudessa kuntajaossa:
unknown <- setdiff(fi.kunnat$Kunta.FI, map$Vanha)
map <- data.frame(list(Uusi = c(as.character(map$Uusi), unknown),
Vanha = c(as.character(map$Vanha), unknown)))

print(paste("Tiedoston", kuntajako.file,
"uudesta kuntajaosta puuttuvat kunnat:", paste(unknown, collapse = ",")))

nykyinen.kunta <- fi.kunnat$Kunta.FI
uusi.kunta <- droplevels(map$Uusi[match(fi.kunnat$Kunta.FI, map$Vanha)])

# ----------------------------------------------

# Yhdista nykyisten kuntien alueet uusi.kunta-muuttujan
# osoittamiin uusiin kuntiin
reg <- maptools::unionSpatialPolygons(fi.kunnat, uusi.kunta, avoidGEOS = T)

# Nimea uudet kunnat taulukkoon
attr <- data.frame(Uusi.kuntajako = names(reg))

# ----------------------------------------------

# Yhdista uudet alueet ja nimet / Merge into a SpatialPolygonsDataFrame
uusi.kuntajako <- sp::SpatialPolygonsDataFrame(reg, attr, match.ID = F)

# ----------------------------------------------

# Laske vakiluku uusille kunnille:
# Hae kuntatason tilastoja:
municipality.info <- sorvi::GetMunicipalityInfo()
vakiluku <- municipality.info[["Väkiluku 31.12.2012"]]
names(vakiluku) <- rownames(municipality.info)
uusi.vakiluku <- sapply(split(vakiluku[as.character(map$Vanha)], map$Uusi), sum, na.rm=TRUE)
uusi.kuntajako$Vakiluku <- uusi.vakiluku[as.character(uusi.kuntajako$Uusi.kuntajako)]

# ----------------------------------------------

# Visualisoi uusi kuntajako
pic <- sorvi::PlotShape(uusi.kuntajako, "Uusi.kuntajako", type = "discrete",
main = "Uusi kuntajako", ncol = 12, plot=FALSE)

# Vaihtoehtoinen visualisointi. Esitä uudet kunnat väreillä ja
# näytä nykyiset kuntarajat viivoilla.
fi.kunnat$uusi.kunta <- uusi.kunta
pic2 <- sorvi::PlotShape(fi.kunnat, "uusi.kunta", type = "discrete",
main = "Uusi kuntajako", ncol = 12, plot=FALSE)

# Visualisoi uusien kuntien vakiluku
pic3 <- sorvi::PlotShape(uusi.kuntajako, "Vakiluku", type = "sequential",
       				 main = "Uusien kuntien vakiluku", 
		palette = colorRampPalette(c("white", "blue"), space = "rgb"), 
		colorkey = FALSE, ncol = 100, plot=FALSE)

# -----------------------------------------------

# Tulosta kuvat PNG-tiedostoon
png("uusi.kuntajako.png"); print(pic); dev.off()
png("uusi.kuntajako2.png"); print(pic2); dev.off()
png("uusi.kuntajako3.png"); print(pic3); dev.off()

# -----------------------------------

# Kirjoita uuden kuntajaon mukaiset rajat ESRI-shapefileen
# HUOM: kirjoitus epäonnistuu, jos työhakemistossa on jo
# ennestaan saman niminen shp-file
rgdal::writeOGR(uusi.kuntajako, "uudet_kunnat.shp", "uusi.kuntajako",
driver="ESRI Shapefile")

