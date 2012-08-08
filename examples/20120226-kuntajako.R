# Suomen kuntien yhdistäminen ja visualisointi uuden kuntajaon mukaisesti

# Copyright (C) 2012 Leo Lahti ja Joona Lehtomäki
# Contact: sorvi-commits@lists.r-forge.r-project.org
# All rights reserved.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License:
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This script was implemented with soRvi version 0.1.55

library(sorvi)
library(rgeos)
library(rgdal)
if (!gpclibPermit()) { gpclibPermit() }

# ----------------------------------------------

# Lue nykyiset kuntarajat Maanmittauslaitoksen aineistosta
# (C) MML 2011
LoadData("MML")
fi.kunnat <- MML[["1_milj_Shape_etrs_shape"]][["kunta1_p"]]

# Länsi-Turunmaan nimi vaihtui Paraisiksi vuoden 2012 alusta,
# lisää muutos dataan
kunnat <- as.character(fi.kunnat$Kunta.FI)
kunnat[[which(kunnat == "Länsi-Turunmaa")]] <- "Parainen"
fi.kunnat$Kunta.FI <- factor(kunnat)

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
reg <- unionSpatialPolygons(fi.kunnat, uusi.kunta, avoidGEOS = T)

# Nimea uudet kunnat taulukkoon
attr <- data.frame(Uusi.kuntajako = names(reg))

# ----------------------------------------------

# Yhdista uudet alueet ja nimet / Merge into a SpatialPolygonsDataFrame
uusi.kuntajako <- SpatialPolygonsDataFrame(reg, attr, match.ID = F)

# ----------------------------------------------

# Laske vakiluku uusille kunnille:
# Hae kuntatason tilastoja:
municipality.info <- GetMunicipalityInfo()
vakiluku <- municipality.info[["Väkiluku 31.12.2010"]]
names(vakiluku) <- rownames(municipality.info)
uusi.vakiluku <- sapply(split(vakiluku[as.character(map$Vanha)], map$Uusi), sum)
uusi.kuntajako$Vakiluku <- uusi.vakiluku[as.character(uusi.kuntajako$Uusi.kuntajako)]

# ----------------------------------------------

# Visualisoi uusi kuntajako
pic <- PlotShape(uusi.kuntajako, "Uusi.kuntajako", type = "discrete",
main = "Uusi kuntajako", ncol = 12)

# Vaihtoehtoinen visualisointi. Esitä uudet kunnat väreillä ja
# näytä nykyiset kuntarajat viivoilla.
fi.kunnat$uusi.kunta <- uusi.kunta
pic2 <- PlotShape(fi.kunnat, "uusi.kunta", type = "discrete",
main = "Uusi kuntajako", ncol = 12)

# Visualisoi uusien kuntien vakiluku
pic3 <- PlotShape(uusi.kuntajako, "Vakiluku", type = "sequential",
       				 main = "Uusien kuntien vakiluku", 
		palette = colorRampPalette(c("white", "blue"), space = "rgb"), 
		colorkey = FALSE, ncol = 100)

# -----------------------------------------------

# Tulosta kuvat PNG-tiedostoon
png("uusi.kuntajako.png"); print(pic); dev.off()
png("uusi.kuntajako2.png"); print(pic2); dev.off()
png("uusi.kuntajako3.png"); print(pic3); dev.off()

# -----------------------------------

# Kirjoita uuden kuntajaon mukaiset rajat ESRI-shapefileen
# HUOM: kirjoitus epäonnistuu, jos työhakemistossa on jo
# ennestaan saman niminen shp-file
writeOGR(uusi.kuntajako, "uudet_kunnat.shp", "uusi.kuntajako",
driver="ESRI Shapefile")

