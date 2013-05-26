# This script is part of the Louhos-project (http://louhos.github.com/)

# Copyright (C) 2010-2013 Juuso Parkkinen.
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
# Remember to install required packages (e.g. 'install.packages("sp")')
library(sp)
library(ggplot2)
library(gridExtra)

# install.packages("mapproj")
library(mapproj)
library(rgeos)


# Load apurahat data
apurahat <- sorvi::GetApurahat()

# Load maakuntakartta-data
con <- url("http://gadm.org/data/rda/FIN_adm2.RData", encoding="UTF-8")
load(con)
close(con)

# Fix province names
gadm@data$Maakunnat <- factor(gsub("\xe4", "?", gadm@data$NAME_2))
levels(gadm@data$Maakunnat) <- c("KESKI-SUOMI", "KESKI-POHJANMAA", "ITÄ-UUSIMAA", "VARSINAIS-SUOMI", "KAINUU", "KYMENLAAKSO", "LAPPI", "POHJOIS-KARJALA","POHJOIS-SAVO", "POHJOIS-POHJANMAA", "POHJANMAA", "PIRKANMAA", "PÄIJÄT-HÄME", "SATAKUNTA", "ETELÄ-KARJALA", "ETELÄ-POHJANMAA", "ETELÄ-SAVO", "KANTA-HÄME", "UUSIMAA")

# Transform to ggplot2-format with fortify
if (!gpclibPermitStatus())
  gpclibPermit()
maakuntakartta <- ggplot2::fortify(gadm, region="Maakunnat")

# Remove ITA-UUSIMAA
maakuntakartta$id[maakuntakartta$id=="ITÄ-UUSIMAA"] <- "UUSIMAA"

# Add data of province population sizes
# Data obtained manually from Tilastokeskus
temp <- data.frame(maakunta=sort(unique(maakuntakartta$id)))

# Get population for each province
pop <- sorvi::GetProvinceInfo()
temp$asukasluku <- pop[match(temp$maakunta, toupper(pop$Maakunta)), "Vakiluku"]
maakuntakartta$asukasluku <- temp$asukasluku[match(maakuntakartta$id, temp$maakunta)]


##########################
## Basic visualizations ##
##########################


# Plot top20 artists (HS 19.5.2011)
henkilo.summat <- aggregate(apurahat$Myontosumma.EUR, by=list(apurahat$Hakijan.nimi), sum)
top20.nimet <- henkilo.summat[order(henkilo.summat$x, decreasing=T)[1:20],1]
top20 <- subset(henkilo.summat, henkilo.summat$Group.1 %in% top20.nimet)
names(top20) <- c("Hakijan.nimi", "Myontosumma.EUR")
top20$Hakijan.nimi <- reorder(factor(top20$Hakijan.nimi), top20$Myontosumma.EUR, sum)
p1 <- ggplot(top20, aes(x=Hakijan.nimi, y=Myontosumma.EUR)) + geom_bar(fill="red", stat="identity")
p1 <- p1 + coord_flip() + theme(axis.text.x=element_text(angle=-90, hjust=0)) + ggtitle("Top 20 apurahan saaneet")
ggsave("top20.png", plot=p1)

# Plot by province and year
maakunta.summat <- aggregate(apurahat$Myontosumma.EUR, list(apurahat$Maakunta, apurahat$Vuosi), sum)
names(maakunta.summat) <- c("Maakunta", "Vuosi", "Myontosumma.EUR")
p2 <- ggplot(maakunta.summat, aes(x=Maakunta, y=Myontosumma.EUR, fill=factor(Vuosi))) + geom_bar(position="dodge", stat="identity")
p2 <- p2 + coord_flip() + theme(axis.text.x=element_text(angle=-90, hjust=0)) + ggtitle("Apurahat maakunnittain summattuna")

# Plot by class and year
hakemusluokka.summat <- aggregate(apurahat$Myontosumma.EUR, list(apurahat$Hakemusluokka, apurahat$Vuosi), sum)
names(hakemusluokka.summat) <- c("Hakemusluokka", "Vuosi", "Myontosumma.EUR")
p3 <- ggplot(hakemusluokka.summat, aes(x=Hakemusluokka, y=Myontosumma.EUR, fill=factor(Vuosi))) + geom_bar(position="dodge", stat="identity")
p3 <- p3 + coord_flip() + theme(axis.text.x=element_text(angle=-90, hjust=0)) + ggtitle("Apurahat hakemusluokittain summattuna")
ggsave("hakemusluokat.png", plot=p3)

# Plot by age and gender
ikaryhma.summat <- aggregate(apurahat$Myontosumma.EUR, list(apurahat$Ikaryhma, apurahat$Sukupuoli), sum)
names(ikaryhma.summat) <- c("Ikaryhma", "Sukupuoli", "Myontosumma.EUR")
p4 <- ggplot(ikaryhma.summat, aes(x=Ikaryhma, y=Myontosumma.EUR, fill=Sukupuoli)) + geom_bar(position="dodge", stat="identity")
p4 <- p4 + coord_flip() + theme(axis.text.x=element_text(angle=-90, hjust=0)) + ggtitle("Apurahat ikaryhmittain")
ggsave("ika_sukupuoli.png", plot=p4)

# Plot age group vs. class
ika.vs.hakemus <- aggregate(apurahat$Myontosumma.EUR, list(apurahat$Hakemusluokka, apurahat$Ikaryhma), sum)
names(ika.vs.hakemus) <- c("Hakemusluokka", "Ikaryhma", "Myontosumma.EUR")
p5 <- ggplot(ika.vs.hakemus, aes(Hakemusluokka, Ikaryhma)) + geom_point(aes(size=Myontosumma.EUR))# + geom_jitter()
p5 <- p5 + coord_flip() + theme(axis.text.x=element_text(angle=-90)) + ggtitle("Apurahat, ikaryhma vs. hakemusluokka")
p5 <- p5 + scale_size_area()
ggsave("ikaryhma_vs_hakemusluokka.png", plot=p5)


##############################
## Plot on a map of Finland ##
##############################

# Sum by province, remove "ITa-UUSIMAA"
apurahat$Maakunta[apurahat$Maakunta=="ITa-UUSIMAA"] <- "UUSIMAA"
apurahat$Maakunta <- droplevels(apurahat$Maakunta)
maakunta.summat <- aggregate(apurahat$Myontosumma.EUR, by=list(apurahat$Maakunta), sum)
maakuntakartta$summat <- maakunta.summat[match(toupper(maakuntakartta$id), maakunta.summat[,1]),2]

# Add size of population for each province (2010, obtained originally from Tilastokeskus), compute scholarschip per citizen
maakuntakartta$EUR.per.asukas <- maakuntakartta$summat / maakuntakartta$asukasluku

# Plot apurahat on a map by province
p1 <- ggplot(maakuntakartta, aes(x = long, y = lat))
p1 <- p1 + geom_polygon(aes(group=group, fill=EUR.per.asukas), colour="white")
p1 <- p1 + ggtitle("Apurahat per asukas maakunnittain")
p1 <- p1 + coord_map(project="gilbert") + xlab(NULL) + ylab(NULL) + scale_colour_discrete(name = "EUR per asukas")

# Another plot without Uusimaa
p2 <- ggplot(subset(maakuntakartta, id != "UUSIMAA"), aes(x = long, y = lat))
p2 <- p2 + geom_polygon(aes(group=group, fill=EUR.per.asukas), colour="white")
p2 <- p2 + ggtitle("Apurahat per asukas maakunnittain (ei Uusimaa)")
p2 <- p2 + coord_map(project="gilbert") + xlab(NULL) + ylab(NULL) + scale_colour_discrete(name = "EUR per asukas")

# Plot both maps side by side
ggsave("Apurahat_kartalla_maakunnittain.png", plot=arrangeGrob(p1, p2, ncol=2), width=12, height=8)

