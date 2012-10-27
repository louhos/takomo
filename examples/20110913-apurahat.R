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

# Install soRvi package
# Instructions in http://sorvi.r-forge.r-project.org/asennus.html
# NOTE! This script has been udpated 26.12.2011 to use sorvi version 0.1.40!
library(sorvi)

# Load apurahat data
apurahat <- GetApurahat()

# Load maakuntakartta (need to permit the use of gpclib)
if (!gpclibPermitStatus())
  gpclibPermit()
#maakuntakartta <- GetMaakuntainfo()

# Load maakuntakartta-data
library(sp)
con <- url("http://gadm.org/data/rda/FIN_adm2.RData", encoding="UTF-8")
load(con)
close(con)

# Fix province names
gadm@data$Maakunnat <- factor(gsub("\xe4", "?", gadm@data$NAME_2))
levels(gadm@data$Maakunnat) <- c("KESKI-SUOMI", "KESKI-POHJANMAA", "ITÄ-UUSIMAA", "VARSINAIS-SUOMI", "KAINUU", "KYMENLAAKSO", "LAPPI", "POHJOIS-KARJALA","POHJOIS-SAVO", "POHJOIS-POHJANMAA", "POHJANMAA", "PIRKANMAA", "PÄIJÄT-HÄME", "SATAKUNTA", "ETELÄ-KARJALA", "ETELÄ-POHJANMAA", "ETELÄ-SAVO", "KANTA-HÄME", "UUSIMAA")

# Change to ggplot2-format with fortify
library(ggplot2)
if (!gpclibPermitStatus())
  gpclibPermit()
maakuntakartta <- fortify(gadm, region="Maakunnat")

# Remove ITA-UUSIMAA
maakuntakartta$id[maakuntakartta$id=="ITÄ-UUSIMAA"] <- "UUSIMAA"

# Add data of province population sizes
# Data obtained manually from Tilastokeskus
temp <- data.frame(maakunta=sort(unique(maakuntakartta$id)))


# Get population for each province
pop <- GetProvinceInfo()
temp$asukasluku <- pop[match(temp$maakunta, toupper(pop$Maakunta)), "Vakiluku"]
maakuntakartta$asukasluku <- temp$asukasluku[match(maakuntakartta$id, temp$maakunta)]


##########################
## Basic visualizations ##
##########################

library(ggplot2)
library(gridExtra)

# Plot top20 artists (HS 19.5.2011)
henkilo.summat <- aggregate(apurahat$Myontosumma.EUR, by=list(apurahat$Hakijan.nimi), sum)
top20.nimet <- henkilo.summat[order(henkilo.summat$x, decreasing=T)[1:20],1]
top20 <- subset(henkilo.summat, henkilo.summat$Group.1 %in% top20.nimet)
names(top20) <- c("Hakijan.nimi", "Myontosumma.EUR")
top20$Hakijan.nimi <- reorder(factor(top20$Hakijan.nimi), top20$Myontosumma.EUR, sum)
p1 <- ggplot(top20, aes(Hakijan.nimi, Myontosumma.EUR)) + geom_bar(fill="red")
p1 <- p1 + coord_flip() + opts(axis.text.x=theme_text(angle=-90, hjust=0),title="Top 20 apurahan saaneet")
#ggsave("top20.png", plot=p1)
png("top20.png")
print(p1)
dev.off()

# Plot by province and year
maakunta.summat <- aggregate(apurahat$Myontosumma.EUR, list(apurahat$Maakunta, apurahat$Vuosi), sum)
names(maakunta.summat) <- c("Maakunta", "Vuosi", "Myontosumma.EUR")
p2 <- ggplot(maakunta.summat, aes(x=Maakunta, y=Myontosumma.EUR, fill=factor(Vuosi))) + geom_bar(position="dodge")
p2 <- p2 + coord_flip() + opts(axis.text.x=theme_text(angle=-90, hjust=0),title="Apurahat maakunnittain summattuna")
p2 <- p2 + scale_y_continuous(formatter="comma")

# Plot by class and year
hakemusluokka.summat <- aggregate(apurahat$Myontosumma.EUR, list(apurahat$Hakemusluokka, apurahat$Vuosi), sum)
names(hakemusluokka.summat) <- c("Hakemusluokka", "Vuosi", "Myontosumma.EUR")
p3 <- ggplot(hakemusluokka.summat, aes(x=Hakemusluokka, y=Myontosumma.EUR, fill=factor(Vuosi))) + geom_bar(position="dodge")
p3 <- p3 + coord_flip() + opts(axis.text.x=theme_text(angle=-90, hjust=0),title="Apurahat hakemusluokittain summattuna")
p3 <- p3 + scale_y_continuous(formatter="comma")
#ggsave("hakemusluokat.png", plot=p3)
png("hakemusluokat.png")
print(p3)
dev.off()

# Plot by age and gender
ikaryhma.summat <- aggregate(apurahat$Myontosumma.EUR, list(apurahat$Ikaryhma, apurahat$Sukupuoli), sum)
names(ikaryhma.summat) <- c("Ikaryhma", "Sukupuoli", "Myontosumma.EUR")
p4 <- ggplot(ikaryhma.summat, aes(x=Ikaryhma, y=Myontosumma.EUR, fill=Sukupuoli)) + geom_bar(position="dodge")
p4 <- p4 + coord_flip() + opts(axis.text.x=theme_text(angle=-90, hjust=0),title="Apurahat ikaryhmittain")
p4 <- p4 + scale_y_continuous(formatter="comma")
#ggsave("ika_sukupuoli.png", plot=p4)
png("ika_sukupuoli.png")
print(p4)
dev.off()

# Plot age group vs. class
ika.vs.hakemus <- aggregate(apurahat$Myontosumma.EUR, list(apurahat$Hakemusluokka, apurahat$Ikaryhma), sum)
names(ika.vs.hakemus) <- c("Hakemusluokka", "Ikaryhma", "Myontosumma.EUR")
p5 <- ggplot(ika.vs.hakemus, aes(Hakemusluokka, Ikaryhma)) + geom_point(aes(size=Myontosumma.EUR))# + geom_jitter()
p5 <- p5 + coord_flip() + opts(axis.text.x=theme_text(angle=-90), title="Apurahat, ikaryhma vs. hakemusluokka")
p5 <- p5 + scale_area()
#ggsave("ikaryhma_vs_hakemusluokka.png", plot=p5)
png("ikaryhma_vs_hakemusluokka.png")
print(p5)
dev.off()


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
p1 <- p1 + opts(title="Apurahat per asukas maakunnittain")
p1 <- p1 + coord_map(project="gilbert") + xlab(NULL) + ylab(NULL) + scale_colour_discrete(name = "EUR per asukas")

# Another plot without Uusimaa
p2 <- ggplot(subset(maakuntakartta, id != "UUSIMAA"), aes(x = long, y = lat))
p2 <- p2 + geom_polygon(aes(group=group, fill=EUR.per.asukas), colour="white")
p2 <- p2 + opts(title="Apurahat per asukas maakunnittain (ei Uusimaa)")
p2 <- p2 + coord_map(project="gilbert") + xlab(NULL) + ylab(NULL) + scale_colour_discrete(name = "EUR per asukas")

# Plot both maps side by side
#ggsave("Apurahat_kartalla_maakunnittain.png", plot=arrangeGrob(p1, p2, ncol=2), width=12, height=8)
pc <- arrangeGrob(p1, p2, ncol = 2)
png("Apurahat_kartalla_maakunnittain.png")
print(pc)
dev.off()


