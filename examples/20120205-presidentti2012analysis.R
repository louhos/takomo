# Script for the analysis of Finnish Presidential election data 
# from Helsingin Sanomat vaalikone 2012 at
# http://www.vaalikone.fi/presidentti2012/

# This script is part of the Louhos-project (http://louhos.github.com/)

# Copyright (C) 2010-2013 Leo Lahti and Juuso Parkkinen.
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
# This script is tested with sorvi version 0.1.47
library(sorvi)

# Load required packages
# Remember to install required packages (e.g. 'install.packages("stats")')
library(stats)
library(reshape)



##########################################

# Use this script to retrieve the data 
# source("President2012GetData.R")

###########################################

# Puolue vs. Ykkosehdokkaan puolue 
puoluekanta.vs.ykkosehdokkaan.puolue <- data.frame(list(Puoluekanta = as.character(user.info$Puolue), 
				     		YkkosehdokkaanPuolue = unlist(candidate.info[as.character(user.info$Ykkosehdokas), "party"])))

# Ristiintaulukointi:
xtab <- stats::xtabs(~ Puoluekanta + YkkosehdokkaanPuolue, data = puoluekanta.vs.ykkosehdokkaan.puolue)

# Muunna prosenteiksi (kullekin puoluekannalle ykkösehdokkaiden kannatusprosentit)
xtab.relative <- t(xtab / rowSums(xtab)) # Ykkosehdokas vs puoluekanta
df <- as.data.frame(xtab.relative[1:8, 1:11])
df$YkkosehdokkaanPuolue <- rownames(df)

# Visualisoi
dfm <- reshape::melt(df, id = c("YkkosehdokkaanPuolue"), variable = "Puoluekanta")
p <- ggplot(dfm, aes(x = Puoluekanta, weight = value, fill = YkkosehdokkaanPuolue))
p <- p + geom_bar(position="stack") + coord_flip() + ylab("") + opts(title = "Ykkösehdokkaan puolue")
cbgFillPalette <- scale_fill_manual(values = c("black", "blue", "white", "brown", "yellow", "orange", "red", "green"))
p <- p + cbgFillPalette 

# Talleta kuva
png("taktikointi.png")
print(p)
dev.off()

###############################

# Korrelaatiot kullekin kysymys-taustamuuttujaparille
# -> Hae kysymykset, joilla on vahvin kytkös kuhunkin taustamuuttujaan

res <- NULL
qids <- colnames(user.answ.int)
vars <- c("Koulutustaso", "Ika", "Sukupuoli", "Tuloluokka")
for (var in vars) {
  inds <- !(user.info[[var]] %in% c("NULL")) & !is.na(user.info[[var]])
  cc <- abs(cor(as.numeric(user.info[inds, var]), user.answ.int[inds,], method = "spearman"))
  res <- rbind(res, cc)
}
rownames(res) <- vars

#######################################################

# Visualisoi kysymykset, joissa nähdään vahvimmat erot sukupuolten välillä

# Valitse pari ensimmaista kysymysta tarkasteluun
var <- "Sukupuoli"
qids <- colnames(res)[order(res[var,], decreasing = TRUE)][1:2]

# Visualisoi
df <- data.frame(Sukupuoli = user.info[["Sukupuoli"]], 
      		 user.answ.int[, qids] + 1)
dfm <- melt(subset(df, !Sukupuoli %in% "NULL"))
dfm$Sukupuoli <- droplevels(dfm$Sukupuoli)
my_breaks <- seq(min(dfm$value), max(dfm$value), 1)
my_labs <- seq(min(dfm$value), max(dfm$value), 1)
p <- ggplot(data = dfm, aes(x = value, fill = Sukupuoli)) + geom_bar() + facet_grid(variable ~ .)
p <- p + scale_x_continuous(breaks=my_breaks, labels=my_labs)
p <- p + opts(axis.text.x = theme_text(size = 12)) + xlab("Vastauvaihtoehdot") 
p <- p + opts(axis.text.y = theme_text(size = 10)) + ylab("Vastaajien lukumäärä")
	   
# Talleta kuva
png("sukupuolikysymykset.png")
print(p)
dev.off()

####################################

# Vahvimin ikasidonnaiset kysymykset
var <- "Ika"
qids <- colnames(res)[order(res[var,], decreasing = TRUE)][1:3]

# Muokkaa taulukko sopivaan muotoon
df <- data.frame(Ikaluokka = user.info[["Ika"]], 
      		 user.answ.int[, qids] + 1)
dfm <- melt(subset(df, !Ikaluokka %in% "NULL"))
dfm$Ikaluokka <- droplevels(dfm$Ikaluokka)
dfm$value <- factor(dfm$value)

# Visualisoi
p <- ggplot(data = dfm, aes(x = value, fill = Ikaluokka)) 
p <- p + geom_bar(aes(group = Ikaluokka, y = ..density..), position = "dodge") + facet_grid(variable ~ .)
p <- p + opts(axis.text.x = theme_text(size = 15)) + xlab("Vastausvaihtoehdot") 
p <- p + opts(axis.text.y = theme_text(size = 15)) + ylab("Frekvenssi")
library(RBGL)
my.palette <- colorRampPalette(c("red", "darkblue"), space = "rgb")
p <- p + scale_fill_manual(values = my.palette(length(levels(dfm$Ikaluokka)))) 

# Talleta kuva
png("ikakysymykset.png")
print(p)
dev.off()

####################################

# Tulotason mukana eniten mielipiteitä jakava kysymys

var <- "Tuloluokka"
qid <- colnames(res)[order(res[var,], decreasing = TRUE)][[1]]
# PlotDistribution(var, qid, user.info, user.answ.int)

# Muokkaa datataulu
df <- data.frame(Tuloluokka = user.info[["Tuloluokka"]], 
      		 Kysymys = user.answ.int[, qid] + 1)
dfm <- melt(subset(df, !Tuloluokka %in% "NULL"))
dfm$Tuloluokka <- droplevels(dfm$Tuloluokka)
dfm$value <- factor(dfm$value)
dfm <- subset(dfm, !is.na(Tuloluokka))

# Visualisoi
p <- ggplot(data = dfm, aes(x = value, fill = Tuloluokka)) 
p <- p + geom_bar(aes(group = Tuloluokka, y = ..density..), position = "dodge") 
p <- p + opts(title = qid)	    
p <- p + opts(axis.text.x = theme_text(size = 15)) + xlab("Vastauvaihtoehdot") 
p <- p + opts(axis.text.y = theme_text(size = 15)) + ylab("Frekvenssi")
p <- p + scale_fill_manual(values = my.palette(length(levels(dfm$Tuloluokka)))) 

# Talleta
png("tulotasokysymys.png")
print(p)
dev.off()

###############################

# Tutkaile kysymysta, joka jakaa toiseksi vahvimmin
# mielipiteet koulutustason mukaan

var <- "Koulutustaso"
qid <- colnames(res)[order(res[var,], decreasing = TRUE)][[2]]

# Datataulu
df <- data.frame(Koulutustaso = user.info[[var]], 
      		 Kysymys = user.answ.int[, qid] + 1)

tab <- sapply(unique(df[[var]]), function(lev) {tab <- table(df[df[[var]] == lev, "Kysymys"]); tab/sum(tab)})
colnames(tab) <- unique(df[[var]]);
tab <- tab[sort(rownames(tab)), !(colnames(tab) == "NULL")]
m <- melt(tab) 
names(m) <- c("Vaihtoehto", "Koulutustaso", "value")
m$Koulutustaso <- factor(m$Koulutustaso, levels = c("Peruskoulu, kansakoulu tai vastaava",
"Ammattikoulu, kauppakoulu tai vastaava", "Opistotason koulutus",
"Ylioppilas", "Ammattikorkeakoulu", "Yliopisto tai korkeakoulu"))
m$Vaihtoehto <- factor(m$Vaihtoehto)

# Visu
p <- ggplot(m, aes(x = Koulutustaso, y = value, group = Vaihtoehto, color = Vaihtoehto)) 
p <- p + geom_line() + ylab("Frekvenssi") + xlab("Koulutus")
p <- p + opts(axis.text.x = theme_text(angle = 25)) + opts(title = qid)

# Talletus
png("koulutuskysymys.png")
print(p)
dev.off()

#########################################################

# Talla voi tarkastella kysymyksiä ja vastausvaihtoehtoja
#Presidentti2012RetrieveAnswerText(qid, questions)$question; Presidentti2012RetrieveAnswerText(qid, questions)$text


