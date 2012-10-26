# Copyright (C) 2012 Louhos
# All rights reserved. Contact: <http://louhos.github.com/contact.html>

# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# -------------------------------------------

# Tama esimerkki visualisoi kunnallisvaaliehdokkaiden aktiivisuutta
# sosiaalisessa mediassa 2012. Tietojen lahteina ovat
# Oikeusministeriö
# Datavaalit-sivusto
# Tilastokeskus
# Maanmittauslaitos

# ---------------------------------------

# Hae data puolueiden kunnallisvaaliehdokkaiden aktiivisuudesta
# sosiaalisessa mediassa Datavaalit-palvelimelta
some <- read.csv("http://www.datavaalit.fi/storage/some-updates-stats-2012-10-26.csv", sep = ",")
names(some) <- c("Puolue", "Media", "dat")
some$Media <- factor(gsub("FB", "Facebook", gsub("TW", "Twitter", some$Media)))
some.full <- some

# Hae 2012 kunnallisvaalien ehdokastiedot
cand <- read.csv2("http://www.datavaalit.fi/storage/avoindata/datavaalit-ehdokas-ja-tulostiedot/2012/municipal_elections_candidates_2012_finland.csv", sep = ";", fileEncoding = "iso-8859-1")
cand.per.party <- sort(table(cand$Puolue_lyhenne_fi))

# Load party information (C) Oikeusministeriö 2012
library(sorvi)
parties.all <- ReadAllParties()

# ------------------------------------

# Esikasittele data 
some <- some.full
some <- subset(some, Media == "Facebook")
some$dat.normalized <- some$dat/cand.per.party[as.character(some$Puolue)]
some$candidates <- cand.per.party[as.character(some$Puolue)]
some <- some[rev(order(some$dat.normalized)),]
some <- subset(some, candidates > 1)
some$Puolue.lyh <- some$Puolue
some$Puolue <- parties.all$Nimi_fi[match(as.character(some$Puolue), parties.all$Puolue_lyhenne_fi)]
some$Puolue <- factor(some$Puolue, levels = rev(as.character(some$Puolue)))


# Barplot
library(ggplot2)
theme_set(theme_bw(8))
some$Paivitykset.edustajaa.kohden <- some$dat.normalized
p <- ggplot(some, aes(x = Puolue, y = Paivitykset.edustajaa.kohden, group = Media)) + geom_bar(stat = "identity") + coord_flip() + ggtitle("Ehdokkaiden aktiivisuus sosiaalisessa mediassa")

# ----------------------------------------

# Vertaile Facebook- ja Twitter-aktiivisuutta

some <- some.full
some$dat.normalized <- some$dat/cand.per.party[as.character(some$Puolue)]
some$candidates <- cand.per.party[as.character(some$Puolue)]
some <- some[rev(order(some$dat.normalized)),]
some <- subset(some, candidates > 1)
some$Puolue.lyh <- some$Puolue
some$Puolue <- parties.all$Nimi_fi[match(as.character(some$Puolue), parties.all$Puolue_lyhenne_fi)]
some$Puolue <- factor(some$Puolue, levels = rev(as.character(some$Puolue)))

puolueet <- as.character(some$Puolue)
fb <- subset(some, Media == "Facebook")
rownames(fb) <- as.character(fb$Puolue)
tw <- subset(some, Media == "Twitter")
rownames(tw) <- as.character(tw$Puolue)
df <- data.frame(list(Puolue = puolueet, Facebook = fb[puolueet, "dat.normalized"], Twitter = tw[puolueet, "dat.normalized"]))
df <- df[apply(df, 1, function (x) {!any(is.na(x))}),]
p <- ggplot(df, aes(x = Facebook, y = Twitter, label = Puolue)) + geom_text(size = 3) + scale_x_continuous(lim = c(-1,max(df[, c("Facebook")]) + 2)) + scale_y_continuous(lim = c(-1,max(df[, c("Twitter")]))) + ggtitle("Facebook- ja Twitter-aktiivisuuden vertailua")

