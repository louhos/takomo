# This script is part of the Louhos-project (http://louhos.github.com/)

# Copyright (C) 2010-2013 Leo Lahti, Juuso Parkkinen and Joona Lehtomäki.
# Contact: <http://louhos.github.com/contact>. 
# All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# Tama tiedosto hakee Helsingin seudun ymparistopalvelujen (HSY)
# avoimia aineistoja ja visualisoi niita paakaupunkiseudun kartalla.
# HSY data Copyright: (C) HSY 2011. Lisenssitiedot ja aineistokuvaukset, ks.:
# http://www.hsy.fi/seututieto/kaupunki/paikkatiedot/Sivut/Avoindata.aspx
# http://www.hsy.fi/seututieto/Documents/Paikkatiedot/Tietokuvaukset_kaikki.pdf

# Install and load sorvi package
# Instructions in http://louhos.github.com/sorvi/asennus.html
# This script is tested with sorvi version 0.2.27
library(sorvi)

# Load required packages
# Remember to install required packages (e.g. 'install.packages("reshape")')
library(reshape)


# Hae HSY:n Vaestoruudukko-data shape-muodossa
sp <- sorvi::GetHSY("Vaestoruudukko")

## Tutki sp-objektin sisaltoa
print(head(as.data.frame(sp)))

# Visualisoi Helsingin asukasjakauma
at <-  c(seq(0, 2000, 250), Inf) # color palette breakpoints
q <- sorvi::PlotShape(sp, "ASUKKAITA", type = "oneway", at = at, 
               ncol = length(at), main = "Helsingin asukasjakauma")

# Tallenna kuva PNG-muodossa
png("HSY.vaesto.png")
print(q)
dev.off()

#####################################################

# Visualize the building year of the oldest building in
# different regions in Helsinki

# Download and preprocess HSY SeutuRAMAVA data
# (C) HSY 2011; for data description see:
# http://www.hsy.fi/seututieto/Documents/Paikkatiedot/Tietokuvaukset_kaikki.pdf
sp <- sorvi::GetHSY("SeutuRAMAVA")
sp$VANHINRAKE <- as.integer(sp$VANHINRAKE)
sp$VANHINRAKE[sp$VANHINRAKE == 999999999] <- NA
at <- seq(1800, 2020, 10)
palette <- colorRampPalette(c("blue", "gray", "red"), space = "rgb")
q <- sorvi::PlotShape(sp, "VANHINRAKE", type = "twoway",
at = at, ncol = length(at),
palette = palette,
main = "Vanhimman rakennuksen rakennusvuosi")

png("HSY.vanhinrakennus.png")
print(q)
dev.off()

###############################################

# Visualize the distribution of the building years for the oldest building
# across Helsinki regions

par(mar=c(4, 4, 4, 2), las = 1)
df <- dfsort(as.data.frame(sp), VANHINRAKE)
df <- df[apply(df, 1, function (x) {!any(is.na(x))}), c("NIMI", "VANHINRAKE")]
v <- df$VANHINRAKE
names(v) <- as.character(df$NIMI)
v <- rev(v)

#pdf("HSY.ikajakauma.pdf")
#plot(v, 1:length(v), type = "n", xlim = c(min(v) - 10, max(v) + 10),
#    ylab = "Kaupunginosat", xlab = "Rakennusvuosi",
#    main = "Vanhimman rakennuksen rakennusvuosi", yaxt = "n")
#text(v, 1:length(v), labels = names(v), cex = 0.4)
#dev.off()

###############################################

# Area which has been built or is currently being built

df <- as.data.frame(sp)
df <- df[, c("RAKERA_AS", "RAKERA_MUU", "KARA_AS", "KARA_MUU")]
keep <- apply(df, 1, function (x) {!all(x == 0)}) & !is.na(as.data.frame(sp)$NIMI)
df <- df[keep,]
rownames(df) <- as.character(as.data.frame(sp)[keep, "NIMI"])

df <- reshape::rename(df, c(RAKERA_AS = "Rakenteilla (asuminen)", RAKERA_MUU = "Rakenteilla (muu)", KARA_AS = "Rakennettu (asuminen)", KARA_MUU = "Rakennettu (muu)"))
#df <- df/as.data.frame(sp)$YKSLKM
df <- df[rev(order(rowSums(df), decreasing = TRUE)[1:50]),]
df <- df[, c(4,3,2,1)]
# display.brewer.all()
FD.palette <- rev(c("orange", "darkgray", "blue", "black"))
options(scipen=2)


png("HSY.kerrosala.png")
par(mar=c(6, 8, 3, 2), las = 1)
barplot(t(df), beside=F,col=FD.palette, border=FD.palette, space=1, legend=F,
ylab = "", xlab="Neliometria",
main="Rakenteilla oleva kerrosala",
mgp=c(4.5,1,0), horiz = TRUE,
cex.names = 0.7, xlim = c(0, 1.02*max(rowSums(df))))
legend("bottomright", legend=rev(rownames(t(df))), fill=rev(FD.palette))
box()
dev.off()

###############################################

# Rakenteilla oleva kerrosala

df <- as.data.frame(sp)
keep <- !df$RAKERA_AS == 0 & !df$RAKERA_MUU == 0
df <- df[keep, c("RAKERA_AS", "RAKERA_MUU")]
rownames(df) <- as.data.frame(sp)[keep, "NIMI"]
df <- df[rev(order(rowSums(df), decreasing = TRUE)[1:50]),]
df <- reshape::rename(df, c(RAKERA_AS = "Asuminen", RAKERA_MUU = "Muu"))
df <- df[, c("Muu", "Asuminen")]
# display.brewer.all()
FD.palette <- c("darkgray", "orange")
options(scipen=2)

png("HSY.kerrosala2.png")
par(mar=c(4, 8, 3, 2), las = 1)
barplot(t(df), beside=F,col=FD.palette, border=FD.palette, space=1, legend=F,
ylab = "", xlab="Neliometria",
main="Rakenteilla oleva kerrosala",
mgp=c(4.5,1,0), horiz = TRUE,
cex.names = 0.7, xlim = c(0, 1.02*max(rowSums(df))))
legend("bottomright", legend=rev(rownames(t(df))), fill=rev(FD.palette))
box()
dev.off()

