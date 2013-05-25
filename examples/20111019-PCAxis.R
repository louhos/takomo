# This script is part of the Louhos-project (http://louhos.github.com/)

# Copyright (C) 2010-2013 Leo Lahti.
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


###############################################

# Lue Suomen kuntarajat SpatialPolygon-muodossa
# (C) Maanmittauslaitos 2011
# http://www.maanmittauslaitos.fi/aineistot-palvelut/digitaaliset-tuotteet/ilmaiset-aineistot/hankinta
sp <- sorvi::LoadMML(data.id = "kunta1_p", resolution = "1_milj_Shape_etrs_shape") 

#################################################

# Lue kuntatason vaestonkasvutiedot tilastokeskuksen StatFin-tietokannasta
# http://www.stat.fi/tup/statfin/index.html
# PC Axis-muodossa ja muunna data.frameksi
#px <- GetPXTilastokeskus("http://pxweb2.stat.fi/database/StatFin/vrm/synt/080_synt_tau_203_fi.px")
px <- sorvi::GetPXTilastokeskus("http://pxweb2.stat.fi/database/StatFin/vrm/muutl/080_muutl_tau_203.px")

# Poimi taulukosta halutut tiedot
vaestonkasvu <- subset(px, Väestönmuutos.ja.väkiluku == "Luonnollinen väestönlisäys" & Vuosi == 2010)

################################################

# Lisaa tiedot karttaobjektiin
sp@data$vaestonkasvu <- vaestonkasvu$dat[match(sp$Kunta.FI, vaestonkasvu$Alue)]
# Korvaa puuttuvat arvot nollalla
sp[["vaestonkasvu"]][is.na(sp[["vaestonkasvu"]])] <- 0

################################################

# Piirra kuva
varname <- "vaestonkasvu"
int <- max(abs(sp[[varname]]))
q <- sorvi::PlotShape(sp, varname, type = "twoway",
main = "Väestönkasvu 2010",
at = seq(0 - int, 0 + int, length = 11))

# png("vaestonkasvu.png")
## jpeg("vaestonkasvu.jpg")
print(q)
# dev.off()

