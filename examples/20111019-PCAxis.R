# (C) 2011-2012 Leo Lahti <leo.lahti@iki.fi> All rights reserved.
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses

# Tama esimerkki on testattu sorvi-paketin versiolla 0.1.42
# Asennusohjeet: http://sorvi.r-forge.r-project.org/asennus.html

# Esimerkki Suomen kuntatason vaestonkasvutilastojen (Tilastokeskus)
# visualisoinnista Maanmittauslaitoksen karttadatalla (vuonna 2010)

# Lataa soRvi
# http://louhos.github.com/sorvi
library(sorvi)

###############################################

# Lue Suomen kuntarajat SpatialPolygon-muodossa
# (C) Maanmittauslaitos 2011
# http://www.maanmittauslaitos.fi/aineistot-palvelut/digitaaliset-tuotteet/ilmaiset-aineistot/hankinta
sp <- LoadMML(data.id = "kunta1_p", resolution = "1_milj_Shape_etrs_shape") 

#################################################

# Lue kuntatason vaestonkasvutiedot tilastokeskuksen StatFin-tietokannasta
# http://www.stat.fi/tup/statfin/index.html
# PC Axis-muodossa ja muunna data.frameksi
#px <- GetPXTilastokeskus("http://pxweb2.stat.fi/database/StatFin/vrm/synt/080_synt_tau_203_fi.px")
px <- GetPXTilastokeskus("http://pxweb2.stat.fi/database/StatFin/vrm/muutl/080_muutl_tau_203.px")

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
q <- PlotShape(sp, varname, type = "twoway",
main = "Väestönkasvu 2010",
at = seq(0 - int, 0 + int, length = 11))

#png("vaestonkasvu.png")
jpeg("vaestonkasvu.jpg")
print(q)
dev.off()

