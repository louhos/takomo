# Esimerkkikoodi, joka hakee rasterimuotoisia kartta-aineistoja Suomen
# ympäristökeskuksen (SYKE) OIVA-palvelusta (WMS)
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
# Copyright 2011 Joona Lehtomäki, joona.lehtomaki@gmail.com.

library(sorvi)
library(raster)
library(rgdal)

# MML:n kuntadata ladataan soRvista
LoadData("MML")
sp <- MML[["1_milj_Shape_etrs_shape"]][["kunta1_p"]]

# Listaa kaikki soRvi:ssa olevat WMS urlit
ListWMSurls()

# Jokaiselle WMS URLille (ts. palvelulle) on luotava oma WMS-olio.
# Corine 2006 maankäyttöluokat
corine.wms <- PreprocessWMS(url=LoadWMSurl(provider="OIVA", service="Corine"))
# Natura2000-suojelualueet
suojelu.wms <- PreprocessWMS(url=LoadWMSurl(provider="OIVA", service="Suojelu"))
# Pohjavesialueet
pohjavesi.wms <- PreprocessWMS(url=LoadWMSurl(provider="OIVA", service="Pohjavesi"))

# Ortoilmakuvat
ortoilma.wms <- PreprocessWMS(url=LoadWMSurl(provider="OIVA", service="Image"))

# Erotellaan kunta-aineistosta Lahden polygoni
sp.lahti <- sp[which(sp@data$Kunta_ni1 == "Lahti"),]

# Haetaan Corine rasteri Lahden alueelta
# HUOM: jos aineistoa haetaan suuren tai useamman kunnan alueelta, on syytä
# käyttää pienempää resoluutiota kuin 25x25m (esim 100x100m)
# HUOM: toistaiseksi WMS-palvelun karttakerroksia (layer) ei voi kysellä soRvin
# kautta, vaan kerroksen nimi pitää tietää (ominaisuus tulossa)
corine.lahti <- GetWMSraster(WMS=corine.wms,
                             layer='CorineLandCover2006_25m',
                             extent=sp.lahti,
                             resolution=25)

# Liitetään rasterin 3 kaistaa yhdeksi (RGB)
brick.corine.lahti <- brick(corine.lahti)
# raster-paketin plotRGB sopii plottaamiseen
plotRGB(brick.corine.lahti)
# Lisätään kuntaraja
plot(sp.lahti, add=TRUE, lwd=2)

# Toistetaan sama ortoilmakuville
image.lahti <- GetWMSraster(WMS=ortoilma.wms,
                            layer='Image2006mosaiikki',
                            extent=sp.lahti,
                            resolution=25)

brick.image.lahti <- brick(image.lahti)
plotRGB(brick.image.lahti)
plot(sp.lahti, add=TRUE, lwd=2)

# Toistetaan sama Natura2000-alueille
natura.lahti <- GetWMSraster(WMS=suojelu.wms,
                             layer='ProtectedSites.Natura2000Polygons',
                             extent=sp.lahti,
                             resolution=25)

brick.natura.lahti <- brick(natura.lahti)
plotRGB(brick.natura.lahti)
plot(sp.lahti, add=TRUE, lwd=2)

# Toistetaan sama pohjavesialueille
pohjavesi.lahti <- GetWMSraster(WMS=pohjavesi.wms,
                                layer='Pohjavesialue',
                                extent=sp.lahti,
                                resolution=25)

brick.pohjavesi.lahti <- brick(pohjavesi.lahti)
plotRGB(brick.pohjavesi.lahti)
plot(sp.lahti, add=TRUE, lwd=2)

