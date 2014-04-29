# soRvi-paketin ja suositeltujen riippuvuuksien asennusskripti R-kielelle
# Lisätietoa projektista: louhos.github.com/sorvi

# (C) 2011-2013 Louhos (louhos.github.com). 
# All rights reserved.
# <louhos@googlegroups.com>
# License: FreeBSD (keep this notice).

# Kayttoohjeet: 
# 1) Asenna vaaditut jarjestelmariippuvuudet:
#     http://louhos.github.com/sorvi/asennus.html
#
# 2) Lataa ja aja tama skripti R:n komentorivilta:
#    (asentaminen edellyttaa toimivaa verkkoyhteytta)

if (!require(devtools, quietly = quietly)) {try(install.packages("devtools"), silent = silent)}
library(devtools) 
install_github(repo = "sorvi", username = "louhos")
       
# Riippuvuuksien asentaminen.
# HUOM: Mahdolliset virhekohdat ohitetaan automaattisesti. 
#

skip <- TRUE

if (!skip) {

# Suppress error messages during installation
silent <- TRUE
quietly <- TRUE

# Install the packages
if (!require(gdata, quietly = quietly)) {try(install.packages("gdata"), silent = silent)}
if (!require(ggplot2, quietly = quietly)) {try(install.packages("ggplot2"), silent = silent)}
if (!require(googleVis, quietly = quietly)) {try(install.packages("googleVis"), silent = silent)}
if (!require(gpclib, quietly = quietly)) {try(install.packages("gpclib"), silent = silent)}
if (!require(gridExtra, quietly = quietly)) {try(install.packages("gridExtra"), silent = silent)}
if (!require(mapproj, quietly = quietly)) {try(install.packages("mapproj"), silent = silent)}
if (!require(maps, quietly = quietly)) {try(install.packages("maps"), silent = silent)}
if (!require(maptools, quietly = quietly)) {try(install.packages("maptools"), silent = silent)}
if (!require(plyr, quietly = quietly)) {try(install.packages("plyr"), silent = silent)}
if (!require(png, quietly = quietly)) {try(install.packages("png"), silent = silent)}
if (!require(pxR, quietly = quietly)) {try(install.packages("pxR"), silent = silent)}
if (!require(raster, quietly = quietly)) {try(install.packages("raster"), silent = silent)}
if (!require(RColorBrewer, quietly = quietly)) {try(install.packages("RColorBrewer"), silent = silent)}
if (!require(RCurl, quietly = quietly)) {try(install.packages("RCurl"), silent = silent)}
if (!require(ReadImages, quietly = quietly)) {try(install.packages("ReadImages"), silent = silent)}
if (!require(rgdal, quietly = quietly)) {try(install.packages("rgdal"), silent = silent)}
if (!require(rgeos, quietly = quietly)) {try(install.packages("rgeos"), silent = silent)}
if (!require(rgl, quietly = quietly)) {try(install.packages("rgl"), silent = silent)}
if (!require(RgoogleMaps, quietly = quietly)) {try(install.packages("RgoogleMaps"), silent = silent)}
if (!require(rjson, quietly = quietly)) {try(install.packages("rjson"), silent = silent)}
if (!require(rworldmap, quietly = quietly)) {try(install.packages("rworldmap"), silent = silent)}
if (!require(sp, quietly = quietly)) {try(install.packages("sp"), silent = silent)}
if (!require(spdep, quietly = quietly)) {try(install.packages("spdep"), silent = silent)}
if (!require(XLConnect, quietly = quietly)) {try(install.packages("XLConnect"), silent = silent)}
if (!require(XML, quietly = quietly)) {try(install.packages("XML"), silent = silent)}

}
