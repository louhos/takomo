# soRvi-paketin ja suositeltujen riippuvuuksien asennusskripti R-kielelle
# Lisätietoa projektista: louhos.github.com/sorvi

# (C) 2011-2012 Louhos (louhos.github.com). 
# All rights reserved.
# <sorvi-commits@lists.r-forge.r-project.org>
# License: FreeBSD (keep this notice).

# Kayttoohjeet: 
# 1) Asenna vaaditut jarjestelmariippuvuudet:
#     http://sorvi.r-forge.r-project.org/asennus.html
#
# 2) Aja tama skripti R:n komentorivilta kirjoittamalla
#    source("http://sorvi.r-forge.r-project.org/examples/sorvi.installation.R")
#    (asentaminen edellyttaa toimivaa verkkoyhteytta)
#
# HUOM: Mahdolliset virhekohdat ohitetaan automaattisesti. 
#

# Suppress error messages during installation
#silent <- TRUE
#quietly <- TRUE
#if (!require(devtools, quietly = quietly)) {try(install.packages("devtools"), silent = silent)}

install.packages(devtools)
library(devtools)
install_github(repo = "soRvi-dev", username = "louhos", type = "source")




