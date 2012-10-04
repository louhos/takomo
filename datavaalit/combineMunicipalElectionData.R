# This file is a part of the soRvi program
# louhos.github.com/sorvi/

# Copyright (C) Louhos (http://github.com/louhos)
# All rights reserved. Contact: <leo.lahti@iki.fi>

# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


##########################################################################

# Retrieve and preprocess Finnish election data tables 
# from Statistics Finland

##########################################################################

# Asenna ensin sorvi ja vaaditut riippuvuudet. 
# Asennusohjeet löytyvät osoitteesta:
# http://louhos.github.com/sorvi/asennus.html

# sorvi-paketin paikallinen asennus 
# install.packages("sorvi_0.1.89", repos = "louhos", branch = "develop")

library(sorvi)
tabs <- NULL

# Fix pxR
#source("statfi.px2csv.R")
source("read.pxr.fix.R")

##########################################################################

print("Tilastokeskus")

# (C) Tilastokeskus 2012
# "http://pxweb2.stat.fi/Database/Kuntien%20perustiedot/Kuntien%20perustiedot/Kuntaportaali.px

statfi <- GetMunicipalityInfoStatFi() 
municipalities <- rownames(statfi)
write.table(statfi[municipalities, ], file = "Tilastokeskus-KuntienAvainluvut.csv", sep = ";", quote = FALSE, row.names = FALSE)

tabs <- statfi[municipalities, ]

##########################################################################

print("MML")

# (C) MML 2012
# http://www.maanmittauslaitos.fi/aineistot-palvelut/digitaaliset-tuotteet/ilmaiset-aineistot/hankinta

LoadData("MML")
mml <- GetMunicipalityInfoMML(MML)    
write.table(mml[municipalities, ], file = "MML.csv", sep = ";", quote = FALSE, row.names = FALSE)

tabs <- cbind(tabs, mml[municipalities, ])

##########################################################################

print("Eduskuntavaalit 2007-2011")

tab <- GetParliamentaryElectionData("municipality")

# Match election data with other municipality data and write to output
# write.table(tab[municipalities,], file = "Eduskuntavaalit_2007_2011.csv", sep = ";", quote = FALSE, row.names = FALSE)

tabs <- cbind(tabs, tab[municipalities, ])

##############################################################################

print("Kunnallisvaalit 2000")

tab <- GetMunicipalElectionData2000("all.municipality.level.data")

# Match election data with other municipality data
write.table(tab[municipalities,], file = "Kunnallisvaalit2000.csv", sep = ";", quote = FALSE, row.names = FALSE)

tabs <- cbind(tabs, tab[municipalities, ])

###############################################

print("Kunnallisvaalit 2004")

tab <- GetMunicipalElectionData2004("all.municipal")

inds <- match(municipalities, rownames(tab))

write.table(tab[inds,], file = "Kunnallisvaalit2004.csv", sep = ";", quote = FALSE, row.names = FALSE)

tabs <- cbind(tabs, tab[inds, ])

##############################################################################

print("Kunnallisvaalit 2008")

can <- GetElectedCandidates(2008, "municipal", election.district) 

Helsingin vaalipiiri
http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/410_kvaa_2008_2009-11-02_tau_123_fi.px

Uudenmaan vaalipiiri
http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/420_kvaa_2008_2009-11-02_tau_124_fi.px

Varsinais-Suomen vaalipiiri
http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/430_kvaa_2008_2009-11-02_tau_125_fi.px

Satakunnan vaalipiiri
http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/440_kvaa_2008_2009-11-02_tau_126_fi.px

Hämeen vaalipiiri
http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/460_kvaa_2008_2009-11-02_tau_127_fi.px

Pirkanmaan vaalipiiri
http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/470_kvaa_2008_2009-11-02_tau_128_fi.px

Kymen vaalipiiri
http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/480_kvaa_2008_2009-11-02_tau_129_fi.px

Etelä-Savon vaalipiiri
http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/490_kvaa_2008_2009-11-02_tau_130_fi.px

Pohjois-Savon vaalipiiri
http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/500_kvaa_2008_2009-11-02_tau_131_fi.px

Pohjois-Karjalan vaalipiiri
http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/510_kvaa_2008_2009-11-02_tau_132_fi.px

Vaasan vaalipiiri
http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/520_kvaa_2008_2009-11-02_tau_133_fi.px

Keski-Suomen vaalipiiri
http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/530_kvaa_2008_2009-11-02_tau_134_fi.px

Oulun vaalipiiri
http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/540_kvaa_2008_2009-11-02_tau_135_fi.px

Lapin vaalipiiri
http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/550_kvaa_2008_2009-11-02_tau_136_fi.px


inds <- match(municipalities, rownames(tab))

write.table(tab[inds,], file = "Kunnallisvaalit2008.csv", sep = ";", quote = FALSE, row.names = FALSE)

tabs <- cbind(tabs, tab[inds, ])

###############################################

# Kaikki taulukot - kooste:

# write.table(tabs, file = "elections.csv", sep = ";", quote = FALSE, row.names = FALSE)

