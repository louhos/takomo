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

# Install and load sorvi package
# Instructions in http://louhos.github.com/sorvi/asennus.html
# This script is tested with sorvi version 0.2.27
library(sorvi)


##########################################################################

#
# This script was used to retrieve and preprocess 
# Finnish election data tables for HSOpen 5/2012
#

##########################################################################

tabs <- NULL

print("Tilastokeskus")

print("Create output directory HSOpen")
system("mkdir HSOpen")

# (C) Tilastokeskus 2012
statfi <- sorvi::GetMunicipalityInfoStatFi() 
municipalities <- rownames(statfi)
write.table(statfi[municipalities, ], file = "HSOpen/Tilastokeskus-KuntienAvainluvut.csv", sep = ";", quote = FALSE, row.names = FALSE)

tabs <- statfi[municipalities, ]

##########################################################################

print("MML")

# (C) MML 2012
# http://www.maanmittauslaitos.fi/aineistot-palvelut/digitaaliset-tuotteet/ilmaiset-aineistot/hankinta
mml <- sorvi::GetMunicipalityInfoMML()    
write.table(mml[municipalities, ], file = "HSOpen/MML.csv", sep = ";", quote = FALSE, row.names = FALSE)

tabs <- cbind(tabs, mml[municipalities, ])

##########################################################################

print("Eduskuntavaalit 2007-2011")

tab <- sorvi::GetParliamentaryElectionData("municipality")

# Match election data with other municipality data and write to output
write.table(tab[municipalities,], file = "HSOpen/Eduskuntavaalit_2007_2011.csv", sep = ";", quote = FALSE, row.names = FALSE)

tabs <- cbind(tabs, tab[municipalities, ])

##############################################################################

print("Kunnallisvaalit 2000")

tab <- sorvi::GetMunicipalElectionData2000("all.municipality.level.data")

# Match election data with other municipality data
write.table(tab[municipalities,], file = "HSOpen/Kunnallisvaalit2000.csv", sep = ";", quote = FALSE, row.names = FALSE)

tabs <- cbind(tabs, tab[municipalities, ])

###############################################

print("Kunnallisvaalit 2004")

tab <- sorvi::GetMunicipalElectionData2004("all.municipal")

inds <- match(municipalities, rownames(tab))

write.table(tab[inds,], file = "HSOpen/Kunnallisvaalit2004.csv", sep = ";", quote = FALSE, row.names = FALSE)

tabs <- cbind(tabs, tab[inds, ])

##############################################################################

print("Kunnallisvaalit 2008")

tab <- sorvi::GetMunicipalElectionData2008("all.municipal")

inds <- match(municipalities, rownames(tab))

write.table(tab[inds,], file = "HSOpen/Kunnallisvaalit2008.csv", sep = ";", quote = FALSE, row.names = FALSE)

tabs <- cbind(tabs, tab[inds, ])

###############################################

# Kaikki taulukot - kooste:

write.table(tabs, file = "HSOpen/HSOpenKuntadata.csv", sep = ";", quote = FALSE, row.names = FALSE)

