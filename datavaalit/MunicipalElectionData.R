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
# install.packages("sorvi_0.1.92", repos = "louhos", branch = "develop")

library(sorvi)

# Fix pxR
source("read.pxr.fix.R")

election.districts <- setdiff(1:15, 5) # There is no district 5

###############################################

print("Kunnallisvaalit 2004")
tabs2004 <- lapply(election.districts, function (id) {GetElectedCandidates(2004, "municipal", election.district = id, verbose = TRUE) })

candidates2004 <- do.call(rbind, tabs2004)

save(candidates2004, file = "candidates2004.RData", compress = "xz")

###############################################

print("Kunnallisvaalit 2008")

tabs2008 <- lapply(election.districts, function (id) {GetElectedCandidates(2008, "municipal", election.district = id, verbose = TRUE) })

candidates2008 <- do.call(rbind, tabs2008)

save(candidates2008, file = "candidates2008.RData", compress = "xz")

#################################################

print("Kunnallisvaalit 2012")

candidates2012 <- ReadAllCandidates()

save(candidates2012, file = "candidates2012.RData", compress = "xz")

###################################################

# Fields in the same order, and discrepant fields in the end
coms <- sort(intersect(colnames(candidates2004), colnames(candidates2012)))
candidates2004 <- candidates2004[, c(coms, c("Äänestysalue", "Alue"), sort(setdiff(colnames(candidates2004), c(coms, c("Äänestysalue", "Alue")))))]
candidates2008 <- candidates2008[, c(coms, c("Äänestysalue", "Alue"), sort(setdiff(colnames(candidates2008), c(coms, c("Äänestysalue", "Alue")))))]
candidates2012 <- candidates2012[, c(coms, sort(setdiff(colnames(candidates2012), coms)))]

###################################################




###################################################

# Dump into a csv file
write.table(candidates2004, "municipal_elections_candidates_2004_finland.csv", sep=";", quote=FALSE, fileEncoding="iso-8859-1", row.names = FALSE)

# Dump into a csv file
write.table(candidates2008, "municipal_elections_candidates_2008_finland.csv", sep=";", quote=FALSE, fileEncoding="iso-8859-1", row.names = FALSE)

# Dump into a csv file
write.table(candidates2012, "municipal_elections_candidates_2012_finland.csv", sep=";", quote=FALSE, fileEncoding="iso-8859-1", row.names = FALSE)


