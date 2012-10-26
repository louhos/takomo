inds <- match(municipalities, rownames(tab))

write.table(tab[inds,], file = "Kunnallisvaalit2008.csv", sep = ";", quote = FALSE, row.names = FALSE)

tabs <- cbind(tabs, tab[inds, ])

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

# Kaikki taulukot - kooste:

# write.table(tabs, file = "elections.csv", sep = ";", quote = FALSE, row.names = FALSE)

