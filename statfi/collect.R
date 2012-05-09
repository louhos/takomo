library(sorvi)

source("~/Rpackages/louhos/soRvi-dev/pkg/R/elections.R")
source("~/Rpackages/louhos/soRvi-dev/pkg/R/get.province.info.R")

data(MML)
municipality.info <- GetMunicipalityInfo(MML = MML)

# Municipality-level data of parliamentary elections 2007 and 2011
tab <- GetParliamentaryElectionData("municipality")

# Match election data with other municipality data
inds <- match(municipality.info$Kunta, tab$Kunta)
colnames(tab) <- paste("Eduskuntavaalit", colnames(tab))
municipality.info <- cbind(municipality.info, tab[inds,])

write.table(municipality.info, file = "~/municipality.info.csv", sep = "\t", quote = FALSE, row.names = FALSE)

##############################################################################

# Vaalipiirikohtaista äänestysstatistiikkaa
# koottuna Tilastokeskukselta
tab <- GetParliamentaryElectionData("election.region")

write.table(tab, file = "~/election.region.info.csv", sep = "\t", quote = FALSE, row.names = FALSE)

##############################################################################

# Municipal elections 2000. Collect all data into one table.
tab <- GetMunicipalElectionData2000("all.municipality.level.data")

write.table(tab, file = "~/municipal.elections.2000.csv", sep = "\t", quote = FALSE, row.names = FALSE)

# This has no municipality info, just candidates vote stats
# tab <- GetMunicipalElectionData2000("selected.candidates.all")  

##############################################################################

# Kunnallisvaalit 2008
#    tab1 <- GetMunicipalElectionData2008("voting.stats")
#    tab2 <- GetMunicipalElectionData2008("party.votes")
#    tab3 <- GetMunicipalElectionData2008("parties.change")
#    tab4 <- GetMunicipalElectionData2008("selected.candidates.count")
#    tab5 <- GetMunicipalElectionData2008("woman.candidates")
#    tab6 <- GetMunicipalElectionData2008("election.statistics")

tab <- GetMunicipalElectionData2008("all.municipal")

###############################################

source("~/Rpackages/louhos/soRvi-dev/pkg/R/elections.R")
source("~/Rpackages/louhos/soRvi-dev/pkg/R/elections2000.R")
source("~/Rpackages/louhos/soRvi-dev/pkg/R/elections2008.R")
source("~/Rpackages/louhos/soRvi-dev/pkg/R/get.province.info.R")


nam <- "StatFin.filtered/vaa/evaa/185_evaa_tau_101_fi.csv.gz"  
tmp <- cast(alue.tabs[[nam]], Vaaliliitto.Puolue.Vaalipiiri~Lukumäärä)

##############################################################################

nam <- "StatFin.filtered/vaa/evaa/186_evaa_tau_102_fi.csv.gz"
tmp <- cast(alue.tabs[[nam]], Alue~Puolue~Pienimmät.luvut)

##############################################################################

nam <- "StatFin.filtered/vaa/kvaa_1996/010_kvaa_tau_101_fi.csv.gz"
tmp <- cast(alue.tabs[[nam]], Alue~Puolue~Pienimmät.luvut)

##############################################################################
