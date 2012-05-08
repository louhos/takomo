
file.list <- c()
for (di in dir("StatFin.filtered")) {
  d2 <- dir(paste("StatFin.filtered/", di, sep = ""))
  for (di2 in d2) {
    fs <- dir(paste("StatFin.filtered/", di, "/", di2, sep = ""))

    fs <- paste(paste("StatFin.filtered/", di, "/", di2, "/", sep = ""), fs[grep(".csv.gz", fs)], sep = "")

    file.list <- c(file.list, fs)

  }
}

fs <- file.list[grep("csv.gz", file.list)]

tablist <- list()
for (f in fs) {
  print(f)
  tab <- read.csv(f, sep = "\t")
  tablist[[f]] <- tab
}

inds <- which(sapply(tablist, function (x) {any(c("Asuinmaakunta", "Maakunta", "maakunta", "alue", "Alue", "Aluejako", "Kunta", "kunta", "Vaalipiiri", "Vaalipiiri.ja.kunta", "Vaaliliitto.Puolue.Vaalipiiri", "Ehdokas..puolue.ja.kunta", "Ehdokas", "Seutukunta", "Suuralue") %in% colnames(x))}))
alue.tabs <- tablist[inds]

library(sorvi)
municipality.info <- GetMunicipalityInfo()

election.data <- names(alue.tabs)[grep("vaa/", names(alue.tabs))]


##############################################################################

nam <- "StatFin.filtered/vaa/evaa/120_evaa_tau_103_fi.csv.gz"
tmp <- cast(alue.tabs[[nam]], Vaalipiiri~Äänestystiedot~Lukumäärätiedot)
tab1 <- tmp[,,1]
tab2 <- tmp[,,2]
colnames(tab1) <- paste(colnames(tmp[,,"Lukumäärä"]), "(Lukumäärä)")
colnames(tab2) <- paste(colnames(tmp[,,"Osuus äänistä"]), "(Osuus äänistä)")
tab <- cbind(tab1, tab2)

##############################################################################

nam <- "StatFin.filtered/vaa/evaa/120_evaa_tau_104_fi.csv.gz"
tmp <- cast(alue.tabs[[nam]], Vaalipiiri.ja.kunta~Äänestystiedot~Lukumäärätiedot)
tab1 <- tmp[,,"Lukumäärä 2007"]
tab2 <- tmp[,,"Lukumäärä 2011"]
tab3 <- tmp[,,"-Osuus äänistä"]
tab4 <- tmp[,,"- Osuus äänistä"]


colnames(tab1) <- paste(colnames(tmp[,,"Lukumäärä 2007"]), "(Lukumäärä 2007)")
colnames(tab2) <- paste(colnames(tmp[,,"Lukumäärä 2011"]), "(Lukumäärä 2011)")
colnames(tab3) <- paste(colnames(tmp[,,"-Osuus äänistä"]), "(Osuus 2011)")
colnames(tab4) <- paste(colnames(tmp[,,"- Osuus äänistä"]), "(Osuus 2007)")
tab <- cbind(tab1, tab2, tab3, tab4)

##############################################################################

nam <- "StatFin.filtered/vaa/evaa/120_evaa_tau_105_fi.csv.gz"                     
tmp <- cast(alue.tabs[[nam]], Vaalipiiri~Hylkäysperuste)

tab <- tmp

##############################################################################

nam <- "StatFin.filtered/vaa/evaa/185_evaa_tau_101_fi.csv.gz"  
tmp <- cast(alue.tabs[[nam]], Vaaliliitto.Puolue.Vaalipiiri~Lukumäärä)

##############################################################################

nam <- "StatFin.filtered/vaa/evaa/186_evaa_tau_102_fi.csv.gz"
tmp <- cast(alue.tabs[[nam]], Alue~Puolue~Pienimmät.luvut)

##############################################################################

nam <- "StatFin.filtered/vaa/kvaa_1996/010_kvaa_tau_101_fi.csv.gz"
tmp <- cast(alue.tabs[[nam]], Alue~Puolue~Pienimmät.luvut)

##############################################################################

nam <- "StatFin.filtered/vaa/kvaa_2000/010_kvaa_2000_2008-10-17_tau_101_fi.csv.gz"
tmp <- cast(alue.tabs[[nam]], Alue~Äänestystiedot)
tab <- tmp

##############################################################################

nam <- "StatFin.filtered/vaa/kvaa_2000/020_kvaa_2000_2008-10-17_tau_102_fi.csv.gz"
tmp <- cast(alue.tabs[[nam]], Alue~Puolue~Ehdokastiedot)

tab1 <- tmp[,,"Ehdokkaiden lkm"]
tab2 <- tmp[,,"Ehdokkaiden osuus (%)"]
tab3 <- tmp[,,"Naisehdokkaiden lkm"]
tab4 <- tmp[,,"Naisten osuus ehdokkaista (%)"]

colnames(tab1) <- paste(colnames(tmp[,,"Ehdokkaiden lkm"]), "(Ehdokkaiden lkm)")
colnames(tab2) <- paste(colnames(tmp[,,"Ehdokkaiden osuus (%)"]), "(Ehdokkaiden osuus)")
colnames(tab3) <- paste(colnames(tmp[,,"Naisehdokkaiden lkm"]), "(Naisehdokkaiden lkm)")
colnames(tab4) <- paste(colnames(tmp[,,"Naisten osuus ehdokkaista (%)"]), "(Naisten osuus ehdokkaista)")
tab <- cbind(tab1, tab2, tab3, tab4)

##############################################################################

nam <- "StatFin.filtered/vaa/kvaa_2000/030_kvaa_2000_2008-10-17_tau_103_fi.csv.gz"
tmp <- cast(alue.tabs[[nam]], Alue~Puolue~Valittujen.tiedot)

tab1 <- tmp[,,"Valittujen lkm"]
tab2 <- tmp[,,"Valittujen osuus (%)"]
tab3 <- tmp[,,"Valittujen naisten lkm"]
tab4 <- tmp[,,"Naisten osuus valituista (%)"]

colnames(tab1) <- paste(colnames(tmp[,,"Valittujen lkm"]), "(Valittujen lkm)")
colnames(tab2) <- paste(colnames(tmp[,,"Valittujen osuus (%)"]), "(Valittujen osuus)")
colnames(tab3) <- paste(colnames(tmp[,,"Valittujen naisten lkm"]), "(Naisehdokkaiden lkm)")
colnames(tab4) <- paste(colnames(tmp[,,"Naisten osuus valituista (%)"]), "(Naisten osuus valituista)")
tab <- cbind(tab1, tab2, tab3, tab4)

##############################################################################

nam <- "StatFin.filtered/vaa/kvaa_2000/040_kvaa_2000_2008-10-17_tau_104_fi.csv.gz"

tmp <- cast(alue.tabs[[nam]], Alue~Puolue~Kannatustiedot)

tab1 <- tmp[,,"Ääniä yhtensä"]
tab2 <- tmp[,,"Ennakkoäänet"]
tab3 <- tmp[,,"Naisehdokkaiden äänimäärä"]
tab4 <- tmp[,,"Naisehdokkaiden osuus äänistä (%)"]
tab5 <- tmp[,,"Osuus äänistä (%)"]
tab6 <- tmp[,,"Osuus ennakkoäänistä (%)"]

colnames(tab1) <- paste(colnames(tmp[,,"Ääniä yhtensä"]), "(Ääniä yhtensä)")
colnames(tab2) <- paste(colnames(tmp[,,"Ennakkoäänet"]), "(Ennakkoäänet)")
colnames(tab3) <- paste(colnames(tmp[,,"Naisehdokkaiden äänimäärä"]), "(Naisehdokkaiden äänimäärä)")
colnames(tab4) <- paste(colnames(tmp[,,"Naisehdokkaiden osuus äänistä (%)"]), "(Naisehdokkaiden osuus äänistä (%))")
colnames(tab5) <- paste(colnames(tmp[,,"Osuus äänistä (%)"]), "(Osuus äänistä (%))")
colnames(tab6) <- paste(colnames(tmp[,,"Osuus ennakkoäänistä (%)"]), "(Osuus ennakkoäänistä (%))")

tab <- cbind(tab1, tab2, tab3, tab4, tab5, tab6)

############################################################

nam <- "StatFin.filtered/vaa/kvaa_2000/050_kvaa_2000_2008-10-17_tau_105_fi.csv.gz"
tmp <- cast(alue.tabs[[nam]], Ehdokas~Ehdokastiedot)
tab <- tmp

#############################################################