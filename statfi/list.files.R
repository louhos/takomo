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
