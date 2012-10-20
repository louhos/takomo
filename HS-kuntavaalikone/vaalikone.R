# Read and preprocess HS vaalikone data. Initial scripts.

# (C) Louhos (louhos.github.com/contact.html)
# License: FreeBSD

# 15.10.2012
# http://blogit.hs.fi/hsnext/vaalikoneen-arvokartan-taustatiedot-avoimena-datana
# http://files.snstatic.fi/hs/2012/10/vaalikone121012.zip


#library(gdata)
#tab <- read.xls("ehdokasvastaukset-12-10-2012-verkkoon.xlsx")
tab <- read.csv("ehdokasvastaukset-12-10-2012-verkkoon.txt.gz", sep = "\t")

question <- tab[, grep("Kysymys", colnames(tab))]
ans <- tab[, grep("Vastaus", colnames(tab))]
w <- tab[, grep("Painotus", colnames(tab))]

# Order the levels
for (i in 1:ncol(w)) {
  levels(w[, i]) <- c("", "Pieni merkitys", "Keskisuuri merkitys", "Suuri merkitys")
}
w[w == ""] <- NA
