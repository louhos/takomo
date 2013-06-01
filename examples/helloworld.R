# (C) Louhos 2011-2013 / Leo Lahti
# simple sorvi helloworld example
# This downloads municipality information data from statfi (Tilastokeskus)
# and produces basic visualization

# Load libraries
# sorvi installation instructions: 
# http://louhos.github.com/sorvi/asennus.html
library(sorvi)

# Hae kuntatason tiedot. Lähteet: Tilastokeskus ja Maanmittauslaitos.
suppressWarnings(municipality.info <- GetMunicipalityInfo())

# Tutki taulukon sisaltoa
print(names(municipality.info))

# Plot example
xvar <- 13 #"Valtionveronalaiset tulot, euroa/tulonsaaja  2011"
yvar <- 3 #"Taajama-aste, % 1.1.2011"
x <- municipality.info[[xvar]]
y <- municipality.info[[yvar]]
lab <- municipality.info[["Kunta"]]

# Remove 'Koko maa'
keep <- (!lab == "Koko maa")
x <- x[keep]
y <- y[keep]
lab <- lab[keep]


plot(log10(x), y, type = "n", xlab = paste(names(municipality.info)[xvar], "log10"), ylab = names(municipality.info)[yvar], main = "Tulot vs. taajama-aste")
text(log10(x), y, labels = lab, cex = 0.8)
