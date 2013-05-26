# (C) Louhos 2011-2013 / Leo
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
plot(x, y, type = "n", xlab = names(municipality.info)[xvar], ylab = names(municipality.info)[yvar], main = "Tulot vs. taajama-aste")
text(x, y, labels = lab, cex = 0.8)
