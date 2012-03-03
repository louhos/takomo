
# (C) 2011 Leo Lahti <leo.lahti@iki.fi> All rights reserved.
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses

# Tama esimerkki on testattu sorvi-paketin versiolla 0.1.42

# Skripti hakee Suomen kuntarajat ja vaestorekisterin asukaslukutiedot
# kunnittain, ja laskee sekä visualisoi sukupuolten suhteellisen
# osuuden eri kunnissa Suomen kartalla.

# Lataa sorvi-paketti
# Asennusohjeet: http://sorvi.r-forge.r-project.org/asennus.html
library(sorvi)

# hae suomen kartta ja kuntarajat gadm-muodossa
gadm <- GetGADM("FIN_adm", "Kunta")

# vaestorekisterin asukasluvut kunnittain
vrek <- GetPopulationRegister("http://vrk.fi/default.aspx?docid=5127&site=3&id=0")

# Liita vaestorekisterin tiedot karttaobjektiin ja
# aseta nollaan asukasluku kunnissa joiden osalta se ei ole tiedossa
gadm$asukkaita <- log10(rowSums(vrek[gadm$Kunta, c("Miehet", "Naiset")]))
gadm$asukkaita[is.na(gadm$asukkaita)] <- 0
# Laske myos sukupuolten suhteellinen osuus
gadm$miehet.osuus <- vrek[gadm$Kunta, "Miehet"]/vrek[gadm$Kunta, "Yhteensa"]
gadm$naiset.osuus <- vrek[gadm$Kunta, "Naiset"]/vrek[gadm$Kunta, "Yhteensa"]
# Aseta arvoon 50% miesten/naisten osuus
# kunnissa joiden osalta vakiluku ei ole tiedossa
gadm$miehet.osuus[is.na(gadm$miehet.osuus)] <- 0.5
gadm$naiset.osuus[is.na(gadm$naiset.osuus)] <- 0.5


# paletin rajapisteet
varname <- "naiset.osuus"
interval <- max(abs(gadm[[varname]] - 0.5))
at <- seq(0.5 - interval, 0.5 + interval, length = 100)

# Piirra Suomen kartta varitettyna naisten suhteellisen osuuden nojalla
q <- PlotShape(gadm, varname, type = "twoway",
at = at, main = "Naiset Suomen kunnissa")

# Save the Figure into a file:
#png("Suomen.kuntien.sukupuolijakauma.png", width = 600, height = 600)
print(q)
#dev.off()

