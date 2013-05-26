# This script is part of the Louhos-project (http://louhos.github.com/)

# Copyright (C) 2010-2013 Leo Lahti.
# Contact: <http://louhos.github.com/contact>. 
# All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# Skripti hakee Suomen kuntarajat ja vaestorekisterin asukaslukutiedot
# kunnittain, ja laskee sekä visualisoi sukupuolten suhteellisen
# osuuden eri kunnissa Suomen kartalla.

# Install and load sorvi package
# Instructions in http://louhos.github.com/sorvi/asennus.html
# This script is tested with sorvi version 0.2.27
library(sorvi)

# hae suomen kartta ja kuntarajat gadm-muodossa
gadm <- sorvi::GetGADM("FIN_adm", "Kunta")

# vaestorekisterin asukasluvut kunnittain
vrek <- sorvi::GetPopulationRegister("http://vrk.fi/default.aspx?docid=5127&site=3&id=0")

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
q <- sorvi::PlotShape(gadm, varname, type = "twoway",
at = at, main = "Naiset Suomen kunnissa")

# Save the Figure into a file:
#png("Suomen.kuntien.sukupuolijakauma.png", width = 600, height = 600)
print(q)
#dev.off()

