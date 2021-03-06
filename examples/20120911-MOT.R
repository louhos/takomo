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

# This script retrieves information of the state support for
# Finnish companies from MOT YLE website
# (YLE is a national broadcasting company in Finland);
# calculates the total subsidies obtained in each county;
# merges with National Land Survey (MML) county region data;
# visualizes the total subsidies.
# Shortcomings: for data retrieval demonstration purposes only;
# not normalized against the number of years available for
# each county etc.; see the MOT data for details.

# Install and load sorvi package
# Instructions in http://louhos.github.com/sorvi/asennus.html
# This script is tested with sorvi version 0.2.27
library(sorvi)

# Load required packages
# Remember to install required packages (e.g. 'install.packages("plyr")')
library(plyr)


# Read Finnish company support information from YLE MOT website
# Source: (C) Yle MOT-toimitus; published under CC-BY-SA 3.0
tuet <- sorvi::GetMOTYritystuet()
 
# Read county borders from Land Survey Finland (MML) data
# Hosted at the datavaalit.fi site
# (C) MML 2011
maakuntadata <- sorvi::LoadMML(data.id = "maaku1_p", resolution = "1_milj_Shape_etrs_shape")
 
# Calculate total company support for each county (absolute)
kokonaistuki <- plyr::ddply(tuet, .(maakunta), function (x) {sum(na.omit(x$maksettu.summa))})
names(kokonaistuki) <-  c("maakunta", "kokonaistuki")
# same, relative to the county which got least subsidies
kokonaistuki$suhteellinen.kokonaistuki <- kokonaistuki$kokonaistuki/min(kokonaistuki$kokonaistuki)
 
# Add company subsidies to the maakuntadata info table
maakuntadata$yritystuki <- kokonaistuki[match(maakuntadata$Maakunta.FI, kokonaistuki$maakunta), "suhteellinen.kokonaistuki"]
 
# Visualize total company subsidies in each county
pic <- sorvi::PlotShape(maakuntadata, "yritystuki", type = "oneway", main = "Suhteellinen yritystuki", ncol = 100, max.color = "blue")
 
# Save the image
#png("MOT-yritystuki.png")
print(pic)
#dev.off()

