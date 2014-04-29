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

# Load required packages
# Remember to install required packages (e.g. 'install.packages("ggplot2")')
library(ggplot2)
library(WDI)


# --------------------------

# Select investigated variable
k <- 27
indi <- WDI::WDIsearch()[k,1]
titl <- WDI::WDIsearch()[k,2]

# Get the data
DF <- WDI::WDI(country="all", indicator=indi, start=1990, end=2008)

# Visualize
theme_set(theme_bw(20)) 
p <- ggplot(data = subset(DF, country == "Finland")) + aes(x = year, y = EN.ATM.CO2E.KD.GD) + geom_line() + ggtitle(titl) + xlab("Year") + ylab("CO2 emissions")

# Store into file by uncommenting #
#png("FinlandCO2.png")
print(p)
#dev.off()


