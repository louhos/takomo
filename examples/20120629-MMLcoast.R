# This script is part of the Louhos-project (http://louhos.github.com/)

# Copyright (C) 2010-2013 Leo Lahti and Joona Lehtomäki.
# Contact: <http://louhos.github.com/contact>. 
# All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# Install and load sorvi package
# Instructions in http://louhos.github.com/sorvi/asennus.html
# This script is tested with sorvi version 0.2.27
library(sorvi)

# NOTE: color scales have not been optimized for this example

# Load coast data
coast1 <- sorvi::LoadMML(data.id = "coast", resolution = "4_5_milj_shape_etrs-tm35fin")

# Add random variable
coast1@data$myvar <- rnorm(nrow(coast1@data))

# Plot
p1 <- sorvi::PlotShape(coast1, varname = "myvar")

#coast2 <- LoadMML(data.id = "coast_p", resolution = "1_milj_Shape_etrs_shape")
#p2 <- PlotShape(coast2, varname = "SHAPE_Area")