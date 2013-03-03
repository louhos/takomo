# Copyright (C) 2012 Louhos (louhos.github.com)
# Contact: louhos@googlegroups.com
# All rights reserved.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License:
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This script has been tested with soRvi version 0.2.12
library(sorvi)

# NOTE: color scales have not been optimized for this example

# Load coast data
coast1 <- LoadMML(data.id = "coast", resolution = "4_5_milj_shape_etrs-tm35fin")

# Add random variable
coast1@data$myvar <- rnorm(nrow(coast1@data))

# Plot
p1 <- PlotShape(coast1, varname = "myvar")

#coast2 <- LoadMML(data.id = "coast_p", resolution = "1_milj_Shape_etrs_shape")
#p2 <- PlotShape(coast2, varname = "SHAPE_Area")