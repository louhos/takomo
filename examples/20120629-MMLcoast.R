# Copyright (C) 2012 Louhos (louhos.github.com)
# Contact: sorvi-commits@lists.r-forge.r-project.org
# All rights reserved.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License:
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This script was implemented with soRvi version 0.1.79
library(sorvi)
LoadData("MML")

coast1 <- MML[["4_5_milj_shape_etrs-tm35fin"]][["coast"]]
PlotShape(coast1, varname = "SHAPE_Area")

coast2 <- MML[["1_milj_Shape_etrs_shape"]][["coast_p"]]
PlotShape(coast2, varname = "SHAPE_Area")