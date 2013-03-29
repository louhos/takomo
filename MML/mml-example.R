# MML 3D surface visualization examples contributed by
# Janne Aukia

# Copyright (C) 2013 Louhos <louhos.github.com>. 
# All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 

# Tested with sorvi 0.2.25
# rgl requires in some systems installation of glu-dev ja freeglut-dev
		  

# Install and load libraries
if (try(library(devtools)) == "try-error") {install.packages("devtools")}
library(devtools)
install_github("sorvi", "louhos", ref = "develop")
library(sorvi) # http://louhos.github.com/sorvi

# KAUPPATORI - 2M MAP DATA
mat <- ReadASC(gzfile("data/L4133C.asc.gz"))
tsel <- NormalizeValues(mat[0:600,2400:3000])
PlotSurface(tsel/4)

# KAUPPATORI - 10M MAP DATA
mat <- ReadXYZ(gzfile("data/L4133C.xyz.gz"))
tsel <- NormalizeValues(mat[0:120,480:600])
PlotSurface(tsel/16)

