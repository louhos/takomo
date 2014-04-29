# Shiny script

# This script is part of the Louhos-project (http://louhos.github.com/)

# Copyright (C) 2012-2013 Juuso Parkkinen and Leo Lahti.
# Contact: <http://louhos.github.com/contact>. 
# All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

library(shiny)
library(googleVis)
library(reshape2)
library(ggplot2)
library(grid)
setwd("/Users/juusoparkkinen/Documents/workspace/Rdrafts/demos/sotkanet/sotkanet_shiny/")

load("../sotkanet_data_20130908.RData")
load("../Sotkanet_MunicipalityData_20130908.RData")
