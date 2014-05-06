# This file is a part of the helsinki package (http://github.com/rOpenGov/helsinki)
# in association with the rOpenGov project (ropengov.github.io)

# Copyright (C) 2010-2014 Juuso Parkkinen, Leo Lahti and Joona Lehtomäki / Louhos <louhos.github.com>. 
# All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


# Script for retrieving the 'aluejakokartat' data with the fingis package and saving for helsinki package.
# Script run on 6.5.2014.

# Install fingis from github
install.packages("devtools")
library(devtools)
install_github(repo="fingis", username="ropengov")
library(fingis)

# Get list of aluejakokartat data
data.list <- get_Helsinki_aluejakokartat()
names(data.list) <- data.list

# Get first all spatial data objects
aluejakokartat <- list(sp=lapply(data.list, function(dat) get_Helsinki_aluejakokartat(map.specifier=dat, data.dir="TEMPDIR")))

# Declare Encoding to UTF-8
# Following this: http://stackoverflow.com/questions/18837855/making-non-ascii-data-suitable-for-cran
aluejakokartat$sp[1:7] <- lapply(aluejakokartat$sp[1:7], function(dat) {res=dat; res@data$Name =  as.character(res@data$Name); Encoding(res@data$Name) = "UTF-8"; res})
aluejakokartat$sp$aanestysalue@data$Nimi <- as.character(aluejakokartat$sp$aanestysalue@data$Nimi)
Encoding(aluejakokartat$sp$aanestysalue@data$Nimi) <- "UTF-8"


# # Convert characters to ASCII (required by R packages)
# # Converts to bytes, not optimal...
# # To convert these back, needs first gsub for each character and then iconv
# aluejakokartat$sp[1:7] <- lapply(aluejakokartat$sp[1:7], function(dat) {res=dat; res@data$Name = iconv(res@data$Name, from="UTF-8", to="ASCII", sub="byte"); res})
# aluejakokartat$sp$aanestysalue@data$Nimi <- iconv(aluejakokartat$sp$aanestysalue@data$Nimi, from="UTF-8", to="ASCII", sub="byte")

# # # Try to convert back
# temp <- lapply(aluejakokartat$sp[1:7], function(dat) {res=dat; res@data$Name = gsub("<c3><96>", "ö", res@data$Name); res})
# # temp2 <- lapply(temp, function(dat) {res=dat; res@data$Name = iconv(res@data$Name, from="ASCII", to="UTF-8", sub="byte"); res})
# # temp3 <- lapply(temp, function(dat) {res=dat; res@data$Name = iconv(res@data$Name, from="US-ASCII", to="UTF-8"); res})
# # temp3 <- lapply(temp, function(dat) {res=dat; res@data$Name = iconv(res@data$Name, from="ASCII", to="UTF-16"); res})

# # Alternative
# temp <- lapply(aluejakokartat$sp[1:7], function(dat) {res=dat; res@data$Name = iconv(res@data$Name, from="UTF-8", to="ASCII"); res})



# Remove TEMPDIR
file.remove(file.path("TEMPDIR", dir("TEMPDIR")))
file.remove("TEMPDIR")

# Add also transformed dataframe objects
aluejakokartat$df <- lapply(aluejakokartat$sp[1:7], function(dat) sp2df(sp=dat, region="Name"))
aluejakokartat$df[["aanestysalue"]] <- sp2df(sp=aluejakokartat$sp$aanestysalue, region="Nimi")

# Save 
filename <- "./data/aluejakokartat.rda"
save(aluejakokartat, file=filename)

# Resave with 'tools' to get better compression
library(tools)
checkRdaFiles(filename)
#                                size ASCII compress version
# ./data/aluejakokartat.RData 2580380 FALSE     gzip       2
resaveRdaFiles(filename)
checkRdaFiles(filename)
#                                size ASCII compress version
# ./data/aluejakokartat.RData 1081128 FALSE       xz       2