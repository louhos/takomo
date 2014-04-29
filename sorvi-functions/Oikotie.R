# This file is a part of the soRvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2012 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.



#' Preprocess Oikotie data
#'
#' Preprocess data about Finnish apartment prices from Oikotie in 
#' years 2010-2011. First download and unzip data from http://www2.hs.fi/extrat/hsnext/oikotie-data.zip, and store the resulting CSV files.
#'
#' @return list with Oikotie myynnit data
#'
#' @author Juuso Parkkinen \email{louhos@@googlegroups.com}
#' @export
GetOikotie <- function() {
     
  message("Loading Oikotie data...") 
  #  library(gdata)
    # First download and unzip data from http://www2.hs.fi/extrat/hsnext/oikotie-data.zip

  temp <- tempfile()
  download.file("http://www2.hs.fi/extrat/hsnext/oikotie-data.zip", temp)
  myynnit <- read.csv(unz(temp, "myynnit.csv"), sep=";", quote=";", fileEncoding="ISO-8859-1")
  unlink(temp)
#  myynnit <- read.csv("data/myynnit.csv", sep=";", quote="", fileEncoding="ISO-8859-1")
  
  # Fix formats, and remove lines with errors (additional ';'s)
  myynnit$Size <- as.numeric(gsub(pattern=",", replacement=".", as.vector(myynnit$Size)))
  myynnit$Price <- as.numeric(gsub(pattern=",", replacement=".", as.vector(myynnit$Price)))
  myynnit$Floor <- as.numeric(as.vector(myynnit$Floor))
  myynnit$Apartment.condition <- as.numeric(as.vector(myynnit$Apartment.condition))
  myynnit <- myynnit[-unique(c(which(is.na(myynnit$Size)), which(is.na(myynnit$Floor)))),]
  
  # Compute price per square meter and fix zip codes
  myynnit$Price.per.square <- myynnit$Price / myynnit$Size
  myynnit$Zip.code <- as.character(myynnit$Zip.code)
  for (i in 2:4)
    myynnit$Zip.code[nchar(myynnit$Zip.code)==i] <- paste(paste(rep("0", 5-i), collapse=""), myynnit$Zip.code[nchar(myynnit$Zip.code)==i], sep="") 
  
  # Filter data based on price and size
  myynnit <- myynnit[-which(myynnit$Price <= 3),]
  myynnit <- myynnit[-which(myynnit$Price > 1000000),] 
  myynnit <- myynnit[-which(myynnit$Size < 10),]
  myynnit <- myynnit[-which(myynnit$Size > 500),]
  myynnit <- myynnit[-which(myynnit$Price.per.square < 500),]
  
  # Extract street names
  myynnit$Location <- factor(iconv(myynnit$Location, from="ISO-8859-1", to="UTF-8"))
  
  streets <- strsplit(as.vector(myynnit$Location), split=" ")
  streets2 <- sapply(streets, function(x) paste(x[1:(length(x)-1)], collapse=" "))
  lengths <- sapply(streets, length)
  streets2[grep("Hennalankuja", streets2)] <- "Hennalankuja"
  streets2[lengths %in% c(5,6)] <- sapply(streets[lengths %in% c(5,6)], function(x) x[1])
  myynnit$Street <- streets2
  
  # Take only Helsinki region data (zip code begins with 00, 01, 02)
  zips <- unique(myynnit$Zip.code)
  zips.beginnings <- sapply(strsplit(zips, split=""), function(x) paste(x[1:2], collapse=""))
  zips.hr <- zips[zips.beginnings %in% c("00", "01", "02")]
  #hr.myynnit <- subset(myynnit, Zip.code %in% zips.hr)
  hr.myynnit <- myynnit[myynnit$Zip.code %in% zips.hr, ]
  
  # Fix encoding
#  myynnit$Location <- factor(iconv(myynnit$Location, from="ISO-8859-1", to="UTF-8"))
  myynnit$Street <- factor(iconv(myynnit$Street, from="ISO-8859-1", to="UTF-8"))
  myynnit$Room.configuration <- factor(iconv(myynnit$Room.configuration, from="ISO-8859-1", to="UTF-8"))
  hr.myynnit$Location <- factor(iconv(hr.myynnit$Location, from="ISO-8859-1", to="UTF-8"))
  hr.myynnit$Street <- factor(iconv(hr.myynnit$Street, from="ISO-8859-1", to="UTF-8"))
  hr.myynnit$Room.configuration <- factor(iconv(hr.myynnit$Room.configuration, from="ISO-8859-1", to="UTF-8"))
  
  message("DONE")
  return(list(myynnit=myynnit, hr.myynnit=hr.myynnit))
}
