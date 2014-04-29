# This file is a part of the soRvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2012 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.




#' Preprocess lukio data
#'
#'
#' Preprocess data about Finnish high school performance in year 2011
#' 
#' @return list Data for all high schools and separately for the Helsinki Region
#'
#' @author Juuso Parkkinen \email{louhos@@googlegroups.com}
#' @export
GetLukiot <- function() {

  message("Loading Lukiot data...")

  # Circumwent warnings
  hr.lukiot <- NULL    		   
  
  # Script for processing Finnish school data
  # License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
  # Copyright 2011 Juuso Parkkinen, juuso.parkkinen@gmail.com. All rights reserved.

  # NOTE! Changed code to load preprocessed hr.lukiot data from my Dropbox, as the original code has some problems with the school addresses - Juuso 26.10.2012
  con <- url("http://dl.dropbox.com/u/792906/data/PKS_lukiot_20111017.RData")
  load(con)
  close(con)
  lukiot <- NA
  
#   .InstallMarginal("XML")
#   .InstallMarginal("gdata")
#   
#   # Read data about high school performance (HS 31.5.2011)  
#   u <- "http://www.hs.fi/kotimaa/artikkeli/1135266565425"
#   tables <- XML::readHTMLTable(u)
#   lukiot <- tables[[1]]
#   
#   # Fix some fields and remove broken ones
#   lukiot$Ylioppilaita <- as.numeric(as.vector(lukiot[[4]]))
#   lukiot$Keskiarvo <- round(as.numeric(gsub(",", ".", as.vector(lukiot[[5]]))), digits=1)
#   lukiot$Ranking <- paste(as.vector(lukiot[[1]]), ".", sep="")
#   lukiot <- lukiot[-c(1, 4, 5)]
#   
#   # Get subset of Helsinki region schools, drop "aikuislukiot"
#   #hr.lukiot <- subset(lukiot, Kunta %in% c("Helsinki", "Espoo", "Vantaa", "Kauniainen"))
#   hr.lukiot <- lukiot[lukiot$Kunta %in% c("Helsinki", "Espoo", "Vantaa", "Kauniainen"),]
# 
#   #hr.lukiot <- subset(hr.lukiot, Kunta %in% c("Helsinki", "Espoo", "Vantaa", "Kauniainen"))
#   hr.lukiot <- hr.lukiot[hr.lukiot$Kunta %in% c("Helsinki", "Espoo", "Vantaa", "Kauniainen"),]
# 
#   hr.lukiot <- hr.lukiot[-grep("aikuis", hr.lukiot$Koulu),]
#   hr.lukiot <- droplevels(hr.lukiot)
#   
#   # Query OSM with high school names
#   hr.lukiot$lon <- hr.lukiot$lat <- NA
#   lukio.names <- tolower(paste(gsub(" ", "+", hr.lukiot$Koulu), hr.lukiot$Kunta, sep=","))
#   for (i in 1:length(lukio.names)) {
#     Sys.sleep(1)
#     latlon <- GetGeocodeOpenStreetMap(lukio.names[i])
#     if (!is.null(latlon)) {
#       hr.lukiot$lat[i] <- latlon[1]
#       hr.lukiot$lon[i] <- latlon[2]
#     }
#   }
#   
#   # Some coordinates are missing
#   message("Some school coordinates are missing. Try fetching them manually.")
#   # hr.lukiot$Koulu[is.na(hr.lukiot$lon)]
#   # Get mising coordinates manually with school addresses (should find a better way!)
#   addresses <- c("Kevatkatu+2,Helsinki", "Kalevankatu+8,Helsinki", "Urheilukatu+10-12,Helsinki", 
#                  "Kettutie+6,Helsinki", "Mantytie+14,Helsinki", "Laajalahdentie+21,Helsinki", 
#                  "Sandelsgatan+3,Helsinki", "Unioninkatu+2,Helsinki", "Elevhemsvagen+23,Grankulla",
#                  "Kasavuorentie+1,Kauniainen", "Pietari+Hannikaisen+tie+6,Helsinki", "Martinlaaksontie+36,Vantaa",
#                  "Ylastontie+3,Vantaa", "Sotungintie+19,Vantaa", "Lucina+Hagmanin+polku+4,Helsinki",
#                  "Moisiontie+3,Helsinki", "Makipellontie+19,Helsinki", "Louhentie+3,Helsinki",
#                  "Arkadiankatu+26,Helsinki", "Rintinpolku+2,Helsinki", "Arentipolku+1+,Helsinki")
#   
#   # Query OSM with high school addresses
#   lats <- lons <- rep(NA, length(addresses))
#   for (i in 1:length(addresses)) {
#     Sys.sleep(1)
#     latlon <- GetGeocodeOpenStreetMap(addresses[i])
#     if (!is.null(latlon)) {
#       lats[i] <- latlon[1]
#       lons[i] <- latlon[2]
#     }
#   }
#   
#   # Add the rest of the coordinates
#   stopifnot(length(which(is.na(hr.lukiot$lon))) == length(lons))
#   hr.lukiot$lat[is.na(hr.lukiot$lat)] <- lats
#   hr.lukiot$lon[is.na(hr.lukiot$lon)] <- lons
  
#   # Convert to UTF-8
#   hr.lukiot$Koulu <- factor(iconv(hr.lukiot$Koulu, from="ISO-8859-1", to="UTF-8"))
  
  # Save final data
  message("DONE\n")
  return(list(lukiot=lukiot, hr.lukiot=hr.lukiot))
}
