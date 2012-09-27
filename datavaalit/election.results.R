# Preliminary functionality to read Finnish election result data 
# from Ministry of Justice web server. This script is based on
# example data set and schema documentation available at hs.fi: 
# http://dynamic.hs.fi/datat/om-esimerkkidata-huhtikuu.zip

##################################################################

# Copyright (C) 2012 Louhos <louhos.github.com>. All rights reserved.
# Author: Leo Lahti

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

##################################################################

# Function definitions

#' Description:
#' This function can be used to construct file name for Finnish Election data, 
#' according to the standardized naming scheme defined by the ministy of justice.
#' For details on example data and schema documentation, see data from hs.fi: 
#' http://dynamic.hs.fi/datat/om-esimerkkidata-huhtikuu.zip
#'
#' @param election Election name. Options: "presidentin vaali", "eduskuntavaalit", "kunnallisvaalit", "europarlamenttivaalit", "aluevaali", "kansanäänestys"
#' @param year Election year
#' @param stage Election stage. Options: "alustava", "tarkastus"
#' @param data Election data type. Options: "alue", "ehdokas", "puolue", "kansanäänestys" (this field is only used with file.type = "csv")
#' @param info Information type. Options: "äänestysaluetaso", "tilastotiedot" (only with file.type = "csv"), "ei.äänestysaluetasoa", ""
#' @param region Election region ID. Options: "01" ... "15", "maa"
#' @param suffix Optional suffix. Needed with example data sets.
#' @param file.type "csv" or "xml"
#'
#' @return File name
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

FileNameElectionData <- function (election, year, stage, data = NULL, info, 
                                  region, suffix = "", file.type = "csv") { 

  # Define ID conversions		     
  election.ids <- rbind(c("pv", "presidentin vaali"),
	                      c("e", "eduskuntavaalit"),
                  		  c("k", "kunnallisvaalit"),
                  		  c("epv", "europarlamenttivaalit"),
                  		  c("mkv", "aluevaali"),
                  		  c("vka", "kansanäänestys"))
  colnames(election.ids) <- c("id", "name")
  election.ids <- as.data.frame(election.ids)

  stage.ids <- rbind(c("a", "alustava"),
	                   c("t", "tarkastus"))
  colnames(stage.ids) <- c("id", "name")
  stage.ids <- as.data.frame(stage.ids)

  data.ids <- rbind(c("a", "alue"),
             	      c("e", "ehdokas"),
             	      c("p", "puolue"),
             	      c("k", "kansanäänestys"))
  colnames(data.ids) <- c("id", "name")
  data.ids <- as.data.frame(data.ids)	      

  info.ids <- rbind(c("a", "äänestysaluetaso"),
        	          c("t", "tilastotiedot"),
        	          c("y", "ei.äänestysaluetasoa"),
        	          c("", ""))
  colnames(info.ids) <- c("id", "name")
  info.ids <- as.data.frame(info.ids)	      

  # ---------------------------------------------

  # Convert the user-defined names into IDs
  election.id <- as.character(election.ids$id[which(election.ids$name == election)])
  year.id   <- as.character(year)
  stage.id  <- as.character(stage.ids$id[which(stage.ids$name == stage)])
  info.id   <- as.character(info.ids$id[which(info.ids$name == info)])
  region.id <- region
  if (!is.null(data)) {
    data.id   <- as.character(data.ids$id[which(data.ids$name == data)])
  }

  # TODO: add option to provide region.id as text instead of ID
  if (file.type == "csv") {
    fname <- paste(election.id, "-", year.id, suffix, "_", stage.id, data.id, 
                   info.id, "_", region.id, ".csv", sep = "")
  } else if (file.type == "xml") { 
    fname <- paste(election.id, "-", year.id, suffix, "_", stage.id, info.id, 
                   "_", region.id, ".xml", sep = "")
  }

  fname

}

#' Description:
#' Function for reading in Finnish Municipal Election candidate data published
#' by Ministry of justice. As of 27-09-2012, the data and descriptions are
#' available from http://192.49.229.35/K2012/s/ehd_listat/kokomaa.htm#ladattavat
#'
#' Candidate data comes in divided into 14 Election districts (vaalipiiri).
#'
#' @param district.id integer marking the election district ID. Options: [1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
#' @param cache character directory path to location where files are cached
#'
#' @return Data frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Joona Lehtomaki \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

ReadCandidates <- function(district.id, cache=NA) {
  
  # Body of the filename is always the same
  file.name.body <- "ehd_"
  
  # Coerce the disrict id into a character for building file paths / urls
  district.id.char <- as.character(district.id)
  
  # Padding with leading zeros if needed
  if (nchar(district.id.char) == 1) {
    district.id.char <- paste("0", district.id.char, sep="")
  }
  
  # Construct the file name
  file.name <- paste(file.name.body, district.id.char, ".csv", sep="")
  
  # Either use the cached files or fetch over network
  if (is.na(cache)) {
    
    data.source <- paste("http://192.49.229.35/K2012/s/ehd_listat/",
                          file.name, sep="")
    
    message(paste("Reading data from URL", data.source))
    
  } else {
    
    if (file.exists(cache)) {
      data.source <- file.path(cache, file.name)
      
      # Check if the actual file exists
      if (!file.exists(data.source)) {
        stop(paste("File", data.source, "does not exist."))
      } else {
        message(paste("Using cached version", data.source))
      }
      
    } else {
      stop("Cache requested, but not found")
    }
    
    # Read the table over network, use the encodign provided by MoJ
  }
  # Read the data from selected data source
  raw.data <- read.table(data.source, sep=";", as.is=TRUE, strip.white=TRUE,
                         fileEncoding="iso-8859-1")
  
  # In the original csv file, there is also a trailing ";" -> there really is
  # only 29 columns
  raw.data <- raw.data[1:29]
  
  # Get the suitable header from common_data.json
  header <- .readCommoData()
  header <- header$OMehdokkaat$header
  colnames(raw.data)  <- header
  
  return(raw.data)
  
}

ReadAllCandidates <- function(cache=NA) {
  
  election.district.ids  <- 1:15
  # Remember, there is no id 5!
  election.district.ids  <- election.district.ids[-c(5)]
  # Determine the cache dir if needed
  #cache.dir <- "/home/jlehtoma/Data/Datavaalit2012/OM-ehdokasdata/ehdokkaat"
  
  all.districts <- lapply(election.district.ids, 
                          function(x) {ReadCandidates(x, cache)})
  
  # Bind everything into a single data frame
  candidates <- do.call("rbind", all.districts)
  
  return(candidates)
}


# Private functions -------------------------------------------------------

.readCommoData <- function(data.file="common_data.json") {
  library(rjson)
  
  return(fromJSON(paste(readLines(data.file), collapse = "")))
} 

# ---------------------------------------------------------------

# Reading CSV example files:

# OK
#k-2012-tlt_aaa_maa.csv
csv <- FileNameElectionData("kunnallisvaalit", 2012, "alustava", "alue", 
                            "äänestysaluetaso", "maa", suffix = "-tlt")
tab <- read.csv(csv, sep = ";")

# OK
#k-2012-tlt_aea_maa.csv
csv <- FileNameElectionData("kunnallisvaalit", 2012, "alustava", "ehdokas", 
                            "äänestysaluetaso", "maa", suffix = "-tlt")
tab <- read.csv(csv, sep = ";")

# Reading fails. TODO: Fix this.
#k-2012-tlt_apa_maa.csv
csv <- FileNameElectionData("kunnallisvaalit", 2012, "alustava", "puolue", 
                            "äänestysaluetaso", "maa", suffix = "-tlt")
#tab <- read.csv(csv, sep = ";")

# ----------------------------------------------------------------

# Reading XML example file:

#k-2012-tlt_aa_maa.xml
xml <- FileNameElectionData(election = "kunnallisvaalit", year = 2012, stage = "alustava", info = "äänestysaluetaso", region = "maa", suffix = "-tlt", file.type = "xml")

# The best way to read the election XML?
# tab <- read.xml(xml, sep = ";")

# ----------------------------------------------------------------

# TODO: 
# 1) Headers are missing from the CSV files. Can these be defined based on the 
#    standard efinitions in the OM documentation? 
# 2) Validate the tables manually
# 3) Add preprocessing for the tables if appropriate
# 4) Routines to fetch data for individual candidates etc.?
# 5) Analyses, combined with other data?
# 6) Etc.


# Canidates ---------------------------------------------------------------

# Election district files are:
# Helsingin vaalipiiri = ehd_01.csv
# Uudenmaan vaalipiiri = ehd_02.csv
# Varsinais-Suomen vaalipiiri = ehd_03.csv
# Satakunnan vaalipiiri= ehd_04.csv
# Hämeen vaalipiiri = ehd_06.csv
# Pirkanmaan vaalipiiri = ehd_07.csv
# Kymen vaalipiiri = ehd_08.csv
# Etelä-Savon vaalipiiri = ehd_09.csv
# Pohjois-Savon vaalipiiri = ehd_10.csv
# Pohjois-Karjalan vaalipiiri = ehd_11.csv
# Vaasan vaalipiiri = ehd_12.csv
# Keski-Suomen vaalipiiri = ehd_13.csv
# Oulun vaalipiiri = ehd_14.csv
# Lapin vaalipiiri = ehd_15.csv

election.district.ids  <- 1:15
# Remember, there is no id 5!
election.district.ids  <- election.district.ids[-c(5)]
# Determine the cache dir if needed
cache <- "/home/jlehtoma/Data/Datavaalit2012/OM-ehdokasdata/ehdokkaat"

candidates <- ReadAllCandidates(cache)

# Dump into a csv file (for Teelmo)
write.table(candidates, "MoJ_canidates_finland.csv", sep=";", quote=FALSE,
            fileEncoding="iso-8859-1")


