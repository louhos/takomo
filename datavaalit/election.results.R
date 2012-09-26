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

FileNameElectionData <- function (election, year, stage, data = NULL, info, region, suffix = "", file.type = "csv") { 

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
    fname <- paste(election.id, "-", year.id, suffix, "_", stage.id, data.id, info.id, "_", region.id, ".csv", sep = "")
  } else if (file.type == "xml") { 
    fname <- paste(election.id, "-", year.id, suffix, "_", stage.id, info.id, "_", region.id, ".xml", sep = "")
  }

  fname

}

# ---------------------------------------------------------------

# Reading CSV example files:

# OK
#k-2012-tlt_aaa_maa.csv
csv <- FileNameElectionData("kunnallisvaalit", 2012, "alustava", "alue", "äänestysaluetaso", "maa", suffix = "-tlt")
tab <- read.csv(csv, sep = ";")

# OK
#k-2012-tlt_aea_maa.csv
csv <- FileNameElectionData("kunnallisvaalit", 2012, "alustava", "ehdokas", "äänestysaluetaso", "maa", suffix = "-tlt")
tab <- read.csv(csv, sep = ";")

# Reading fails. TODO: Fix this.
#k-2012-tlt_apa_maa.csv
csv <- FileNameElectionData("kunnallisvaalit", 2012, "alustava", "puolue", "äänestysaluetaso", "maa", suffix = "-tlt")
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







