# This file is a part of the soRvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2012 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Load Finnish company funding information from MOT website
#'
#' Source: (C) Yle MOT-toimitus; published under CC-BY-SA 3.0
#' For data documentation, see http://ohjelmat.yle.fi/mot/10_9
#' 
#' @param url URL 
#'
#' @return data frame
#' 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @export
#' @examples # tuet <- GetMOTYritystuet()

GetMOTYritystuet <- function (url = "http://yle.fi/fst/mot-2012-yritystuet/data/mot_yritystuet_data.csv") {

  tuet <- read.csv(url, header = TRUE, sep = ";", fileEncoding = "iso-8859-1"); 
  tuet$maksettu.summa <- as.numeric(gsub(" ", "", tuet$maksettu.summa))
  tuet$lainat <- as.numeric(gsub(" ", "", tuet$lainat))
  tuet$maakunta <- iconv(tuet$maakunta, from = "iso-8859-1", to = "utf-8")
  tuet$maakunta[tuet$maakunta == "Ahvenanmaa"] <- "Ahvenanmaan maakunta" 

  tuet

}

