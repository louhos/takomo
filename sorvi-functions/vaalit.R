# This file is a part of the soRvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2013 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


# Function for reading the Presidentti2012 data
# For documentation, see
# http://www2.hs.fi/extrat/hsnext/Vaalikone_API_20111207.pdf

#' Load Vaalipiiri information
#' Useful for mapping election data to other municipality information
#'
#' @param url URL for Tilastokeskus vaalipiirit.
#'
#' @return data.frame listing election regions (Vaalipiiri), region IDs (Aluenumero) and municipalities (Alue)
#' 
#' @author Juuso Parkkinen and Leo Lahti \email{louhos@@googlegroups.com}
#' @export

GetVaalipiiri <- function (url = "http://www.stat.fi/meta/luokitukset/vaalipiiri/001-2012/luokitusavain_kunta.html") {

  .InstallMarginal("XML")
  
  message(paste("Reading Vaalipiiri information from ", url))
  # Read info of municipalities and election areas from Tilastoteskus
  temp <- XML::readHTMLTable(url)

  # Extract info that we want
  municipalities <- temp[[1]][-1,]
  municipalities$Vaalipiiri <- paste(as.vector(municipalities[,1]), as.vector(municipalities[,2]))
  municipalities <- municipalities[3:5]
  names(municipalities) <- c("Aluenumero", "Alue", "Vaalipiiri")

  # Fill missing Vaalipiiri info
  current.piiri <- NA
  for (i in 1:nrow(municipalities)) {
    # If vaalipiiri given, save it as current
#     if (!nchar(gsub(" ", "", municipalities[i,"Vaalipiiri"])) == 2) {
    if (municipalities[i,"Vaalipiiri"]!=" ") {
      current.piiri <- as.vector(municipalities[i,"Vaalipiiri"])
    } else { # Else add current vaalipiiri
      municipalities[i, "Vaalipiiri"] <- current.piiri
    }
  }

  municipalities

}



#' Load presidential election 2012 results from HS Next
#'
#' @param election.round Presidential election round (1/2)
#' @param level Optional. Pick results for particular region type. Options: "municipalities" (kunnat)
#' @return Votes table
#' 
#' @author Juuso Parkkinen and Leo Lahti \email{louhos@@googlegroups.com}
#' @export

GetElectionResultsPresidentti2012 <- function (election.round, level = NULL) {

    # Read first Election round

    ## Read 1st presidential election round votes from HS Next
    votes.url <- "http://www2.hs.fi/extrat/hsnext/presidentti1-tulos.csv"
    message(paste("Reading Finnish presidential election result data from", votes.url))
    votes <- read.csv(votes.url, sep=";")

    # Fix column names ("osuus" and "aania" are mixed with each other)
    names(votes) <- gsub("osuus", "temp", names(votes))
    names(votes) <- gsub("aania", "osuus", names(votes))
    names(votes) <- gsub("temp", "aania", names(votes))
    # Field 5: Aania yhteensa:
    votes[[5]] <- as.numeric(as.vector(gsub("None", "0", votes[[5]])))
  
    # Refine variable names
    names(votes) <- gsub("\\.", " ", names(votes))
    names(votes)[3:39] <- paste("1.K", names(votes)[3:39], sep=" ")
    
    votes1 <- votes

  
    # Read 2nd round votes from HS Next
    votes.url <- "http://www2.hs.fi/extrat/hsnext/presidentti2.csv"
    message(paste("Reading Finnish presidential election result data from", votes.url))
    votes <- read.csv(votes.url, sep=";", fileEncoding="ISO-8859-15")
 
    # Here the names are ok, but ',' has been used as the decimal separator
    bad.cols <- c(3,4,7,9,11,13,15)
    votes[,bad.cols] <- apply(votes[,bad.cols], 2, function(x) as.numeric(gsub(",", ".", x)))

    # Rows in votes1 and votes match perfectly with one exception:
    # votes1 is missing row 1995: 499021 K\"oklot
    # As we are now not interested in it, we simply remove it from
    # votes to make merging these two easier
    votes <- droplevels(votes[-1995,])
    names(votes) <- gsub("\\.", " ", names(votes))
    names(votes)[3:15] <- paste("2.K", names(votes)[3:15], sep=" ")
   
    votes2 <- votes
	
  # Vuoden 2012 alusta Lansi-Turunmaan kaupunki otti nimekseen Parainen
  # ja palasi nain aiemman Paraisten kaupungin vanhaan nimeen.
  levels(votes$Alue)[korvaa.skandit(as.character(levels(votes$Alue))) == "Lansi-Turunmaa"] <- "Parainen"

  # Get list of election region codes from Tilastokeskus
  message("Loading election region data from Tilastokeskus")
  url <- "http://www.stat.fi/meta/luokitukset/vaalipiiri/001-2012/luokitusavain_kunta.html"
  vaalipiirit <- sorvi::GetVaalipiiri(url)

  # Rename regions to match voting data
  levels(vaalipiirit$Alue)[levels(vaalipiirit$Alue)=="Maarianhamina - Mariehamn"] <- "Maarianhamina"

  if (level == "municipalities") {

    # Match vaalipiirit and first election round 
    municipality.rows <- votes1$Aluenumero %in% vaalipiirit$Aluenumero
    votes1 <- votes1[municipality.rows,]
    votes2 <- votes2[municipality.rows,]
    regions <- vaalipiirit[match(votes1$Aluenumero, vaalipiirit$Aluenumero),]

    # RegionIDs do not match exactly between election rounds 1/2 but
    # region names do, use them to match 1/2 rounds
    # Confirm that regions match on the first and second election round 
    if (!all(as.vector(votes1$Alue) == as.vector(votes2$Alue))) {
      stop("Election regions do not match between election rounds.")
    } 
  }

  if (election.round == 1) {
    votes <- votes1
  } else if (election.round == 2) {
    votes <- votes2
  }

  # Merge municipality IDs and names with first round election results
  votes <- droplevels(cbind(regions, votes[,3:ncol(votes)]))

  votes

}




#' Load Presidentti2012 data
#'
#' Load data from Presidentti2012 vaalikone
#' Note! You need a personal API key to get the data!
#'
#' @param category Data category ("questions", "candidates", "useranswers")
#' @param API Personal api key, required
#' @param ID id for the query, optional
#' @param filter filter for the query, required for 'useranswers'-category. ("question", "timerange", "topcandidate")
#' @param page Pagenumber of results to get, optional
#' @param per_page Number of answers to get (500-10000), optional
#' @param show_total Show data information, optional
#'
#' @return res List of data
#' 
#' @author Juuso Parkkinen \email{louhos@@googlegroups.com}
#' @export

GetPresidentti2012 <- function(category=c("questions", "candidates", "useranswers"), API, ID=NULL, filter=NULL, page=1, per_page=500, show_total="true") {

  # category=c("questions", "candidates", "useranswers"); ID=NULL; filter=NULL; page=1; per_page=500; show_total="true"
  # library(gdata) 

  .InstallMarginal("RCurl") 
  .InstallMarginal("rjson") 


  curl <- RCurl::getCurlHandle(cookiefile="")
  vaalikone.url <- paste("http://api.vaalikone.fi/presidentti2012/v1/", category, sep="")
  
  # Define parameters based on category
  if (category == "questions") {
    params <- list(api_key=API, id=ID)
    if (!is.null(ID))
      warning("Note! Parameter 'id' doesn't work with category 'questions'. Will return all questions.")
  } else if (category == "candidates") {
    params <- list(api_key=API, id=ID)
  } else if (category == "useranswers") {
    params <- list(api_key=API, filter=filter, page=page, per_page=per_page, show_total=show_total)  
  } else {
    stop("Invalid 'category' given!")
  }
  val <- RCurl::getForm(uri = vaalikone.url, .params = params, curl = curl, .encoding = "utf-8")
  res <- rjson::fromJSON(val)
  
  # Report error if given
  if (length(res$error) > 0) {
    stop(paste("ERROR! code: ", res$error$code, ", message: ", res$error$message, sep=""))
  } else {
    return(res)
  }
}


#' For Presidentti2012 candidate answers, form numerical rating in [0, 1] for the 
#' answer options (rougly corresponding to conservative-liberal axis)
#'
#' @param candidates candidate information as given by 
#'                   candidates <- GetPresidentti2012(category="candidates", API=API)
#' @param questions questions returned by GetPresidentti2012(category="questions", API=API)
#' @param type return the answer rating as integers ("integer") 0, 1, 2, ... 
#'        or as rates between [0,1].
#' @return matrix A matrix: each row corresponds to a candidate. For each candidate, 
#'              the answer options (columns) are rated within [0, 1]
#' 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @export

Presidentti2012CandidateAnswers2Numeric <- function (candidates, questions, type = "rate") {

  # Question IDs
  qids <- paste("Q", sapply(questions[[1]], function(x){x$id}), sep = "")

  # Single-option questions
  single.qids <- qids[sapply(questions[[1]], function(x){x$maxSelect}) == 1]  

  # Pick candidate answers
  mat <- matrix(NA, length(candidates$data), length(single.qids))
  #rownames(mat) <- paste("C", as.character(sapply(candidates$data, function(x) {x$id})), sep = "")
  rownames(mat) <- as.character(sapply(candidates$data, function(x) {x$lastname}))

  colnames(mat) <- single.qids

  for (cind in 1:length(candidates$data)) {

    #cidx <- paste("C", candidates$data[[cind]]$id, sep = "")
    cidx <- candidates$data[[cind]]$lastname
    ans <- sapply(candidates$data[[cind]]$answers, function(x){x$choices})
    names(ans) <- paste("Q", sapply(candidates$data[[cind]]$answers, function(x){x$question}), sep = "")  
    # Convert list() to NAs
    ans[sapply(ans, length) == 0] <- NA
				 
    mat[cidx, single.qids] <- as.character(unlist(ans)[single.qids])

  }

  # Convert choiceIDs to numeric
  mat2 <- sorvi::Presidentti2012ConvertOptionsToNumeric(mat, questions, type = type)
 
  mat2
}


#' For Presidentti2012 answers, form numerical rating in [0, 1] for the 
#' answer options (rougly corresponding to conservative-liberal axis)
#'
#' @param df data.frame giving the merged table of subjects vs. background information + answers
#'             The example script for obtaining this was posted to Louhos. 
#'             http://louhos.wordpress.com/2012/01/06/kenesta-seuraava-presidentti-ennusta-itse-hsn-vaalikonedatan-avulla/
#' @param questions questions returned by GetPresidentti2012(category="questions", API=API)
#' @param type return the answer rating as integers ("integer") 0, 1, 2, ... 
#'        or as rates between [0,1].
#'
#' @return list A list with two data.frames: info (user information) and 
#'         answer (user answers). The answer options are rated within [0, 1]. 
#'         Each row corresponds to a user in each of the two data.frames.
#' 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @export

Presidentti2012ConvertOptionsToNumeric <- function (df, questions, type = "rate") {

  # Convert matrices to data.frames
  if (is.matrix(df)) { df <- as.data.frame(df) }

  # Rate the choices
  choice.ratings <- sorvi::Presidentti2012RateChoices(questions, type = type)

  # Replace selection IDs by corresponding selection rates
  for (qid in names(choice.ratings)) {
    print(qid)
    df[, qid] <- as.numeric(choice.ratings[[qid]][as.character(df[, qid])])
  }

  user.info <- df[, 1:8] # do not include user ID (field 9)
  user.answers <- df[, -seq(1,9,1)]

  if ("Tulot" %in% colnames(user.info)) {
    # Luokittele tulotason mukaan ja jarjesta tasot
    Tuloluokka <- c("10000", "5000", "15000", "100000", "25000", "30000", "40000", "50000", "60000", "80000", "20000", NA)
    names(Tuloluokka) <- c("c(10000, 14999)", "c(5000, 9999)", "c(15000, 19999)", "c(1e+05, 999999)", "c(25000, 29999)", "c(30000, 39999)", "c(40000, 49999)",
"c(50000, 59999)", "c(60000, 79999)", "c(80000, 99999)",  "c(20000, 24999)", "NULL")
    user.info$Tuloluokka <- Tuloluokka[as.character(user.info$Tulot)]
    user.info$Tuloluokka[is.na(user.info$Tuloluokka)] <- "NULL"
    user.info$Tuloluokka <- factor(user.info$Tuloluokka, levels = c("NULL", "5000", "10000", "15000", "20000", "25000", "30000", "40000", "50000", "60000", "80000"))
  }

  if ("Ika" %in% colnames(user.info)) {
    user.info$Ika <- factor(user.info$Ika, levels =  c("NULL", "c(18, 19)", "c(20, 24)", "c(25, 29)", "c(30, 34)", "c(35, 39)", "c(40, 44)", "c(45, 49)", "c(50, 54)", "c(55, 59)", "c(60, 64)", "c(65, 69)", "c(70, 74)", "c(75, 79)", "c(80, 84)", "c(85, 89)", "c(90, 100)"))
  }

  list(info = user.info, answer = user.answers)

}


#' For Presidentti2012 answers, form numerical rating (in integers) for the 
#' answer options (rougly corresponding to the index on conservative-liberal axis)
#'
#' @param questions questions returned by GetPresidentti2012(category="questions", API=API)
#' @param type return the answer rating as integers ("integer") 0, 1, 2, ... 
#'        or as rates between [0,1].
#'
#' @return list A list: each element corresponds to a question. For each question, 
#'              the answer options are given an index, roughly corresponding to their
#'              position on conservative-liberal axis
#' 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @export

Presidentti2012RateChoices <- function (questions, type = "rate") {

  # Single-option questions
  qinds <- which(sapply(questions[[1]], function(x){x$maxSelect}) == 1)

  # Measure each choice with rate from 0...1 
  # and for each question (no multichoice questions).
  # list choiceID and the corresponding rate

  choice.ratings <- list()

  for (qind in qinds) {
    question.id <- paste("Q", questions[[1]][[qind]]$id, sep = "")

    choice.ids <- sapply(questions[[1]][[qind]]$choices, function (x) {x$id})

    if (type == "rate") {
      choice.rate <- seq(0, 1, length = length(choice.ids))    
    } else if (type == "integer") {
      choice.rate <- (1:length(choice.ids)) - 1
    }
    names(choice.rate) <- choice.ids 

    choice.ratings[[question.id]] <- choice.rate

  }

  choice.ratings
}


#' For Presidentti2012 answers, get answer IDs, text and rating
#' for the given question ID.
#'
#' @param question.id Question ID as in HS vaalikone (eg. numerical 80), or in soRvi e.g. character "Q80") 
#' @param questions questions returned by GetPresidentti2012(category="questions", API=API)
#'
#' @return list A list with the fields question, answer id, answer text and answer rate for the given question.
#' 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @export

Presidentti2012RetrieveAnswerText <- function (question.id, questions) { 
  qid <- as.numeric(gsub("Q", "", question.id))
  ans.text <- sapply(questions$data[[which(sapply(questions$data, function (x) {x$id}) == qid)]]$choices, function(x) {x$text})
  ans.id <- sapply(questions$data[[which(sapply(questions$data, function (x) {x$id}) == qid)]]$choices, function(x) {x$id})
  ans.rate <- seq(0, 1, length = length(ans.id))

  question <- questions$data[[which(sapply(questions$data, function (x) {x$id}) == qid)]]$text

  list(question = question, id = ans.id, text = ans.text, rate = ans.rate)
}


#' Preprocess Presidentti2012 question data
#'
#' @param questions output from 
#'        questions <- GetPresidentti2012(category="questions", API=API)
#' @return list A list with the fields Questions and Choices
#' @note A wrapper 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @export

PreprocessPresidentti2012 <- function (questions) {
  Questions <- data.frame(ID=sapply(questions$data, function(x) x$id))
  Questions$Text <- sapply(questions$data, function(x) x$text)
  Choices <- list(ID=lapply(questions$data, function(y) sapply(y$choices, function(x) x$id)))
  Choices$Text <- lapply(questions$data, function(y) sapply(y$choices, function(x) x$text))
  # Wrap texts for visualization
  Questions$TextWrapped <- lapply(Questions$Text, function(x) paste(strwrap(x, width=80), collapse="\n"))
  Choices$TextWrapped <- lapply(Choices$Text, function(x) sapply(x, function(y) paste(strwrap(y, width=40), collapse="\n")))
  list(Questions = Questions, Choices = Choices)
}


#' Get user answer data for HS vaalikone 2012 
#'
#' @param dates dates for which to retrieve data, for instance: c(paste("2011-11", 23:30, sep="-"), paste("2011-12", 1:31, sep="-"))
#' @param API API key
#' @param per.page maximum number of results to retrieve at one query in the for loop
#' @return dat.list list containing user answer data for the specified dates
#' @note A wrapper 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @export

Presidentti2012GetUserData <- function (dates, API, per.page = 10000) {

  dat.list <- list()
  for (di in 1:length(dates)) {

    filter <- paste("timerange:",dates[di], sep="")
    message("\n",filter, ", page 1...", appendLF=FALSE)

    # Get results (can download only 10000 at a time)
    dat <- sorvi::GetPresidentti2012(category = "useranswers", API = API, filter = filter, 
       				   page = 1, per_page = per.page, show_total = "true")

    # Check if more than per.page answers given
    ten.ks <- ceiling(dat$pagination$total / per.page)
    if (ten.ks > 1) { 
      # Get remaining results, per.page at a time
      for (t in 2:ten.ks) {
        message("page ", t, "... ", appendLF = FALSE)
        temp.dat <- sorvi::GetPresidentti2012(category = "useranswers", API = API, filter = filter, 
	    			page = t, per_page = per.page, show_total = "true")
        dat$data <- c(dat$data, temp.dat$dat)
    }
  }
  dat.list[[di]] <- dat
  }
  names(dat.list) <- dates

  dat.list
}



#' Preprocess user answer data for HS vaalikone 2012 
#'
#' @param dat.list Output from: dat.list <- Presidentti2012GetUserData(dates, API, per.page = 10000)
#' @param API API key
#' @return data.frame with user answer data
#' @note A wrapper 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @export


PreprocessPresidentti2012UserData <- function (dat.list, API = API) {

  questions <- sorvi::GetPresidentti2012(category="questions", API = API)
  Questions <- sorvi::PreprocessPresidentti2012(questions)$Questions

  # Construct a data frame
  Presidentti2012.df <- c()
  for (di in 1:length(dat.list)) {
    message(paste("Collecting the data", 100*di/length(dat.list), " percent.."))

    # Get respondent information
    info <- unlist(lapply(dat.list[[di]]$data, function(x) as.character(x[1:9])))
    info.mat <- matrix(info, ncol = 9, byrow = T)
    colnames(info.mat) <- names(dat.list[[di]]$data[[1]])[1:9]

    # Accept only those users who have answered to all questions
    # Get answers (not for Q14/ID70, because it is a multiple choice question)
    missing <- which(sapply(dat.list[[di]]$data, function(x) length(x$answers)) < 25)

    answer.list <- lapply(dat.list[[di]]$data[-missing], function(x) matrix(as.character(unlist(x$answers[-14])), ncol=2, byrow=T)[,2])
    answer.mat <- matrix(unlist(answer.list), nrow=length(answer.list), ncol=24, byrow=T)
    colnames(answer.mat) <- paste("Q", Questions$ID[-14], sep = "")

    # Join the matrices
    date.df <- cbind(as.data.frame(info.mat[-missing,]), as.data.frame(answer.mat))
    Presidentti2012.df <- rbind(Presidentti2012.df, date.df)

  }

  # Translate variable names and fix some of them
  names(Presidentti2012.df)[1:8] <- c("Paivamaara", "Koulutustaso", "Sukupuoli", "Tulot", "Ykkosehdokas", "Puolue", "Ika", "Asuinpaikka")
  levels(Presidentti2012.df$Sukupuoli) <- c("NULL", "Nainen", "Mies")[match(levels(Presidentti2012.df$Sukupuoli), c("NULL", "f", "m"))]
  Presidentti2012.df$Paivamaara <- as.Date(Presidentti2012.df$Paivamaara)

  # Get candidate data
  candidates <- sorvi::GetPresidentti2012(category = "candidates", API = API)

  # Match candidate IDs and names
  candidate <- sapply(candidates$data, function(x) x$lastname)  # candidate name
  names(candidate) <- sapply(candidates$data, function(x) x$id) # candidate ID
  levels(Presidentti2012.df$Ykkosehdokas) <- candidate[levels(Presidentti2012.df$Ykkosehdokas)]

  # Reorder factor levels, some by abundance, some in the natural way
  # 'attach' lets us use the factors without repeating the data frame name every time
  Presidentti2012.df$Koulutustaso <- reorder(Presidentti2012.df$Koulutustaso, Presidentti2012.df$id, length)
  Presidentti2012.df$Ykkosehdokas <- reorder(Presidentti2012.df$Ykkosehdokas, Presidentti2012.df$id, length)
  Presidentti2012.df$Puolue <- reorder(Presidentti2012.df$Puolue, Presidentti2012.df$id, length)
  Presidentti2012.df$Asuinpaikka <- reorder(Presidentti2012.df$Asuinpaikka, Presidentti2012.df$id, length)
  Presidentti2012.df$Tulot <- factor(Presidentti2012.df$Tulot, levels=levels(Presidentti2012.df$Tulot)[c(1,9,2,3,5:8,10:12,4)])

  Presidentti2012.df
  
}
  
#' GetParliamentaryElectionData
#'
#' Get parliamentary election data at selected regional level.
#' 
#' @param level Indicate whether to get data at the level of municipality or election.region
#' @return data.frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities
GetParliamentaryElectionData <- function (level) {

  .InstallMarginal("reshape2")
  .InstallMarginal("reshape")
  .InstallMarginal("plyr")  

  if (level == "municipality") {

    #http://pxweb2.stat.fi/database/StatFin/vaa/evaa/evaa_fi.asp

    # 2.2 Aanioikeutetut ja aanestaneet seka ennakolta aanestaneet sukupuolen mukaan kunnittain eduskuntavaaleissa 2011 ja 2007
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/evaa/evaa_2011/120_evaa_tau_104_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- try(as.data.frame(px))
    # Manually remove special chars from header
    #names(df) <- c("Aanestystiedot", "Lukumaaratiedot", "Vaalipiiri.ja.kunta", "dat")
    names(df) <- korvaa.skandit(names(df))
    for (i in 1:ncol(df)) {
      df[, i] <- korvaa.skandit(df[, i])
    }

    kaava <- as.formula("Vaalipiiri.ja.kunta~Aanestystiedot~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")

    # Separate tables and preprocess
    tab1 <- tmp[,,"Lukumaara 2007"]
    tab2 <- tmp[,,"Lukumaara 2011"]
    tab3 <- tmp[,,"-Osuus aanista"]
    tab4 <- tmp[,,"- Osuus aanista"]

    colnames(tab1) <- paste(colnames(tmp[,,"Lukumaara 2007"]), "(Lukumaara 2007)")
    colnames(tab2) <- paste(colnames(tmp[,,"Lukumaara 2011"]), "(Lukumaara 2011)")
    colnames(tab3) <- paste(colnames(tmp[,,"-Osuus aanista"]), "(Osuus 2011)")
    colnames(tab4) <- paste(colnames(tmp[,,"- Osuus aanista"]), "(Osuus 2007)")
    tab <- cbind(tab1, tab2, tab3, tab4)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Niista Ruotsissa", rnams)]
    rnams <- rnams[-grep("Suomessa asuvat Suomen kansalaiset", rnams)]
    rnams <- rnams[-grep("Ulkomailla asuvat Suomen kansalaiset", rnams)]
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # FIXME: make generic function to unify municipality names that have multiple versions
    # across different data sets
    rownames(tab) <- gsub("Pedersoren kunta", "Pedersore", rownames(tab))

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})

    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]

    # TODO
    #8.2 Pienin aanimaara ja vertausluku, jolla ehdokas on tullut valituksi 
    # puolueittain ja vaalipiireittain eduskuntavaaleissa 2011
    #url <- "http://pxweb2.stat.fi/database/StatFin/vaa/evaa/186_evaa_tau_102_fi.px"
    #Alue~Puolue~Pienimmat.luvut

  } else if (level == "election.region") {

    #http://pxweb2.stat.fi/database/StatFin/vaa/evaa/evaa_fi.asp

    #2.3 Hylatyt aanestysliput hylkaysperusteen ja vaalipiirin mukaan 
    # eduskuntavaaleissa 2011
    #http://pxweb2.stat.fi/database/StatFin/vaa/evaa/120_evaa_tau_105_fi.px

    # 8.1 Vaaliliitot ja niiden aanimaarat vaalipiireittain eduskuntavaaleissa 2011
    #url <- "http://pxweb2.stat.fi/database/StatFin/vaa/evaa/185_evaa_tau_101_fi.csv.gz"  
    #Vaaliliitto.Puolue.Vaalipiiri~Lukumaara
  
    #2.1 Aanioikeutetut ja aanestaneet seka ennakolta aanestaneet sukupuolen 
    # mukaan vaalipiireittain eduskuntavaaleissa 2011
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/evaa/120_evaa_tau_103_fi.px"

    # Read election data from Statistics Finland			 
    px <- sorvi::read.px(url, na.strings='"-"') 
    df <- try(as.data.frame(px))
    names(df) <- korvaa.skandit(names(df))
    #names(df) <- c("Aanestystiedot", "Lukumaaratiedot", "Vaalipiiri.ja.kunta", "dat")

    kaava <- as.formula("Vaalipiiri~Aanestystiedot~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")

    # Separate the tables
    tab1 <- tmp[,,1]
    tab2 <- tmp[,,2]
    colnames(tab1) <- paste(colnames(tmp[,,"Lukumaara"]), "(Lukumaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus aanista"]), "(Osuus aanista)")
    tab <- cbind(tab1, tab2)

    # Keep only election.region level data
    rnams <- rownames(tab)
    rnams <- rnams[grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    colnames(tab) <- paste("Eduskuntavaalit 2011", colnames(tab))

    tab$Vaalipiiri <- sapply(rnams, function (s) {ss <- strsplit(s, " ")[[1]]; paste(ss[-1], collapse = " ")})
    tab$Vaalipiiri.Koodi <- sapply(rnams, function (s) {strsplit(s, " ")[[1]][[1]]})

    # Read more election data from Statistics Finland			 
    px <- sorvi::read.px("http://pxweb2.stat.fi/database/StatFin/vaa/evaa/120_evaa_tau_105_fi.px", na.strings='"-"') 
    df <- try(as.data.frame(px))
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Vaalipiiri~Hylkaysperuste")
    tab2 <- reshape::cast(df, kaava, value="dat")

    # Keep only election.region level data
    rownames(tab2) <- as.character(tab2[,1])
    rnams <- rownames(tab2)
    rnams <- rnams[grep("vaalipiiri", rnams)]
    tab2 <- as.data.frame(tab2[rnams, ])

    colnames(tab2) <- paste("Eduskuntavaalit 2011", colnames(tab2))

    tab2$Vaalipiiri <- sapply(rnams, function (s) {ss <- strsplit(s, " ")[[1]]; paste(ss[-1], collapse = " ")})
    tab2$Vaalipiiri.Koodi <- sapply(rnams, function (s) {strsplit(s, " ")[[1]][[1]]})


    tab <- cbind(tab, tab2[match(tab$Vaalipiiri, tab2$Vaalipiiri),])
  
  }

  rownames(tab) <- tab$Kunta
  colnames(tab) <- paste("Eduskuntavaalit_2007_2011", colnames(tab))
  

  tab

}


  
#' GetMunicipalElectionData2000
#'
#' Get municipal election data from Statistics Finland (C) 2012
#' http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/kvaa_2000_fi.asp
#' 
#' @param which Indicate which of the available Statistics Finland data sets to parse. Options: election.statistics, candidates, selected.candidates.by.region, selected.candidates.all, parties, all.municipality.level.data
#' @return data.frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities
GetMunicipalElectionData2000 <- function (which = "election.statistics") {

  .InstallMarginal("plyr")
  .InstallMarginal("reshape2")
  .InstallMarginal("reshape")

  if (which == "election.statistics") {

    #Kunnallisvaalit 2000, aanestystiedot
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/010_kvaa_2000_2008-10-17_tau_101_fi.px"
    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Alue ~ Aanestystiedot")
    tab <- reshape::cast(df, kaava, value = "dat")
    rownames(tab) <- korvaa.skandit(as.character(tab$Alue))

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(as.character(rownames(tab)), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    # NOTE: election region information also available

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2000", colnames(tab))

  } else if (which == "candidates") {

    #Ehdokkaat puolueittain vaalipiirin ja kunnan mukaan kunnallisvaaleissa 2000
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/020_kvaa_2000_2008-10-17_tau_102_fi.px"
    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)

    tmp <- reshape::cast(df, Alue ~ Puolue ~ Ehdokastiedot, value="dat")

    tab1 <- tmp[,,"Ehdokkaiden lkm"]
    tab2 <- tmp[,,"Ehdokkaiden osuus (%)"]
    tab3 <- tmp[,,"Naisehdokkaiden lkm"]
    tab4 <- tmp[,,"Naisten osuus ehdokkaista (%)"]

    colnames(tab1) <- paste(colnames(tmp[,,"Ehdokkaiden lkm"]), "(Ehdokkaiden lkm)")
    colnames(tab2) <- paste(colnames(tmp[,,"Ehdokkaiden osuus (%)"]), "(Ehdokkaiden osuus)")
    colnames(tab3) <- paste(colnames(tmp[,,"Naisehdokkaiden lkm"]), "(Naisehdokkaiden lkm)")
    colnames(tab4) <- paste(colnames(tmp[,,"Naisten osuus ehdokkaista (%)"]), "(Naisten osuus ehdokkaista)")
    tab <- cbind(tab1, tab2, tab3, tab4)
    rownames(tab) <- korvaa.skandit(rownames(tab))

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2000", colnames(tab))

  } else if (which == "selected.candidates.by.region") {

    #Valitut puolueittain vaalipiirin ja kunnan mukaan kunnallisvaaleissa 2000
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/030_kvaa_2000_2008-10-17_tau_103_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    tmp <- reshape::cast(df, Alue ~ Puolue ~ Valittujen.tiedot, value="dat")

    tab1 <- tmp[,,"Valittujen lkm"]
    tab2 <- tmp[,,"Valittujen osuus (%)"]
    tab3 <- tmp[,,"Valittujen naisten lkm"]
    tab4 <- tmp[,,"Naisten osuus valituista (%)"]

    colnames(tab1) <- paste(colnames(tmp[,,"Valittujen lkm"]), "(Valittujen lkm)")
    colnames(tab2) <- paste(colnames(tmp[,,"Valittujen osuus (%)"]), "(Valittujen osuus)")
    colnames(tab3) <- paste(colnames(tmp[,,"Valittujen naisten lkm"]), "(Naisehdokkaiden lkm)")
    colnames(tab4) <- paste(colnames(tmp[,,"Naisten osuus valituista (%)"]), "(Naisten osuus valituista)")
    tab <- cbind(tab1, tab2, tab3, tab4)
    rownames(tab) <- korvaa.skandit(rownames(tab))

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2000", colnames(tab))

  } else if (which == "parties") {

    #Kunnallisvaalit 2000, puolueiden kannatus
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/040_kvaa_2000_2008-10-17_tau_104_fi.px"
    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)

    tmp <- reshape::cast(df, Alue ~ Puolue ~ Kannatustiedot, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Aania yhtensa"]
    tab2 <- tmp[,,"Ennakkoaanet"]
    tab3 <- tmp[,,"Naisehdokkaiden aanimaara"]
    tab4 <- tmp[,,"Naisehdokkaiden osuus aanista (%)"]
    tab5 <- tmp[,,"Osuus aanista (%)"]
    tab6 <- tmp[,,"Osuus ennakkoaanista (%)"]

    colnames(tab1) <- paste(colnames(tmp[,,"Aania yhtensa"]), "(Aania yhteensa)")
    colnames(tab2) <- paste(colnames(tmp[,,"Ennakkoaanet"]), "(Ennakkoaanet)")
    colnames(tab3) <- paste(colnames(tmp[,,"Naisehdokkaiden aanimaara"]), "(Naisehdokkaiden aanimaara)")
    colnames(tab4) <- paste(colnames(tmp[,,"Naisehdokkaiden osuus aanista (%)"]), "(Naisehdokkaiden osuus aanista (%))")
    colnames(tab5) <- paste(colnames(tmp[,,"Osuus aanista (%)"]), "(Osuus aanista (%))")
    colnames(tab6) <- paste(colnames(tmp[,,"Osuus ennakkoaanista (%)"]), "(Osuus ennakkoaanista (%))")

    tab <- cbind(tab1, tab2, tab3, tab4, tab5, tab6)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2000", colnames(tab))

  } else if (which == "selected.candidates.all") {

    #Kunnallisvaalit 2000, valitut ehdokkaat
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/050_kvaa_2000_2008-10-17_tau_105_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    tab <- reshape::cast(df, Ehdokas ~ Ehdokastiedot, value="dat")

  } else if (which == "all.municipality.level.data") {

    tab1 <- sorvi::GetMunicipalElectionData2000("election.statistics")
    tab2 <- sorvi::GetMunicipalElectionData2000("candidates")
    tab3 <- sorvi::GetMunicipalElectionData2000("selected.candidates.by.region")
    tab4 <- sorvi::GetMunicipalElectionData2000("parties")

    municipalities <- sort(rownames(tab1))
    tab <- cbind(tab1[municipalities, ],
             tab2[municipalities, ],
      	     tab3[municipalities, ],
      	     tab4[municipalities, ])
  }

  tab

}

  
#' GetMunicipalElectionData2004
#'
#' Get municipal election data from Statistics Finland 2012
#' 
#' Taulukot tilastossa: 5. Kunnallisvaalit 2004 - vaalitulos, aanestaminen
#' http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/2004_05_fi.asp
#'
#' @param which Indicate which of the available Statistics Finland data sets to parse. 
#' @return data.frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities
GetMunicipalElectionData2004 <- function (which = "election.statistics") {

  .InstallMarginal("plyr")
  .InstallMarginal("reshape2")
  .InstallMarginal("reshape")

  if (which == "election.statistics") {

    #Kunnallisvaalit 2004, aanestystiedot
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/010_KVAA_2004_2008-07-23_TAU_101_FI.px"
    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))    

    kaava <- as.formula("Alue~Aanestystiedot~Sukupuoli")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Sukupuolet yhteensa"]
    tab2 <- tmp[,,"Miehet"]
    tab3 <- tmp[,,"Naiset"]

    colnames(tab1) <- paste(colnames(tmp[,,"Sukupuolet yhteensa"]), "(Sukupuolet yhteensa)")
    colnames(tab2) <- paste(colnames(tmp[,,"Miehet"]), "(Miehet)")
    colnames(tab3) <- paste(colnames(tmp[,,"Naiset"]), "(Naiset)")

    tab <- cbind(tab1, tab2, tab3)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    rnams <- rnams[-grep(" 00", rnams)]
    rnams <- rnams[-grep(" 01", rnams)]
    rnams <- rnams[-grep(" 02", rnams)]
    rnams <- rnams[-grep(" 03", rnams)]
    rnams <- rnams[-grep(" 04", rnams)]
    rnams <- rnams[-grep(" 05", rnams)]
    rnams <- rnams[-grep(" 06", rnams)]
    rnams <- rnams[-grep(" 07", rnams)]
    rnams <- rnams[-grep(" 08", rnams)]
    rnams <- rnams[-grep(" 09", rnams)]
    rnams <- rnams[-grep(" 1", rnams)]
    rnams <- rnams[-grep(" 2", rnams)]
    rnams <- rnams[-grep(" 3", rnams)]
    rnams <- rnams[-grep(" 4", rnams)]
    rnams <- rnams[-grep(" 5", rnams)]
    rnams <- rnams[-grep("Manner-Suomi", rnams)]
    #rnams <- rnams[-grep(" 6", rnams)]
    #rnams <- rnams[-grep(" 7", rnams)]
    #rnams <- rnams[-grep(" 8", rnams)]
    #rnams <- rnams[-grep(" 9", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: coarse election region (vaalipiiri) information also available but discarded
    # NOTE: detailed election region information also available (below municipality level) but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2004 aanestystiedot", colnames(tab))

  } else if (which == "selected.candidates.by.election.region") {

    warning("Vaalipiiri level information; TODO")

    #Valittujen lukumaara ja prosenttiosuudet puolueittain ja vaalipiireittain kunnallisvaaleissa 2004
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-08-28_tau_107_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Puolue~Vaalipiiri~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")

    tab1 <- tmp[,,"Valtuutettujen lukumaara"]
    tab2 <- tmp[,,"Puolueen osuus"]

    colnames(tab1) <- paste(colnames(tmp[,,"Valtuutettujen lukumaara"]), "(Valtuutettujen lukumaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Puolueen osuus"]), "(Puolueen osuus)")

    tab <- cbind(tab1, tab2)
    
    tab <- NULL

  } else if (which == "selected.candidates.count") {

    # Kunnallisvaalit 2004, valittujen lukumaara
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/010_kvaa_2004_2008-08-28_tau_103_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Alue~Puolue~Sukupuoli~Valittujen.lukumaara")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Kaikki ehdokkaat", "Valittujen lukumaara"]
    colnames(tab1) <- paste("Kaikki ehdokkaat", "Valittujen lukumaara", colnames(tab1))

    tab2 <- tmp[,,"Miesehdokkaat", "Valittujen lukumaara"]
    colnames(tab2) <- paste("Miesehdokkaat", "Valittujen lukumaara", colnames(tab2))

    tab3 <- tmp[,,"Naisehdokkaat", "Valittujen lukumaara"]
    colnames(tab3) <- paste("Naisehdokkaat", "Valittujen lukumaara", colnames(tab3))

    tab4 <- tmp[,,"Kaikki ehdokkaat", "Osuus valituista %"]
    colnames(tab4) <- paste("Kaikki ehdokkaat", "Osuus valituista %", colnames(tab4))

    tab5 <- tmp[,,"Miesehdokkaat", "Osuus valituista %"]
    colnames(tab5) <- paste("Miesehdokkaat", "Osuus valituista %", colnames(tab5))

    tab6 <- tmp[,,"Naisehdokkaat", "Osuus valituista %"]
    colnames(tab6) <- paste("Naisehdokkaat", "Osuus valituista %", colnames(tab6))

    regs <- rownames(tab1)

    tab <- cbind(tab1[regs,], tab2[regs,], tab3[regs,], 
    	         tab4[regs,], tab5[regs,], tab6[regs,])

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    rnams <- rnams[-grep("Manner-Suomi", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    
    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2004 valittujen lukumaara", colnames(tab))
    
  } else if (which == "selected.candidates.by.party") {  

    # Valittujen lukumaara ja prosenttiosuudet puolueittain ja 
    # vaalipiireittain kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/650_kvaa_2004_2009-11-02_tau_141_fi.px"

    warning("Vaalipiiritason tietoa. TODO.")
    tab <- NULL

    #px <- sorvi::read.px(url, na.strings='"-"')
    #df <- as.data.frame(px)
    #tmp <- reshape::cast(df, Alue~Puolue~Sukupuoli~Valittujen.lukumaara)

  } else if (which == "selected.candidates.count") {

    warning("Puoluetason tietoa, ei kuntia. TODO.")
    tab <- NULL

    #Valitut ikaryhmittain sukupuolen ja puolueen mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-07-15_tau_110_fi.px"

  } else if (which == "selected.candidates.count") {

    #Valitut ikaryhmittain sukupuolen mukaan vaalipiireittain kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/670_kvaa_2004_2009-11-02_tau_143_fi.px"

    warning("Vaalipiiritason tietoa. TODO.")
    tab <- NULL

    #px <- sorvi::read.px(url, na.strings='"-"')
    #df <- as.data.frame(px)
    #tmp <- reshape::cast(df, Alue~Puolue~Sukupuoli~Valittujen.lukumaara)

  } else if (which == "parties") {

    #Kunnallisvaalit 2004, puolueiden kannatus
    #url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/010_KVAA_2004_2008-08-28_TAU_102_FI.px"
    #df <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    # -> Segmentation fault
    warning("Segmentation fault at Kunnallisvaalit 2004, puolueiden kannatus, ignoring.")
    tab <- NULL

  } else if (which == "parties.per.region") {

    # Puolueiden aanimaarat ja prosenttiosuudet seka aanestysprosentit 
    # vaalipiireittain kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-08-27_tau_114_fi.px"
    warning("Vaalipiiri level, TODO")
    tab <- NULL

  } else if (which == "parties.change") {

    # Puolueiden aanimaarat ja aanestysprosentti seka valittujen lukumaara 
    # kunnittain kunnallisvaaleissa 2004 ja muutos edellisiin vaaleihin 
    # verrattuna
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-08-27_tau_111_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Vaalipiiri.ja.kunta~Puolue~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Aanimaara"]
    tab2 <- tmp[,,"Osuus %"]
    tab3 <- tmp[,,"Muutos edelliseen vaaliin verrattuna"]
    tab4 <- tmp[,,"Valittujen lukumaara"]

    colnames(tab1) <- paste(colnames(tmp[,,"Aanimaara"]), "(Aanimaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus %"]), "(Osuus %)")
    colnames(tab3) <- paste(colnames(tmp[,,"Muutos edelliseen vaaliin verrattuna"]), "(Muutos edelliseen vaaliin verrattuna)")
    colnames(tab4) <- paste(colnames(tmp[,,"Valittujen lukumaara"]), "(Valittujen lukumaara)")

    tab <- cbind(tab1, tab2, tab3, tab4)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2004 puolueiden aanimaarat: ", colnames(tab))

  } else if (which == "party.votes") {

    #Puolueiden aanimaarat ja valittujen lukumaara kunnittain (pienet puolueet), hylatyt liput seka ennakkoaanestaneet kunnallisvaaleissa 2004
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-08-27_tau_114_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Vaalipiiri.ja.kunta~Aanestystiedot.ja.puolueiden.kannatus~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Aanimaara"]
    tab2 <- tmp[,,"Osuus %"]
    tab3 <- tmp[,,"Valittujen lukumaara"]

    colnames(tab1) <- paste(colnames(tmp[,,"Aanimaara"]), "(Aanimaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus %"]), "(Osuus %)")
    colnames(tab3) <- paste(colnames(tmp[,,"Valittujen lukumaara"]), "(Valittujen lukumaara)")

    tab <- cbind(tab1, tab2, tab3)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2004 puolueiden kannatus: ", colnames(tab))

  } else if (which == "voting.stats") {

    # Aanioikeutetut ja aanestaneet sukupuolen mukaan, hyvaksytyt aanestysliput, valtuutetuiksi valitut ja ennakkoaanet puolueittain seka hylattyjen 
    # aanestyslippujen lukumaara kunnittain kunnallisvaaleissa 2004
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-08-28_tau_116_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Vaalipiiri.ja.kunta~Aanestystiedot.ja.puolueiden.kannatus~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Lukumaara / Aanimaara"]
    tab2 <- tmp[,,"Osuus aanista"]
    tab3 <- tmp[,,"Valitut"]
    tab4 <- tmp[,,"Osuus valituista"]
    tab5 <- tmp[,,"Ennakkoaanet"]
    tab6 <- tmp[,,"Ennakkoaanten osuus"]

    colnames(tab1) <- paste(colnames(tmp[,,"Lukumaara / Aanimaara"]), "(Lukumaara / Aanimaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus aanista"]), "(Osuus aanista)")
    colnames(tab3) <- paste(colnames(tmp[,,"Valitut"]), "(Valitut)")
    colnames(tab4) <- paste(colnames(tmp[,,"Osuus valituista"]), "(Osuus valituista)")
    colnames(tab5) <- paste(colnames(tmp[,,"Ennakkoaanet"]), "(Ennakkoaanet)")
    colnames(tab6) <- paste(colnames(tmp[,,"Ennakkoaanten osuus"]), "(Ennakkoaanten osuus)")

    tab <- cbind(tab1, tab2, tab3, tab4, tab5, tab6)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2004 muuta: ", colnames(tab))

  } else if (which == "previous.experience") {

    # Valituiksi tulleiden aikaisempi kokemus valtuustossa kuntatyypin, sukupuolen ja puolueen mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/730_kvaa_2004_2009-12-30_tau_149_fi.px"
    #px <- sorvi::read.px(url, na.strings='"-"')
    #df <- as.data.frame(px)
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "rejected") {

    #Hylatyt aanestysliput hylkaysperusteen ja vaalipiirin mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/740_kvaa_2004_2009-12-30_tau_150_fi.px"
    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "results") {

    #Kunnallisvaalit 2004, tulosanalyysi
    #http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_06/2004_06_fi.asp
    
    warning("No municipality level data available. TODO.")
    # NOTE: vaalipiiri level available
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_06/810_kvaa_2004_2004-10-27_tau_150_fi.px"
    #kvaa <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    tab <- NULL

  } else if (which == "pre") {

    #Ennakkoon aanestaneet aanestyspaikan ja vaalipiirin mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/750_kvaa_2004_2009-12-30_tau_151_fi.px"
    #kvaa <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "abroad") {

    # Suomen ulkomaan edustustoissa ja laivoissa aanestaneet sukupuolen mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/760_kvaa_2004_2009-12-30_tau_152_fi.px"
    #kvaa <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "abroad2") {

    #Aanioikeutetut ja aanestaneet ulkomaalaiset vaalipiirin mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/770_kvaa_2004_2009-12-30_tau_153_fi.px"
    kvaa <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "all.municipal") {

    tab1 <- sorvi::GetMunicipalElectionData2004("voting.stats")
    tab2 <- sorvi::GetMunicipalElectionData2004("party.votes")
    tab3 <- sorvi::GetMunicipalElectionData2004("parties.change")
    tab4 <- sorvi::GetMunicipalElectionData2004("selected.candidates.count")
    tab6 <- sorvi::GetMunicipalElectionData2004("election.statistics")

    regs <- rownames(tab1)

    tab <- cbind(tab1[regs,], tab2[regs,], tab3[regs,],
    	         tab4[regs,], tab6[regs,])

  } 
  tab

}


#' GetElectedCandidates
#'
#' Get data on elected candidates 
#' 
#' @param year election year
#' @param election election type (municipal / parliament / president / ...)
#' @param election.district election.district in numeric or character format (for instance: 2 or "Uudenmaan vaalipiiri")
#' @param verbose verbose
#' @return data.frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities
GetElectedCandidates <- function (year, election, election.district, verbose = TRUE) {

  .InstallMarginal("plyr")
  .InstallMarginal("reshape2")
  .InstallMarginal("reshape")

  if (verbose) {message(paste(election.district))}		     

  # Convert IDs to names if needed
  convtab <- .datavaalit.idconversions(type = "election.district.id") 
  if (as.character(election.district) %in% convtab$id) {
    election.district.id <- election.district
    election.district.name <- .datavaalit.idconversions(election.district, type = "election.district.id")
  } else{
    election.district.name <- election.district
    election.district.id <- .datavaalit.idconversions(election.district, type = "election.district.id")
  }

  if (as.numeric(year) == 2012 && election == "municipal") {

    message(paste(election, year))

    # List URLs for Statfi election candidate tables 2012
    # Source (C) Tilastokeskus:
    # http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/2012_04_fi.asp
    urls <- c()
    urls[["Helsingin vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/410_kvaa_2012_2013-01-07_tau_123_fi.px"               
    urls[["Uudenmaan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/420_kvaa_2012_2013-01-07_tau_124_fi.px"
    urls[["Varsinais-Suomen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/430_kvaa_2012_2013-01-07_tau_125_fi.px"
    urls[["Satakunnan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/440_kvaa_2012_2013-01-07_tau_126_fi.px"
    urls[["Hameen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/460_kvaa_2012_2013-01-07_tau_127_fi.px"
    urls[["Pirkanmaan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/470_kvaa_2012_2013-01-07_tau_128_fi.px"
    urls[["Kymen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/480_kvaa_2012_2013-01-07_tau_129_fi.px"
    urls[["Etela-Savon vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/490_kvaa_2012_2013-01-07_tau_130_fi.px"
    urls[["Pohjois-Savon vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/500_kvaa_2012_2013-01-07_tau_131_fi.px"
    urls[["Pohjois-Karjalan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/510_kvaa_2012_2013-01-07_tau_132_fi.px"
    urls[["Vaasan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/520_kvaa_2012_2013-01-07_tau_133_fi.px"
    urls[["Keski-Suomen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/530_kvaa_2012_2013-01-07_tau_134_fi.px"
    urls[["Oulun vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/540_kvaa_2012_2013-01-07_tau_135_fi.px"
    urls[["Lapin vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/550_kvaa_2012_2013-01-07_tau_136_fi.px"

    url <- urls[[election.district.name]]

  } else if (as.numeric(year) == 2008 && election == "municipal") {

    # List URLs for Statfi election candidate tables 2008
    # Source (C) Tilastokeskus:
    # http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/2008_04_fi.asp
    urls <- list()
    urls[["Helsingin vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/410_kvaa_2008_2009-11-02_tau_123_fi.px"
    urls[["Uudenmaan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/420_kvaa_2008_2009-11-02_tau_124_fi.px"
    urls[["Varsinais-Suomen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/430_kvaa_2008_2009-11-02_tau_125_fi.px"
    urls[["Satakunnan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/440_kvaa_2008_2009-11-02_tau_126_fi.px"
    urls[["Hameen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/460_kvaa_2008_2009-11-02_tau_127_fi.px"
    urls[["Pirkanmaan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/470_kvaa_2008_2009-11-02_tau_128_fi.px"
    urls[["Kymen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/480_kvaa_2008_2009-11-02_tau_129_fi.px"
    urls[["Etela-Savon vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/490_kvaa_2008_2009-11-02_tau_130_fi.px"
    urls[["Pohjois-Savon vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/500_kvaa_2008_2009-11-02_tau_131_fi.px"
    urls[["Pohjois-Karjalan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/510_kvaa_2008_2009-11-02_tau_132_fi.px"
    urls[["Vaasan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/520_kvaa_2008_2009-11-02_tau_133_fi.px"
    urls[["Keski-Suomen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/530_kvaa_2008_2009-11-02_tau_134_fi.px"
    urls[["Oulun vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/540_kvaa_2008_2009-11-02_tau_135_fi.px"
    urls[["Lapin vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/550_kvaa_2008_2009-11-02_tau_136_fi.px"

    url <- urls[[election.district.name]]

  } else if (as.numeric(year) == 2004 && election == "municipal") {
    # List URLs for Statfi election candidate tables 2004
    # Source (C) Tilastokeskus:
    # http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/2004_04_fi.asp

    urls <- list()
    urls[["Helsingin vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_101_FI.px"
    urls[["Uudenmaan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_102_FI.px"
    urls[["Varsinais-Suomen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_103_FI.px"
    urls[["Satakunnan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_104_FI.px"
    urls[["Hameen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_106_FI.px"
    urls[["Pirkanmaan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_107_FI.px"
    urls[["Kymen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_108_FI.px"
    urls[["Etela-Savon vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_109_FI.px"
    urls[["Pohjois-Savon vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_110_FI.px"
    urls[["Pohjois-Karjalan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_111_FI.px"
    urls[["Vaasan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_112_FI.px"
    urls[["Keski-Suomen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_113_FI.px"
    urls[["Oulun vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_114_FI.px"
    urls[["Lapin vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_115_FI.px"

    url <- urls[[election.district.name]]

  } else {
    warning(paste("Option", election, year, "not implemented"))
  }

  if (verbose) { message("Reading PC Axis file") }
  px <- read.px(url)

  if (verbose) { message("Converting to data frame") }
  df <- as.data.frame(px)

  if (verbose) { message("Splitting by candidate") }
  df <- split(df, df$Ehdokas)

  if (verbose) { message("Converting into more compact table format") }
  
  kaava <- as.formula("Ehdokas + Äänestysalue ~ Äänestystiedot")
  df <- lapply(df, function(dff) {m <- reshape2::melt(dff, c("Ehdokas", "Äänestysalue", "Äänestystiedot"), "dat"); mc <- reshape::cast(m, kaava); mc <- mc[!mc[["Ehdokkaan numero"]] == 0, ]})
  
  # df <- lapply(df, function(dff) {names(dff) <- c("Äänestystiedot", "Äänestysalue", "Ehdokas", "dat"); m <- reshape2::melt(dff, c("Ehdokas", "Äänestysalue", "Äänestystiedot"), "dat"); mc <- reshape::cast(m, Ehdokas + Äänestysalue ~ Äänestystiedot); mc <- mc[!mc[["Ehdokkaan numero"]] == 0, ]})

  df <- do.call(rbind, df)

  if (verbose) { message("Preprocessing fields") }
  df$Ehdokas <- gsub(" / ", "/", as.character(df$Ehdokas))
  ehd <- do.call(rbind, strsplit(df$Ehdokas, "/"))
  df[["Ehdokkaan nimi"]] <- ehd[, 1]
  df[["Puolue_lyhenne_fi"]] <- ehd[, 2]
  rm(ehd)
  df$Sukunimi <- sapply(strsplit(df[["Ehdokkaan nimi"]], " "), function (x) {x[[1]]})
  df$Etunimi <- sapply(strsplit(df[["Ehdokkaan nimi"]], " "), function (x) {paste(x[-1], collapse = " ")})
  df[["Ehdokkaan nimi"]] <- NULL

  if (verbose) { message("Preprocessing region fields") }
  df[["Äänestysalue"]] <- gsub(" / ", "/", as.character(df[["Äänestysalue"]]))
  alue <- do.call(rbind, strsplit(df[["Äänestysalue"]], "/"))
  df$Kunta <- alue[, 1]
  df$Alue <- alue[, 2]
  rownames(df) <- NULL

  # Add fields for compatibility
  df$Vaalipiirinumero <- election.district.id
  df$Vaalipiiri_fi <- election.district.name
  df$Vaalilaji <- "K"
  df[["Ehdokasnumero"]] <- df[["Ehdokkaan numero"]]
  df[["Ehdokkaan numero"]] <- NULL
  
  df$Vaalilaji_nimi_fi <- .datavaalit.idconversions(tolower(df$Vaalilaji), type = "election.id") 

  # Clean up memory
  gc()

  df

}

  
#' GetMunicipalElectionData2008
#'
#' Get municipal election data 
#' 
#' @param which Indicate which of the available Statistics Finland data sets to parse. Options: 
#' @return data.frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities
GetMunicipalElectionData2008 <- function (which = "election.statistics") {

  .InstallMarginal("plyr")
  .InstallMarginal("reshape")
  .InstallMarginal("reshape2")

  # Taulukot tilastossa: 5. Kunnallisvaalit 2008 - vaalitulos, aanestaminen
  # http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/2008_05_fi.asp

  if (which == "election.statistics") {

    #Kunnallisvaalit 2008, aanestystiedot
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/610_kvaa_2008_2009-10-30_tau_137_fi.px"
    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Alue~Aanestystiedot~Sukupuoli")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Sukupuolet yhteensa"]
    tab2 <- tmp[,,"Miehet"]
    tab3 <- tmp[,,"Naiset"]

    colnames(tab1) <- paste(colnames(tmp[,,"Sukupuolet yhteensa"]), "(Sukupuolet yhteensa)")
    colnames(tab2) <- paste(colnames(tmp[,,"Miehet"]), "(Miehet)")
    colnames(tab3) <- paste(colnames(tmp[,,"Naiset"]), "(Naiset)")

    tab <- cbind(tab1, tab2, tab3)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    rnams <- rnams[-grep(" 00", rnams)]
    rnams <- rnams[-grep(" 01", rnams)]
    rnams <- rnams[-grep(" 02", rnams)]
    rnams <- rnams[-grep(" 03", rnams)]
    rnams <- rnams[-grep(" 04", rnams)]
    rnams <- rnams[-grep(" 05", rnams)]
    rnams <- rnams[-grep(" 06", rnams)]
    rnams <- rnams[-grep(" 07", rnams)]
    rnams <- rnams[-grep(" 08", rnams)]
    rnams <- rnams[-grep(" 09", rnams)]
    rnams <- rnams[-grep(" 1", rnams)]
    rnams <- rnams[-grep(" 2", rnams)]
    rnams <- rnams[-grep(" 3", rnams)]
    rnams <- rnams[-grep(" 4", rnams)]
    rnams <- rnams[-grep(" 5", rnams)]
    rnams <- rnams[-grep("Manner-Suomi", rnams)]
    #rnams <- rnams[-grep(" 6", rnams)]
    #rnams <- rnams[-grep(" 7", rnams)]
    rnams <- rnams[-grep(" 8", rnams)]
    #rnams <- rnams[-grep(" 9", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: coarse election region (vaalipiiri) information also available but discarded
    # NOTE: detailed election region information also available (below municipality level) but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 aanestystiedot", colnames(tab))

  } else if (which == "woman.candidates") {

    #Naisehdokkaitten vaalitiedot puolueen ja kunnan mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/720_kvaa_2008_2009-12-30_tau_148_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    tmp <- reshape::cast(df, Kunta~Puolue~Naisehdokastiedot)
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Aanimaara"]
    tab2 <- tmp[,,"Osuus aanista (%)"]
    tab3 <- tmp[,,"Ehdokkaat"]
    tab4 <- tmp[,,"Osuus ehdokkaista (%)"]
    tab5 <- tmp[,,"Valitut"]
    tab6 <- tmp[,,"Osuus valituista (%)"]

    colnames(tab1) <- paste(colnames(tmp[,,"Aanimaara"]), "(Aanimaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus aanista (%)"]), "(Osuus aanista (%))")
    colnames(tab3) <- paste(colnames(tmp[,,"Ehdokkaat"]), "(Ehdokkaat)")
    colnames(tab4) <- paste(colnames(tmp[,,"Osuus ehdokkaista (%)"]), "(Osuus ehdokkaista (%))")
    colnames(tab5) <- paste(colnames(tmp[,,"Valitut"]), "(Valitut)")
    colnames(tab6) <- paste(colnames(tmp[,,"Osuus valituista (%)"]), "(Osuus valituista (%))")

    regs <- rownames(tab1)

    tab <- cbind(tab1[regs,], tab2[regs,], tab3[regs,], tab4[regs,], tab5[regs,], tab6[regs,])

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 naisehdokkaat", colnames(tab))

  } else if (which == "selected.candidates.by.election.region") {

    warning("Vaalipiiri level information; TODO")

    #Valittujen lukumaara ja prosenttiosuudet puolueittain ja vaalipiireittain kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/690_kvaa_2008_2009-11-02_tau_145_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    kaava <- as.formula("Puolue~Vaalipiiri~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")

    tab1 <- tmp[,,"Valtuutettujen lukumaara"]
    tab2 <- tmp[,,"Puolueen osuus"]

    colnames(tab1) <- paste(colnames(tmp[,,"Valtuutettujen lukumaara"]), "(Valtuutettujen lukumaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Puolueen osuus"]), "(Puolueen osuus)")

    tab <- cbind(tab1, tab2)
    
    tab <- NULL

  } else if (which == "selected.candidates.count") {

    # Kunnallisvaalit 2008, valittujen lukumaara
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/630_kvaa_2008_2009-10-30_tau_139_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))    

    kaava <- as.formula("Alue~Puolue~Sukupuoli~Valittujen.lukumaara")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Kaikki ehdokkaat", "Valittujen lukumaara"]
    colnames(tab1) <- paste("Kaikki ehdokkaat", "Valittujen lukumaara", colnames(tab1))

    tab2 <- tmp[,,"Miesehdokkaat", "Valittujen lukumaara"]
    colnames(tab2) <- paste("Miesehdokkaat", "Valittujen lukumaara", colnames(tab2))

    tab3 <- tmp[,,"Naisehdokkaat", "Valittujen lukumaara"]
    colnames(tab3) <- paste("Naisehdokkaat", "Valittujen lukumaara", colnames(tab3))

    tab4 <- tmp[,,"Kaikki ehdokkaat", "Osuus valituista %"]
    colnames(tab4) <- paste("Kaikki ehdokkaat", "Osuus valituista %", colnames(tab4))

    tab5 <- tmp[,,"Miesehdokkaat", "Osuus valituista %"]
    colnames(tab5) <- paste("Miesehdokkaat", "Osuus valituista %", colnames(tab5))

    tab6 <- tmp[,,"Naisehdokkaat", "Osuus valituista %"]
    colnames(tab6) <- paste("Naisehdokkaat", "Osuus valituista %", colnames(tab6))

    regs <- rownames(tab1)

    tab <- cbind(tab1[regs,], tab2[regs,], tab3[regs,], 
    	         tab4[regs,], tab5[regs,], tab6[regs,])

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    rnams <- rnams[-grep("Manner-Suomi", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    
    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 valittujen lukumaara", colnames(tab))
    
  } else if (which == "selected.candidates.by.party") {  

    # Valittujen lukumaara ja prosenttiosuudet puolueittain ja 
    # vaalipiireittain kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/650_kvaa_2008_2009-11-02_tau_141_fi.px"

    warning("Vaalipiiritason tietoa. TODO.")
    tab <- NULL

    #px <- sorvi::read.px(url, na.strings='"-"')
    #df <- as.data.frame(px)
    #tmp <- reshape::cast(df, Alue~Puolue~Sukupuoli~Valittujen.lukumaara)

  } else if (which == "selected.candidates.count") {

    warning("Puoluetason tietoa, ei kuntia. TODO.")
    tab <- NULL

    # Valitut ikaryhmittain sukupuolen ja puolueen mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/660_kvaa_2008_2009-11-02_tau_142_fi.px"

  } else if (which == "selected.candidates.count") {

    #Valitut ikaryhmittain sukupuolen mukaan vaalipiireittain kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/670_kvaa_2008_2009-11-02_tau_143_fi.px"

    warning("Vaalipiiritason tietoa. TODO.")
    tab <- NULL

    #px <- sorvi::read.px(url, na.strings='"-"')
    #df <- as.data.frame(px)
    #tmp <- reshape::cast(df, Alue~Puolue~Sukupuoli~Valittujen.lukumaara)

  } else if (which == "parties") {

    #Kunnallisvaalit 2008, puolueiden kannatus
    #url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/620_kvaa_2008_2009-10-30_tau_138_fi.px"
    #df <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    # -> Segmentation fault
    warning("Segmentation fault at Kunnallisvaalit 2008, puolueiden kannatus, ignoring.")
    tab <- NULL

  } else if (which == "parties.per.region") {

    # Puolueiden aanimaarat ja prosenttiosuudet seka aanestysprosentit vaalipiireittain kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/640_kvaa_2008_2009-11-02_tau_140_fi.px"

    warning("Vaalipiiri level, TODO")
    tab <- NULL

  } else if (which == "parties.change") {

    # Puolueiden aanimaarat ja aanestysprosentti seka valittujen lukumaara
    # kunnittain kunnallisvaaleissa 2008 ja muutos edellisiin vaaleihin 
    # verrattuna
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/680_kvaa_2008_2009-11-02_tau_144_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Vaalipiiri.ja.kunta~Puolue~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Aanimaara"]
    tab2 <- tmp[,,"Osuus %"]
    tab3 <- tmp[,,"Muutos edelliseen vaaliin verrattuna"]
    tab4 <- tmp[,,"Valittujen lukumaara"]

    colnames(tab1) <- paste(colnames(tmp[,,"Aanimaara"]), "(Aanimaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus %"]), "(Osuus %)")
    colnames(tab3) <- paste(colnames(tmp[,,"Muutos edelliseen vaaliin verrattuna"]), "(Muutos edelliseen vaaliin verrattuna)")
    colnames(tab4) <- paste(colnames(tmp[,,"Valittujen lukumaara"]), "(Valittujen lukumaara)")

    tab <- cbind(tab1, tab2, tab3, tab4)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 puolueiden aanimaarat: ", colnames(tab))

  } else if (which == "party.votes") {

    #Puolueiden aanimaarat ja valittujen lukumaara kunnittain (pienet puolueet), hylatyt liput seka ennakkoaanestaneet kunnallisvaaleissa 2008
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/700_kvaa_2008_2009-11-02_tau_146_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Vaalipiiri.ja.kunta~Aanestystiedot.ja.puolueiden.kannatus~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Aanimaara"]
    tab2 <- tmp[,,"Osuus %"]
    tab3 <- tmp[,,"Valittujen lukumaara"]

    colnames(tab1) <- paste(colnames(tmp[,,"Aanimaara"]), "(Aanimaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus %"]), "(Osuus %)")
    colnames(tab3) <- paste(colnames(tmp[,,"Valittujen lukumaara"]), "(Valittujen lukumaara)")

    tab <- cbind(tab1, tab2, tab3)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 puolueiden kannatus: ", colnames(tab))

  } else if (which == "voting.stats") {

    # Aanioikeutetut ja aanestaneet sukupuolen mukaan, hyvaksytyt aanestysliput, valtuutetuiksi valitut ja ennakkoaanet puolueittain seka 
    # hylattyjen aanestyslippujen lukumaara kunnittain kunnallisvaaleissa 2008
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/710_kvaa_2008_2009-11-02_tau_147_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')

    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))    

    kaava <- as.formula("Vaalipiiri.ja.kunta~Aanestystiedot.ja.puolueiden.kannatus~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Lukumaara / Aanimaara"]
    tab2 <- tmp[,,"Osuus aanista"]
    tab3 <- tmp[,,"Valitut"]
    tab4 <- tmp[,,"Osuus valituista"]
    tab5 <- tmp[,,"Ennakkoaanet"]
    tab6 <- tmp[,,"Ennakkoaanten osuus"]

    colnames(tab1) <- paste(colnames(tmp[,,"Lukumaara / Aanimaara"]), "(Lukumaara / Aanimaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus aanista"]), "(Osuus aanista)")
    colnames(tab3) <- paste(colnames(tmp[,,"Valitut"]), "(Valitut)")
    colnames(tab4) <- paste(colnames(tmp[,,"Osuus valituista"]), "(Osuus valituista)")
    colnames(tab5) <- paste(colnames(tmp[,,"Ennakkoaanet"]), "(Ennakkoaanet)")
    colnames(tab6) <- paste(colnames(tmp[,,"Ennakkoaanten osuus"]), "(Ennakkoaanten osuus)")

    tab <- cbind(tab1, tab2, tab3, tab4, tab5, tab6)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 muuta: ", colnames(tab))

  } else if (which == "previous.experience") {

    # Valituiksi tulleiden aikaisempi kokemus valtuustossa kuntatyypin, sukupuolen ja puolueen mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/730_kvaa_2008_2009-12-30_tau_149_fi.px"
    #px <- sorvi::read.px(url, na.strings='"-"')
    #df <- as.data.frame(px)
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "rejected") {

    # Hylatyt aanestysliput hylkaysperusteen ja vaalipiirin mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/740_kvaa_2008_2009-12-30_tau_150_fi.px"
    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "results") {

    #Kunnallisvaalit 2008, tulosanalyysi
    #http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_06/2008_06_fi.asp
    
    warning("No municipality level data available. TODO.")
    # NOTE: vaalipiiri level available
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_06/810_kvaa_2008_2008-10-27_tau_150_fi.px"
    #kvaa <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    tab <- NULL

  } else if (which == "pre") {

    #Ennakkoon aanestaneet aanestyspaikan ja vaalipiirin mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/750_kvaa_2008_2009-12-30_tau_151_fi.px"
    #kvaa <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "abroad") {

    #Suomen ulkomaan edustustoissa ja laivoissa aanestaneet sukupuolen mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/760_kvaa_2008_2009-12-30_tau_152_fi.px"
    #kvaa <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "abroad2") {

    #Aanioikeutetut ja aanestaneet ulkomaalaiset vaalipiirin mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/770_kvaa_2008_2009-12-30_tau_153_fi.px"
    kvaa <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "all.municipal") {

    tab1 <- GetMunicipalElectionData2008("voting.stats")
    tab2 <- GetMunicipalElectionData2008("party.votes")
    tab3 <- GetMunicipalElectionData2008("parties.change")
    tab4 <- GetMunicipalElectionData2008("selected.candidates.count")
    # tab5 <- GetMunicipalElectionData2008("woman.candidates") # Error
    tab6 <- GetMunicipalElectionData2008("election.statistics")

    regs <- rownames(tab1)

    tab <- cbind(tab1[regs,], tab2[regs,], tab3[regs,],
    	         tab4[regs,], tab6[regs,])

  }

  tab

}



#' Load data sets from datavaalit.fi web service
#'
#' @param data.id Data set ID
#'
#' @return rjson object
#' 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @export

ReadDatavaalit <- function (data.id) {

  # Read election info
  if (data.id ==  "election.data") { 

    f <- "http://beta.datavaalit.fi/api/v1/election/?format=json&limit=500"
    dat <- fromJSON(paste(readLines(f), collapse = ""))
    
  } else if (data.id == "municipality.data") {

    f <- "http://beta.datavaalit.fi/api/v1/municipality/?format=json&limit=500"
    dat <- fromJSON(paste(readLines(f), collapse = ""))

  } else if (data.id == "hel.council.members") {
    f <- "http://beta.datavaalit.fi/api/v1/council_member/?format=json&limit=85"
    # FIXME: Extremely bad idea to have the function to return different value
    # types depending on the data.id
    dat <- new("council", f)
  }
  dat  
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

  ReadElectionData("candidates", district.id, cache)
  
}

#' Description:
#' Wrapper function for ReadCandidates that gets all 14 districts and returns
#' all data in a single data frame.
#'
#' @param cache character directory path to location where files are cached
#'
#' @return Data frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Joona Lehtomaki \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

ReadAllCandidates <- function(cache=NA) {
  
  election.district.ids  <- 1:15
  # Remember, there is no id 5!
  election.district.ids  <- election.district.ids[-c(5)]

  # Determine the cache dir if needed
  # cache = "."  
  all.districts <- lapply(election.district.ids, 
                          function(x) {ReadCandidates(x, cache)})
  
  # Bind everything into a single data frame
  candidates <- do.call("rbind", all.districts)

  candidates$RowIndex <- 1:nrow(candidates)
  
  return(candidates)
}

#' Description:
#' Wrapper function for ReadParties that gets all 14 districts and returns
#' all data in a single data frame.
#'
#' @param cache character directory path to location where files are cached
#'
#' @return Data frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

ReadAllParties <- function(cache=NA) {
 
  # District 5 does not exist!
  election.district.ids  <- setdiff(1:15, 5)

  # Determine the cache dir if needed
  # cache = "."  
  all.districts <- lapply(election.district.ids, 
                          function(x) {ReadParties(x, cache)})
  
  # Bind everything into a single data frame
  parties <- do.call("rbind", all.districts)

  parties$RowIndex <- 1:nrow(parties)
  
  return(parties)
}


# Private functions -------------------------------------------------------

.readCommonData <- function() {

  .InstallMarginal("rjson")

  data.file <- system.file("extdata/common_data.json", package = "sorvi")
  return(fromJSON(paste(readLines(data.file), collapse = "")))
} 

.datavaalit.idconversions <- function (ids = NULL, type = "election.id") {

  if (type == "election.id") {

    conversion.table <- rbind(c("pv", "presidentin vaali"),
	                      c("e", "eduskuntavaalit"),
                  		  c("k", "kunnallisvaalit"),
                  		  c("epv", "europarlamenttivaalit"),
                  		  c("mkv", "aluevaali"),
                  		  c("vka", "kansanäänestys"))
    colnames(conversion.table) <- c("id", "name")
    conversion.table <- as.data.frame(conversion.table)
  } else if (type == "stage.id") {

    conversion.table <- rbind(c("a", "alustava"),
	                   c("t", "tarkastus"))
    colnames(conversion.table) <- c("id", "name")
    conversion.table <- as.data.frame(conversion.table)
  } else if (type == "data.id") {

    conversion.table <- rbind(c("a", "alue"),
             	      c("e", "ehdokas"),
             	      c("p", "puolue"),
             	      c("k", "kansanäänestys"))
    colnames(conversion.table) <- c("id", "name")
    conversion.table <- as.data.frame(conversion.table)	      
  } else if (type == "info.id") {

    conversion.table <- rbind(c("a", "äänestysaluetaso"),
        	          c("t", "tilastotiedot"),
        	          c("y", "ei.äänestysaluetasoa"),
        	          c("", ""))
    colnames(conversion.table) <- c("id", "name")
    conversion.table <- as.data.frame(conversion.table)	      
  } else if (type == "election.district.id") {

    conversion.table <- rbind(c(1, "Helsingin vaalipiiri"),
    		     c("2", "Uudenmaan vaalipiiri"),
		     c("3", "Varsinais-Suomen vaalipiiri"),
		     c("4", "Satakunnan vaalipiiri"),
		     # 5 is intentionally missing here
		     c("6", "Hämeen vaalipiiri"),
		     c("7", "Pirkanmaan vaalipiiri"),
		     c("8", "Kymen vaalipiiri"),
		     c("9", "Etelä-Savon vaalipiiri"),
		     c("10", "Pohjois-Savon vaalipiiri"),
		     c("11", "Pohjois-Karjalan vaalipiiri"),
		     c("12", "Vaasan vaalipiiri"),
		     c("13", "Keski-Suomen vaalipiiri"),
		     c("14", "Oulun vaalipiiri"),
		     c("15", "Lapin vaalipiiri"),
		     c("16", "Koko maa"),
		     c("maa", "Koko maa"))

    colnames(conversion.table) <- c("id", "name")
    conversion.table <- as.data.frame(conversion.table)	      

  }

  if (is.null(ids)) {
    return(conversion.table)
  }

  ids <- as.character(ids)

  if (any(ids %in% conversion.table$id)) {
    as.character(conversion.table$name[match(ids, conversion.table$id)])
  } else if (any(ids %in% conversion.table$name)) {
    as.character(conversion.table$id[match(ids, conversion.table$name)])
  }

}

# ---------------------------------------------------------------


#' Description:
#' Function for reading in Finnish Municipal Election political party data 
#  published by Ministry of justice. As of 27-09-2012, the data and 
#  descriptions are
#' available from http://192.49.229.35/K2012/s/ehd_listat/kokomaa.htm#ladattavat
#  
#' Party data comes divided into 14 Election districts (vaalipiiri).
#'
#' @param district.id integer marking the election district ID. Options: [1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
#' @param cache character directory path to location where files are cached
#'
#' @return Data frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

ReadParties <- function(district.id, cache=NA) {
  
  ReadElectionData("parties", district.id, cache)

}


#' Description:
#' Function for reading in Finnish Municipal Election data 
#  published by Ministry of justice. As of 27-09-2012, the data and 
#  descriptions are
#' available from http://192.49.229.35/K2012/s/ehd_listat/kokomaa.htm#ladattavat
#  
#' Data comes divided into 14 Election districts (vaalipiiri).
#' @param which.data Options: "candidates", "parties"
#' @param district.id integer marking the election district ID. Options: [1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
#' @param cache character directory path to location where files are cached
#'
#' @return Data frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

ReadElectionData <- function(which.data, district.id, cache=NA) {
 
  #  which.data <- "parties";  district.id <- 1;  cache <- NA

  # Body of the filename is always the same
  if (which.data == "parties") { 
    file.name.body <- "puo_"
  } else if (which.data == "candidates") { 
    file.name.body <- "ehd_"
  }

  # Convert plain names into numerical IDs if needed
  convtab <- .datavaalit.idconversions(type = "election.district.id")
  if (district.id %in% convtab$name) {
    district.id <- .datavaalit.idconversions(district.id, type = "election.district.id")
  }

  # Coerce the district id into a character for building file paths / urls
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
  raw.data <- read.csv(data.source, sep=";", as.is=TRUE, strip.white=TRUE, fileEncoding="iso-8859-1")
  
  # The the suitable header names from common_data.json
  header <- .readCommonData()
  
  # In the original csv file, there is also a trailing ";" -> there really is
  # only 29 / 35 columns (as of 27.9.2012); more columns will appear 
  # on the election day
  if (which.data == "parties") {
    raw.data <- raw.data[1:35]
    header <- header$OMpuolueet$header
  } else if (which.data == "candidates") {
    raw.data <- raw.data[1:29]
    header <- header$OMehdokkaat$header
  }

  # Set the header
  colnames(raw.data) <- header[1:length(raw.data)]
  
  # Column pre-processing

  dat <- .preprocessElectionData(raw.data, which.data)

  dat <- cbind(as.character(1:nrow(dat)), dat)  
  colnames(dat) <- c("RowIndex", colnames(dat)[-1])
  rownames(dat) <- NULL

  return(dat)
  
}




#' Description:
#' Internal function for election data preprocessing
#'  
#' @param dat election data frame
#' @param which.data "elections" or "candidates"
#' @return Data frame
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

.preprocessElectionData <- function (dat, which.data) {

  dat <- as.data.frame(dat)

  # Get conversions between municipality IDs and names from MML data
  # (C) MML 2011-2012
  # FIXME: replace with GetVaalipiiri data and Kuntanumero from another server.

  dat$Vaalilaji_nimi_fi <- .datavaalit.idconversions(tolower(dat$Vaalilaji), type = "election.id") 
  dat$Vaalipiiri_fi <- .datavaalit.idconversions(as.character(dat$Vaalipiirinumero), type = "election.district.id") 

  dat$Kuntanumero[nchar(dat$Kuntanumero) == 1] <- paste("00", dat$Kuntanumero[nchar(dat$Kuntanumero) == 1], sep = "")
  dat$Kuntanumero[nchar(dat$Kuntanumero) == 2] <- paste("0", dat$Kuntanumero[nchar(dat$Kuntanumero) == 2], sep = "")
  dat$Kunta <- ConvertMunicipalityCodes(ids = dat$Kuntanumero)
  dat$Kommun <- as.character(dat$Alueen_nimi_sv)

  dat$Puolue_lyhenne_fi <- dat$Nimilyhenne_fi
  dat$Puolue_lyhenne_sv <- dat$Nimilyhenne_sv
  
  dat$Nimilyhenne_fi <- NULL
  dat$Nimilyhenne_sv <- NULL

  if (which.data == "candidates") {
    
    dat$Sukupuoli <- factor(dat$Sukupuoli, labels=c("Mies", "Nainen"))
    dat$Ehdokas <- paste(dat$Sukunimi, " ", dat$Etunimi, " / ",  dat$Puolue_lyhenne_fi, " / ", dat$Kunta, sep = "")

  } 

  dat

}



