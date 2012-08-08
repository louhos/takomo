# Code for analyzing edustajien-puheet data
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
# Copyright 2012 Juuso Parkkinen, juuso.parkkinen@gmail.com. All rights reserved.

###############
## READ DATA ##
###############

# Data given as zip
# More information: http://blogit.hs.fi/hsnext/avodataa-kansanedustajien-puheet-muutettuna-perusmuotoon
temp <- tempfile()
download.file("http://www2.hs.fi/extrat/hsnext/edustajien-puheet.zip", temp)
raw.dat <- read.csv(unz(temp, "edustajien-puheet.csv"), sep="|", header=FALSE)
unlink(temp)

# Read data (header missing in the csv)
# raw.dat <- read.csv("edustajien-puheet.csv", sep="|", header=FALSE)
# Add column names manually (based on the xls-file)
names(raw.dat) <- c("Tunniste", "Päivämäärä", "Puhuja", "Puolue", "Vaalipiiri", "Käsiteltävä_asia", "Puhe_perusmuodossa", "Puhe_alkuperäisenä")
# Remove the original speech data to save space
speech.dat <- raw.dat[,-8]
# Save
save(speech.dat, file="Raw_speech_20120204.RData")


#####################
## PREPROCESS DATA ##
#####################

load("Raw_speech_20120204.RData")

# Get representatives
representatives <- sort(levels(speech.dat$Puhuja))
representatives <- representatives[-which(representatives=="")]

# Add party info
rep.party <- sort(unique(paste(speech.dat$Puhuja, " (", speech.dat$Puolue, ")", sep="")))
rep.party <- rep.party[-which(rep.party==" ()")]
save(rep.party, file="Rep-party_20120731.RData")

# Get list of individual words for each representative
wordlist.raw <- list()
for (ri in 1:length(representatives)) {
  rep.speech <- as.vector(speech.dat$Puhe_perusmuodossa[speech.dat$Puhuja==representatives[ri]])
  wlist <- unlist(sapply(rep.speech, function(x) unlist(strsplit(x, split=" "))))
  names(wlist) <- NULL
  wordlist.raw[[ri]] <- wlist
}
names(wordlist.raw) <- representatives

# Get all words, sorted
word.table.raw <- table(unlist(wordlist.raw))

# Get a representatives to words -matrix
rep.word.mat.raw <- matrix(0, nrow=length(representatives), ncol=length(word.table.raw),
                           dimnames=list(representatives, names(word.table.raw)))
for (ri in 1:length(representatives)) {
  rep.table <- table(wordlist.raw[[ri]])
  rep.word.mat.raw[ri, names(rep.table)] <- rep.table
}
# sum(rep.word.mat.raw) # 1543827
save(rep.word.mat.raw, file="Representatives_vs_words_matrix_raw_20120731.RData")


##################
## FILTER WORDS ##
##################

rep.word.mat.preprocessed <- rep.word.mat.raw
# First 548 in the list are numbers (these include some sensible years, though...)
rep.word.mat.preprocessed <- rep.word.mat.preprocessed[,-(1:548)]
# Remove stopwords
finnish.stopwords <- as.vector(as.matrix(read.csv("finnish_stop_edit_20120305.txt", skip=1, header=F)))
rep.word.mat.preprocessed <- rep.word.mat.preprocessed[,-which(colnames(rep.word.mat.preprocessed) %in% finnish.stopwords)]
# sum(rep.word.mat.preprocessed) # 1081889
save(rep.word.mat.preprocessed, file="Representatives_vs_words_matrix_preprocessed_20120731.RData")


#############
## ANALYZE ##
#############

# Compute td-idf
tf.idf.mat.raw <- prop.table(rep.word.mat.raw, margin=2)
tf.idf.max <- apply(tf.idf.mat.raw, 2, max)

