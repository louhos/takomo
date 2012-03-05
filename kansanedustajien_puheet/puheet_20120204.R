# Code for analyzing edustajien-puheet data
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
# Copyright 2012 Juuso Parkkinen, juuso.parkkinen@gmail.com. All rights reserved.

###############
## READ DATA ##
###############

# Read data (header missing in the csv)
raw.dat <- read.csv("edustajien-puheet.csv", sep="|", header=FALSE)
# Add column names manually (based on the xls-file)
names(raw.dat) <- c("Tunniste", "Päivämäärä", "Puhuja", "Puolue", "Vaalipiiri", 
                    "Käsiteltävä_asia", "Puhe_perusmuodossa", "Puhe_alkuperäisenä")
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

# Get a representative to words -matrix
rep.word.mat.raw <- matrix(0, nrow=length(representatives), ncol=length(word.table.raw),
                       dimnames=list(representatives, names(word.table.raw)))
for (ri in 1:length(representatives)) {
  rep.table <- table(wordlist.raw[[ri]])
  rep.word.mat.raw[ri, names(rep.table)] <- rep.table
}

save(rep.word.mat.raw, file="Representatives_vs_words_matrix_raw_20120304.RData")

# Compute td-idf
tf.idf.mat.raw <- prop.table(rep.word.mat.raw, margin=2)

tf.idf.max <- apply(tf.idf.mat.raw, 2, max)

# Preprocess words
# First 552 in the list are numbers (these include some sensible years, though...)
# word.table <- word.table.raw[-(1:552)]

## TRY TF-IDF?

# OLD
#wordlists2 <- sapply(representatives, function(x) unlist(sapply(as.vector(speech.dat$Puhe_perusmuodossa[speech.dat$Puhuja==x]), function(x) unlist(strsplit(x, split=" ")))))
#wordlists <- sapply(as.vector(speech.dat$Puhe_perusmuodossa), function(x) unlist(strsplit(x, split=" ")))

