# This script is part of the Louhos-project (http://louhos.github.com/)

# Copyright (C) 2010-2013 Juuso Parkkinen, Leo Lahti.
# Contact: <http://louhos.github.com/contact>. 
# All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


###############
## READ DATA ##
###############



# Data given as zip in the HS Next blog
# More information: http://blogit.hs.fi/hsnext/avodataa-kansanedustajien-puheet-muutettuna-perusmuotoon
temp <- tempfile()
download.file("http://www2.hs.fi/extrat/hsnext/edustajien-puheet.zip", temp)
raw.dat <- read.csv(unz(temp, "edustajien-puheet.csv"), sep="|", header=FALSE)
unlink(temp)

# Add column names manually (based on the xls-file)
names(raw.dat) <- c("Tunniste", "Päivämäärä", "Puhuja", "Puolue", "Vaalipiiri", "Käsiteltävä_asia", "Puhe_perusmuodossa", "Puhe_alkuperäisenä")
# Remove the original speech data to save space
speech.dat <- raw.dat[,-8]
# Save
dir.create("data")
save(speech.dat, file="data/Raw_speech_20120813.RData")


#####################
## PREPROCESS DATA ##
#####################

load("data/Raw_speech_20120813.RData")

# Extract representatives
representatives <- sort(levels(speech.dat$Puhuja))
representatives <- representatives[-which(representatives=="")]

# Add party info
rep.party <- sort(unique(paste(speech.dat$Puhuja, " (", speech.dat$Puolue, ")", sep="")))
rep.party <- rep.party[-which(rep.party==" ()")]
save(rep.party, file="data/Rep-party_20120813.RData")

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
rep.word.mat.raw <- matrix(0, nrow=length(representatives), ncol=length(word.table.raw), dimnames=list(representatives, names(word.table.raw)))
for (ri in 1:length(representatives)) {
  rep.table <- table(wordlist.raw[[ri]])
  rep.word.mat.raw[ri, names(rep.table)] <- rep.table
}
# sum(rep.word.mat.raw) # 1543827
save(rep.word.mat.raw, file="data/Representatives_vs_words_matrix_raw_20120813.RData")


#######################
## FILTER STOP WORDS ##
#######################

rep.word.mat.preprocessed <- rep.word.mat.raw
# First 548 in the list are numbers (these include some sensible years, though...)
rep.word.mat.preprocessed <- rep.word.mat.preprocessed[,-(1:548)]
# Remove stopwords
finnish.stopwords <- as.vector(as.matrix(read.csv("http://ouzor.github.com/files/data/misc/finnish_stop_edit_20120305.txt", skip=1, header=F)))
rep.word.mat.preprocessed <- rep.word.mat.preprocessed[,-which(colnames(rep.word.mat.preprocessed) %in% finnish.stopwords)]
# sum(rep.word.mat.preprocessed) # 1081889
save(rep.word.mat.preprocessed, file="data/Representatives_vs_words_matrix_preprocessed_20120813.RData")


#####################
## RUN TOPIC MODEL ##
#####################

# Load preprocessed data
load("data/Representatives_vs_words_matrix_preprocessed_20120813.RData")
load("data/Rep-party_20120813.RData")

# Use the 'topicmodels' package
library(topicmodels)}

# Use 20 topics. See package documentation for proper model selection.
K <- 20
# Set seed for random generator manually for reproducibility
seed <- 1
# For random seed you can use e.g. system time as follows
# as.integer(Sys.time())

# Set parameters for Gibbs inference
control_LDA_Gibbs <- list(alpha = 50/K, estimate.beta = TRUE, verbose = 10, prefix = tempfile(), save = 0, keep = 0, seed = seed, nstart = 1, best = TRUE, delta = 0.1, iter = 500, burnin = 100, thin = 400)
# Run LDA
system.time(lda.gibbs <- LDA(rep.word.mat.preprocessed, k=K, method = "Gibbs", control = control_LDA_Gibbs))
# Takes about 5 minutes
save(lda.gibbs, file="data/LDA_Gibbs_K20_I500_20120813.RData")

# List top 5 words for each topic
terms(lda.gibbs, 5)

# Separate probability matrices for visualization
lda <- lda.gibbs
doc2topic <- lda@gamma
rownames(doc2topic) <- rep.party
topic2word <- exp(lda@beta)
colnames(topic2word) <- lda@terms
lda.matrices <- list(doc2topic=doc2topic, topic2word=topic2word)
save(lda.matrices, file="data/LDA_Gibbs_probmats_20120813.RData")


###############################
## EYE DIAGRAM VISUALIZATION ##
###############################

# Load topic model output and preprocessed data
load("data/LDA_Gibbs_probmats_20120813.RData")

# Get the eye diagram code from Github: https://github.com/ouzor/eyediagram
eyediagram.folder <- "/Users/juusoparkkinen/Documents/workspace/ouzor/eyediagram/"
source(paste(eyediagram.folder, "eyediagram.R", sep=""))
input.folder <- paste(eyediagram.folder, "example/input/", sep="")
output.file <- paste(eyediagram.folder, "example/EyeDiagram_Speech_20120813", sep="")
dir.create(input.folder)

# Get top documents and words for each topic
NdocsPerTopic <- 7
NwordsPerTopic <- 10
topics <- (1:20)[-c(3, 11, 15)]
totake <- list(docs=c(), words=c(), topics=topics)
for (t in topics) {
  totake$docs <- unique(c(totake$docs, order(lda.matrices$doc2topic[,t], decreasing=T)[1:NdocsPerTopic]))
  totake$words <- unique(c(totake$words, order(lda.matrices$topic2word[t,], decreasing=T)[1:NwordsPerTopic]))  
}
d2t <- lda.matrices$doc2topic[totake$docs, totake$topics, drop=FALSE]
w2t <- t(lda.matrices$topic2word[totake$topics, totake$words, drop=FALSE])

# Set more parameters
infotext <- c("Left: Representatives", "Middle: Topics", "Rigth: Top words for the topics")
font.sizes <- c(20, 22, 30, 30, 40, 40)
radius.adjustments <- c(-200, 0, 1.5, 0.8, 0.8, 90)
curve.factors <- c(10, 100, 10, 100)

# Write input for the eyediagram code
writeEyeDiagramInput(d2t, w2t, input.folder, output.file, infotext, font.sizes=font.sizes, radius.adjustments=radius.adjustments, curve.factors=curve.factors, topic.names=topics)

# Run the visualization script
setwd(eyediagram.folder)
system(paste(eyediagram.folder, "application.linux/eyediagram", sep=""))
setwd("/Users/juusoparkkinen/Documents/workspace/louhos/takomo/kansanedustajien_puheet/")

# Run again, highlighting topic 18
output.file2 <- paste(eyediagram.folder, "example/EyeDiagram_Speech_Topic18_20120813", sep="")
writeEyeDiagramInput(d2t, w2t, input.folder, output.file2, infotext, hl.topic=15, font.sizes=font.sizes, radius.adjustments=radius.adjustments, curve.factors=curve.factors, topic.names=topics)
# Run the visualization script
setwd(eyediagram.folder)
system(paste(eyediagram.folder, "application.linux/eyediagram", sep=""))
setwd("/Users/juusoparkkinen/Documents/workspace/louhos/takomo/kansanedustajien_puheet/")
