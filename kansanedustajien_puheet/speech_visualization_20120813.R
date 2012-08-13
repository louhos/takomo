# Code for visualizing edustajien-puheet data
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
# Copyright 2012 Juuso Parkkinen, juuso.parkkinen@gmail.com. All rights reserved.

# Load 
load("data/LDA_Gibbs_probmats_20120813.RData")
load("data/Representatives_vs_words_matrix_preprocessed_20120731.RData")
load("data/Rep-party_20120731.RData")

# Add party info to representatives
rownames(lda.matrices$doc2topic) <- rep.party

# Read eye diagram code
# Get the code from Github: https://github.com/ouzor/eyediagram
eyediagram.folder <- "/Users/juusoparkkinen/Documents/workspace/ouzor/eyediagram/"
source(paste(eyediagram.folder, "eyediagram.R", sep=""))
input.folder <- paste(eyediagram.folder, "example/input/", sep="")
output.file <- paste(eyediagram.folder, "example/EyeDiagram_Speech_test", sep="")
dir.create(input.folder)

# Get top documents and words for each topic
NdocsPerTopic <- 7
NwordsPerTopic <- 7
topics <- (1:20)[-c(4, 19)]
totake <- list(docs=c(), words=c(), topics=topics)
for (t in topics) {
  totake$docs <- unique(c(totake$docs, order(lda.matrices$doc2topic[,t], decreasing=T)[1:NdocsPerTopic]))
  totake$words <- unique(c(totake$words, order(lda.matrices$topic2word[t,], decreasing=T)[1:NwordsPerTopic]))  
}
d2t <- lda.matrices$doc2topic[totake$docs, totake$topics, drop=FALSE]
w2t <- t(lda.matrices$topic2word[totake$topics, totake$words, drop=FALSE])

# Set more parameters
infotext <- c("Left: Representatives", "Middle: Topics", "Rigth: Top words for the topics")
font.sizes <- c(20, 22, 30, 30, 60, 60)
radius.adjustments <- c(-150, 0, 1.5, 0.8, 0.8, 90)
curve.factors <- c(10, 50, 10, 50)

# Write input for the eyediagram code
writeEyeDiagramInput(d2t, w2t, input.folder, output.file, infotext, font.sizes=font.sizes, radius.adjustments=radius.adjustments, curve.factors=curve.factors, topic.names=topics)

# Run the code
setwd(eyediagram.folder)
system(paste(eyediagram.folder, "application.linux/eyediagram", sep=""))
setwd("/Users/juusoparkkinen/Documents/workspace/louhos/takomo/kansanedustajien_puheet/")

