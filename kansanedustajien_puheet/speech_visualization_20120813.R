# Code for visualizing Finnish parliamentary speech data
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
# Copyright 2012 Juuso Parkkinen, juuso.parkkinen@gmail.com. All rights reserved.

# Load topic model output and preprocessed data
load("data/LDA_Gibbs_probmats_20120813.RData")

###############################
## EYE DIAGRAM VISUALIZATION ##
###############################

# Get the eye diagram code from Github: https://github.com/ouzor/eyediagram
eyediagram.folder <- "/Users/juusoparkkinen/Documents/workspace/ouzor/eyediagram/"
source(paste(eyediagram.folder, "eyediagram.R", sep=""))
input.folder <- paste(eyediagram.folder, "example/input/", sep="")
output.file <- paste(eyediagram.folder, "example/EyeDiagram_Speech_test", sep="")
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
font.sizes <- c(20, 22, 30, 30, 60, 60)
radius.adjustments <- c(-200, 0, 1.5, 0.8, 0.8, 90)
curve.factors <- c(10, 50, 10, 50)

# Write input for the eyediagram code
writeEyeDiagramInput(d2t, w2t, input.folder, output.file, infotext, font.sizes=font.sizes, radius.adjustments=radius.adjustments, curve.factors=curve.factors, topic.names=topics)

# Run the visualization script
setwd(eyediagram.folder)
system(paste(eyediagram.folder, "application.linux/eyediagram", sep=""))
setwd("/Users/juusoparkkinen/Documents/workspace/louhos/takomo/kansanedustajien_puheet/")

