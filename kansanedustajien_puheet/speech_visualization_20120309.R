# Code for visualizing edustajien-puheet data
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
# Copyright 2012 Juuso Parkkinen, juuso.parkkinen@gmail.com. All rights reserved.

## LOAD DATA

load("Speech_preprocessed_LDA_C20_20120306.RData")
load("Representatives_vs_words_matrix_preprocessed_20120305.RData")
load("Rep-party_20120307.RData")

source("../../source/circular_component_visualization_20120309.R")
doc2topic <- speech.pre.lda$estimates$theta.hat
rownames(doc2topic) <- rep.party
word2topic <- t(speech.pre.lda$estimates$phi.hat)
#word2topic <- t(prop.table(speech.pre.lda$estimates$phi.hat, 2))
rownames(word2topic) <- colnames(rep.word.mat.preprocessed)

totake <- totake.topics(1:20, doc2topic, word2topic, 100, 200)
infotext <- c("Left: Representatives", 
              "Middle: Topics", 
              "Rigth: Top words for the topics")
d2t <- doc2topic[totake$docs, totake$topics, drop=FALSE]
w2t <- word2topic[totake$words, totake$topics, drop=FALSE]
font.sizes <- c(20, 22, 30, 30, 60, 60)
radius.adjustments <- c(-150, 0, 1.5, 0.8, 0.8, 90)
curve.factors <- c(10, 20, 5, 50)
# filename <- "Speech_preprocessed_LDA_C20"
# filepath <- paste("/Users/juusoparkkinen/Documents/workspace/source/CircCompViz/", filename, "_eye_diagram_20120307", sep="")
# writeCircCompVizInput(d2t, w2t, filepath, infotext=infotext, font.sizes=font.sizes, radius.adjustments=radius.adjustments, curve.factors=curve.factors)
# system("/share/mi/exp/multirex/source/CircCompViz/CircCompViz")
# #system(paste("convert ", filepath,".pdf ",filepath,".png",sep=""))  

# Focus on some interesting topics
topics <- c(1, 4, 6, 8, 10, 11, 13, 14, 15, 16, 17, 18)
totake <- totake.topics(topics, doc2topic, word2topic, 100, 200)
infotext <- c("Left: Representatives", 
              "Middle: Interesting topics", 
              "Rigth: Top words for the topics")
d2t <- doc2topic[totake$docs, totake$topics, drop=FALSE]
w2t <- word2topic[totake$words, totake$topics, drop=FALSE]
font.sizes <- c(20, 22, 30, 30, 60, 60)
radius.adjustments <- c(-150, 0, 1.5, 0.8, 0.8, 90)
curve.factors <- c(10, 20, 5, 50)
filename <- "Speech_preprocessed_LDA_C20"
filepath <- paste("/Users/juusoparkkinen/Documents/workspace/suomiR/edustajien-puheet/", filename, "_eye_diagram_focused_20120309", sep="")
writeCircCompVizInput(d2t, w2t, filepath, infotext=infotext, font.sizes=font.sizes, radius.adjustments=radius.adjustments, curve.factors=curve.factors, topic.names=topics)
system("/share/mi/exp/multirex/source/CircCompViz/CircCompViz")

