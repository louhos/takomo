# Code for running topic model on Finnish parliamentary speech data
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
# Copyright 2012 Juuso Parkkinen, juuso.parkkinen@gmail.com. All rights reserved.


#####################
## RUN TOPIC MODEL ##
#####################

# Load preprocessed data
load("data/Representatives_vs_words_matrix_preprocessed_20120813.RData")
load("data/Rep-party_20120813.RData")

# Use the 'topicmodels' package
library(topicmodels)
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


