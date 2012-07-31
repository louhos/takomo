# Speech LDA

# Load preprocessed data
load("Representatives_vs_words_matrix_preprocessed_20120731.RData")
load("Rep-party_20120731.RData")

# Run topic model
# Use the topicmodels package
library(topicmodels)
# Use 20 topics. See package documentation for proper model selection.
K <- 20
# Set parameters
control_LDA_VEM <- list(estimate.alpha = TRUE, alpha = 50/K, estimate.beta = TRUE, verbose = 1, prefix = tempfile(), save = 0, keep = 0, seed = as.integer(Sys.time()), nstart = 1, best = TRUE, var = list(iter.max = 10, tol = 10^-6), em = list(iter.max = 10, tol = 10^-4))
# Run LDA
lda <- LDA(rep.word.mat.preprocessed, k=K, method = "VEM", control = control_LDA_VEM)
# Save model output for postprocessing
save(lda, file="LDA_VEM_K20_I10_20120731.RData")

# POSTPROCESSING
# Get LDA theta and phi based on word assignments
wordass <- lda@wordassignments
theta <- matrix(0, nrow=nrow(rep.word.mat.preprocessed), ncol=K)
phi <- matrix(0, nrow=K, ncol=ncol(rep.word.mat.preprocessed))
for (ii in seq(wordass$i)) {
  doc <- wordass$i[ii]
  word <- wordass$j[ii]
  topic <- wordass$v[ii]
  theta[doc, topic] <- theta[doc, topic] +1
  phi[topic, word] <- phi[topic, word] +1
}
# Normalize
theta <- prop.table(theta, margin=1)
phi <- prop.table(phi, margin=1)
