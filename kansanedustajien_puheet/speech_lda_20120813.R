# Speech LDA

# Load preprocessed data
load("Representatives_vs_words_matrix_preprocessed_20120731.RData")

## RUN THE TOPIC MODEL

# Use the topicmodels package
library(topicmodels)
# Use 20 topics. See package documentation for proper model selection.
K <- 20
# Set parameters for Gibbs inference
control_LDA_Gibbs <- list(alpha = 50/K, estimate.beta = TRUE, verbose = 10, prefix = tempfile(), save = 0, keep = 0, seed = as.integer(Sys.time()), nstart = 1, best = TRUE, delta = 0.1, iter = 500, burnin = 100, thin = 400)
# Run LDA
system.time(lda.gibbs <- LDA(rep.word.mat.preprocessed, k=K, method = "Gibbs", control = control_LDA_Gibbs))
# Takes about 5 minutes
save(lda.gibbs, file="data/LDA_Gibbs_K20_I500_20120813.RData")

# Separate probability matrices
lda <- lda.gibbs
doc2topic <- lda@gamma
rownames(doc2topic) <- lda@documents
topic2word <- exp(lda@beta)
colnames(topic2word) <- lda@terms
lda.matrices <- list(doc2topic=doc2topic, topic2word=topic2word)
save(lda.matrices, file="data/LDA_Gibbs_probmats_20120813.RData")


