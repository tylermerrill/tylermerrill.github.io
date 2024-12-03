# Clear work environment
rm(list=ls())

# Required packages
# Each package must be installed first
install.packages("stm")
require(stm)

# Import data
offering_docs <- read.csv("MuniBonds.csv", stringsAsFactors=FALSE)
# text of each human right report, aggregated by doc.id in Cordell et al. (2022) dataset

# Examine data
colnames(offering_docs) # Look at column names
nrow(offering_docs) # Calculate how many sentences are in data
ncol(offering_docs) # Calculate how many variables are in data
head(offering_docs)[1,] # Look at first row for all variables

# Pre-processing text content (we will use stm package)


# Pre-process texts using built in stm processing steps
processed <- textProcessor(offering_docs$text) 
# E.g.: converting text to lower case, removing punctuation, removing stopwords, removing numbers, stemming text

# Examine the data
str(processed)
summary(processed)

# Performs several corpus manipulations including removing words and renumbering word indices
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
head(out)
docs <- out$documents
head(docs)
vocab <- out$vocab
head(vocab)
meta <- out$meta
head(meta)
# Examine data
summary(docs) # List of documents
summary(vocab) # Character vector of words in the vocabulary
summary(meta) # Document metadata

# inspect features of the documents and the associated vocabulary list to make sure they have been correctly pre-processed
plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))
?plotRemoved

#Estimate Structural Topic Model (we will use stm package)


# Estimation of the Structural Topic Model using semi-collapsed variational EM. 
# The function takes sparse representation of a document-term matrix, an integer number of topics, and covariates and returns fitted model parameters. 
# Covariates can be used in the prior for topic prevalence, in the prior for topical content or both. 
india_stm <- stm(documents = out$documents, vocab = out$vocab, K = 10)

# Examine stm
summary(india_stm) # 10 topics

# 5. Visualize Structural Topic Model (we will use stm package)


# Corpus level visualization (topics in India reports)

# Plot stm
par(mar = c(1, 1, 1, 1))
?plot.STM
plot(india_stm, type = "summary",labeltype = c("prob"), xlab ="Proportion for Highest Probability Algorithm")
plot(india_stm, type = "summary",labeltype = c("score"), xlab ="Proportion for Score Algorithm")
plot(india_stm, type = "summary",labeltype = c("lift"), xlab ="Proportion for Lift Algorithm")
plot(india_stm, type = "summary",labeltype = c("frex"), xlab ="Proportion for frex Algorithm")

plot(india_stm, type = "hist")

mod.out.corr <- topicCorr(india_stm, cutoff=0.02)
plot(mod.out.corr)
?topicCorr