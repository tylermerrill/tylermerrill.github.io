# Descriptive Stats

# Load in initial libs. (might take a minute)
# Will load in more later
require(quanteda)
require(quanteda.textstats)
require(quanteda.textplots)
require(quanteda.corpora)
require(ggplot2)
require(tm) 
require(readtext) 
library(dplyr)
library(tidytext)
library(magrittr)
library(tidyverse)

# Because our corpus is n = 845, 
# we will often 'repeat' steps later and
# graph w/ smaller output; but still run our analysis on full set.

# 'terms' object from dtm to list all terms
terms <- Terms(emma_dtm)
terms

# This is form tidyverse wants
# 'Rename' tibble in shorthand
emm <- emma_dta
emm

# Freq. plot:
library("quanteda.textstats")
tstat_freq_inaug <- textstat_frequency(dfmat_emma, n = 30)

ggplot(tstat_freq_inaug, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() + 
  labs(x = "Freq.", y = "Term",
       title = "Thirty Most Freq. Terms Overall",
       caption = "Source: EMMA Labs")


# Turn 'documents' var. into int !MAYBE!:
emm <- emm %>%
  mutate_at(vars(document), as.integer)

# Reference: Silge and Robinson (2017), chs. 2-4:
# Term freq.

freq1 <- emm %>%
  group_by(document)

tots <- freq1 %>%
  group_by(document) %>%
  summarize(total = sum(count))

freq1 <- left_join(freq1, tots)
freq1

freq1 %>%
  filter(count > 800) %>%
  ggplot(aes(x=count, y=reorder(term, count), fill = count)) +
  geom_col(show.legend = FALSE) +
  labs(
    x = "Term",
    y = "Frequency",
    title = "Most Freq. Terms (with T > 800 per doc)",
    caption = "Source: EMMA Labs"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

# Ordering
reor <- freq1 %>% arrange(desc(count))

# Get top 100
reor2 <- head(reor, 100)

reor2 %>%
  ggplot(aes(x=term, y = count)) +
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  labs(
    x = "Term",
    y = "Freq.",
    title = "Most Common Terms in Corpus",
    caption = "Source: EMMA Labs"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

# Freq. by rank.
# Zipf's law ==> frq.~ 1/rank
freqr <- freq1 %>%
  group_by(document) %>%
  mutate(rank = row_number(),
         'Term Frequency' = count/total)
freqr

freqr %>%
  ggplot(aes(rank, `Term Frequency`, color = document)) +
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  labs(
    x = "Rank",
    y = "Term Freq.",
    title = "Freq. by Rank",
    caption = "Source: EMMA Labs"
  ) +
  scale_x_log10() +
  scale_y_log10()

# Calc idf and tf-idf

# NOTE : "TF-IDF stands for Term Frequency Inverse Document Frequency of records. 
# It can be defined as the calculation of how relevant a word in a series or 
# corpus is to a text. The meaning increases proportionally to the number of times 
# in the text a word appears but is compensated by the word frequency 
# in the corpus (data-set)." a.k.a TF-IDF = freq in single doc * freq. in corpus.

tfidf <- freq1 %>%
  bind_tf_idf(term, document, count)
tfidf

# Reorder
ttttt <- tfidf %>% arrange(desc(tf_idf))
# Get most common
tfidf11 <- head(ttttt, 1200)

tfidf11 %>%
  ggplot(aes(x=term, y=tf_idf)) +
  geom_col(show.legend = FALSE) +
  labs(
    x = "Term",
    y = "tf-idf",
    title = "Top Twenty via TF-IDF of Terms",
    caption= "Source: EMMA Labs"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

# w/800 per doc
tfidf %>%
  filter(count > 800) %>%
  ggplot(aes(x=term, y=tf-idf)) +
  geom_col(show.legend = FALSE) +
  labs(
    x = "Term (Note: Negative tf-idf b/c ln of x < 1 is negative)",
    y = "tf-idf",
    title = "tf-idf of Terms (with T > 800 per doc)",
    caption= "Source: EMMA Labs"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

# Word comparisons
library(widyr)
# Set up var.
tango <- emm %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0)
tango

# Common pairs (occurring co-words) within same section (rows)
pairs <- tango %>%
  pairwise_count(term, section, sort = TRUE)
pairs

# Get top 30 of pairs
pairs2 <- head(pairs, 30)

pairs2 %>%
  ggplot(aes(x=item1, y=item2, fill = n)) +
  geom_tile(show.legend = TRUE) +
  labs(
    x = "Item One",
    y = "Item Two",
    title = "Top Thirty Pairs",
    caption= "Source: EMMA Labs"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))


# What words occur with, say, "year"?
pairs %>%
  filter(item1 == "year")

# Correlation among words,
# a.k.a how often together rather than not 
# any two words will be.
# Find phi coefficient, with n >= 20 to make
# sure focusing on commoner words
corrs <- tango %>%
  group_by(term) %>%
  filter(n() >= 20) %>%
  pairwise_cor(term, section, sort = TRUE)
corrs

# Get top 40 of corrs
corrs2 <- head(corrs, 40)

# Graph 'em
corrs2 %>%
  ggplot(aes(x=item1, y=item2, fill = correlation)) +
  geom_tile() +
  labs(
    x = "Item One",
    y = "Item Two",
    title = "Pair-Wise - Correlations",
    caption= "Source: EMMA Labs"
  ) +
  guides(fill= guide_legend(title = "Phi Coefficient")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

# What word occurs most often with, say, "fiscal"?
corrs %>%
  filter(item1 == "fiscal")

# Lets plot some of that:
corrs %>%
  filter(item1 %in% c("fiscal", "financi", "forego", "foreign")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# Let's network graph highly correlated pairs
library(ggraph)
library(igraph)
set.seed(2016)
corrs %>%
  filter(correlation > .90) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# Now, a little 'untidy':
# Reference: 'quanteda', https://tutorials.quanteda.io, secs 1-4:

# Lexical diversity of our texts (not used in text)
# NOTE: Takes a long time!
lexical.diversity <- textstat_lexdiv(toks_emma, measure = "all")
lexical.diversity

# Wordcloud
textplot_wordcloud(dfmat_emma, max_words = 100)

# Most common bi-grams:
toks_2gram <- tokens_ngrams(toks_emma, n = 2)
head(toks_2gram[[1]], 30)

# More bi-grams:
# Reference: Hall (2021), https://www.rpubs.com/Kalzay/CapstoneMilestoneReport:
bigram_dfm  <- dfm(toks_2gram)
bigram_freq <- textstat_frequency(bigram_dfm) %>%
  mutate(proportion = frequency / sum(frequency)) %>%
  mutate(cumulative_proportion = cumsum(proportion))
bigram_freq
topbi <- head(bigram_freq, 20)
ggplot(data = topbi, mapping = aes(x= frequency, y = feature)) + geom_col() +
  labs(x = "Freq.", y = "Bigram", title = "Top Bigrams", caption = "Source: EMMA Labs")
         
# Graph bigrams:
library(ggraph)
set.seed(2017)
ggraph(topbi, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  labs(title = "Top Bigrams", caption = "Source: EMMA Labs")

# Tri or quad-grams:
toks_3gram <- tokens_ngrams(toks_emma, n = 3:4)
head(toks_3gram[[1]], 30)

# Search for a keyword.
kw_emma1 <- kwic(toks_emma, pattern =  "demo*")
head(kw_emma1, 20)

# Look for multiple.
# With  window, specify the number of words to
# be displayed around keyword, if any.
kw_emma2 <- kwic(toks_emma, pattern = c("immig*", "demo*", "repub*", "conserv*", "liberal*"), window = 7)
head(kw_emma2, 20)

# Search for multi-word expressions:
kw_emma3 <- kwic(toks_emma, pattern = phrase("fiscal conservative*"))
head(kw_emma3)

# View token objects
View(kw_emma2)

# To find just words that appear around keywords
toks_emma_window <- tokens_select(toks_emma, pattern = c("immig*", "demo*", "conserv*", "liberal*"), padding = TRUE, window = 5)
print(toks_emma_window)
View(toks_emma_window)

# Get multi-word bundles
# (useful for bag-of-word approaches)
toks_comp <- tokens_compound(toks_emma, pattern = phrase(c("immig*", "demo*", "repub*", "conserv*", "liberal*")))
kw_comp <- kwic(toks_comp, pattern = c("immig*", "demo*", "repub*", "conserv*", "liberal*"))
head(kw_comp, 20)

# Dictionary Approach
# For this approach, we will use a new DTM
# that is NOT stemmed b/c dictionaries
# won't be (for the most part)!
# Reference: Puschmann & Haim (2019), https://content-analysis-with-r.com/4-dictionaries.html:

# Make custom dictionary. We will amend this based on Rahul's expertise and
# Tanmoy's research!

# Set up DTM for dictionary filtering:
dict1 <- dictionary(list(liberal = 'liber*',
                        conservative = c('jobs', 'welfare', 'illegal', 'border', 'troops'),
                        democracy = c('people','elect*','tax payer','burden','hard working', 'reform', 'enviro*', 'social justice', 'fair', 'equity'),
                        economy = c('freedom','liberty', 'red tape', 'wall street', 'washington')))

dict1_dtm <- dfm_lookup(emma2, dict1, exclusive=TRUE)
dict1_dtm

# w/ Rahuls' Words:

# Custom Terms: mayor, budget, expenditures, city, revenue, administration, Tax, lobby, property, 
# budget, bureaucracy, partisanship, election, spending, cuts, funds, power, separation, federalism,
# fiscal, executive, reforms, constitution, district, officer, authority, governor, president

dict2 <- dictionary(list(poli_structure = c('mayor', 'budget', 'expenditures', 'city', 'revenue', 'administration', 'Tax', 'lobby', 'property', 'budget', 'bureaucracy', 'spending', 'cuts', 'funds'),
                         poli_power = c('partisanship', 'election', 'power', 'separation', 'federalism','fiscal', 
                                          'executive', 'reforms', 'constitution', 'district', 'officer', 'authority', 'governor', 'president')))

dict2_dtm <- dfm_lookup(emma2, dict2, exclusive=TRUE)
dict2_dtm
dct22 <- convert(dict2_dtm, to = "data.frame")
summary(dct22)

# Better histos
# Load freq csv made w/ 'Pandas'
dct23 <- read_csv('betterdict.csv')

dct23 %>%
  ggplot(aes(x = doc_id, y = stru_freq, col = doc_id)) +
  geom_point() + 
  labs(x = "Document ID",
       y = "Poli Structure Freq.",
       title = "Dict. Distro",
       caption = "Source: EMMA Labs") +
  geom_vline(aes(xintercept = 640), color = "red") + 
  theme(legend.position = "none")

dct23 %>%
  ggplot(aes(x = doc_id, y = power_freq, col = doc_id)) +
  geom_point() + 
  labs(x = "Document ID",
       y = "Poli Power Freq.",
       title = "Dict. Distro",
       caption = "Source: EMMA Labs") +
  geom_vline(aes(xintercept = 320), color = "red") + 
  theme(legend.position = "none")

# OG Histos
dct22 %>%
  ggplot(aes(x = poli_structure, fill = doc_id)) +
  geom_histogram(alpha = 0.4) + 
  labs(x = "Political Structure, # Terms (Sum Total)",
       y = "Freq.",
       title = "Distrubution of Dictionary Partition: Political Structure",
       caption = "Source: EMMA Labs") +
  guides(fill = 'none') 

dct22 %>%
  ggplot(aes(x = poli_power, fill = doc_id)) +
  geom_histogram(alpha = 0.4) + 
  labs(x = "Political Power, # Terms (Sum Total)",
       y = "Freq.",
       title = "Distrubution of Dictionary Partition: Political Power",
       caption = "Source: EMMA Labs") +
  guides(fill = 'none') 

dct22 %>%
  ggplot(aes(x = doc_id, y = poli_structure)) +
  geom_col() + 
  labs(x = "Document ID#",
       y = "Poli Structure, # Terms",
       title = "Sum Count of Poli Structure Terms",
       caption = "Source: EMMA Labs") +
  geom_vline(aes(xintercept = 640), color = "red")
  guides(fill = 'none') 

dct22 %>%
  ggplot(aes(x = doc_id, y = poli_power)) +
  geom_col() + 
  labs(x = "Document ID#",
       y = "Poli Power, # Terms",
       title = "Sum Count of Poli Power Terms",
       caption = "Source: EMMA Labs") +
  geom_vline(aes(xintercept = 320), color = "red")
  guides(fill = 'none') 

# Using an existing dictionary
library(SentimentAnalysis)

GI_dict <- dictionary(DictionaryGI)

dict2_dtm <- dfm_lookup(emma2, GI_dict, exclusive=TRUE)
dict2_dtm

Findic <- dictionary(DictionaryHE)
summary(Findic)
get_sentiments("")
dict2_dtm <- dfm_lookup(emma2, Findic, exclusive=TRUE)
dict2_dtm

# More lexical diversity (used in text)
install.packages("quanteda.textstats")
library("quanteda.textstats")

EMMA_docs <- readtext("/Users/hibbobjibjob/Documents/classes/Methods/group/txts/*.txt")
textstat_readability(EMMA_docs$text,measure=c("Flesch","FOG","SMOG","Linsear.Write"))
g <- textstat_readability(EMMA_docs$text,measure=c("Flesch","FOG","SMOG","Linsear.Write"))

# Plot it:
f1 <-  ggplot(g, aes(x = document, y = Flesch)) +
  geom_point(alpha = 0.4) + 
  labs(x = "Document ID#",
       y = "Flesch Score",
       title = "Flesch Metric",
       caption = "Source: EMMA Labs") +
  geom_hline(aes(yintercept=27), color="red") + 
  guides(fill = 'none') 



f2 <-  ggplot(g, aes(x = document, y = FOG)) +
  geom_point(alpha = 0.4) + 
  labs(x = "Document ID#",
       y = "FOG Score",
       title = "FOG Metric",
       caption = "Source: EMMA Labs") +
  geom_hline(aes(yintercept = 22), color = "red") + 
  guides(fill = 'none') 


f3 <-  ggplot(g, aes(x = document, y = SMOG)) +
  geom_point(alpha = 0.4) + 
  labs(x = "Document ID#",
       y = "SMOG Score",
       title = "SMOG Metric",
       caption = "Source: EMMA Labs") +
  geom_hline(aes(yintercept = 18.2), color = "red") + 
  guides(fill = 'none') 


f4 <-  ggplot(g, aes(x = document, y = Linsear.Write)) +
  geom_point(alpha = 0.4) + 
  labs(x = "Document ID#",
       y = "Linsear Write Score",
       title = "Linsear Write Metric",
       caption = "Source: EMMA Labs") +
  geom_hline(aes(yintercept = 28), color = "red") + 
  guides(fill = 'none') 


library("ggpubr")
# Reference: A. Kassambara, https://rpkgs.datanovia.com/ggpubr/reference/ggarrange.html:
figure <- ggarrange(f1,f2,f3,f4,
                    ncol = 2, nrow = 2)
figure








