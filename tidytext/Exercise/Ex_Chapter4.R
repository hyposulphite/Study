
library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)
library(igraph)
library(ggraph)
library(gutenbergr)
library(stringr)
library(widyr)

###4.1 Tokenizing by n-gram

#?# create austen_bigrams: tokenized by bigram

###4.1.1 Counting and filtering n-grams

#?# austen_bigrams count words

#?# create bigrams_separated: austen_bigrams separated bigrams

#?# create bigrams_filtered: bigrams_separated neither words in stop_words

#?#   1. Create bigrams_counts as count from bigrams_filtered

#?#   2. Also count and unite the bigrams

#?# word count by trigram

###4.1.2 Analyzing bigrams

#?# from bigrams_filtered, create tf_idf for XX street, and plot

###4.1.3 Using bigrams to provide context in sentiment analysis

#?# Plot The most common positive or negative words to follow negations 

#?#   such as ‘never’, ‘no’, ‘not’, and ‘without’, by negation words

###4.1.4 Visualizing a network of bigrams with igraph

#?# This section: check original code and study

#?# Get bigram_graph object from bigram_counts with top 20 words

###

###4.2 Counting and correlating pairs of words with the widyr package

###4.2.1 Counting and correlating among sections

#?# create austen_section_words: austen_books filtered by "Pride & Prejudice"

#?#   section=10 rows block and filter out stopwords

#?# create word_pairs: count words co-occuring within sections

###4.2.2 Pairwise correlation

#?# create word_cors: from austen_section_words,

#?#   filter common words (count>20) and get pairwise correlation

#?# plot words having highest correlation with ("elizabeth", "pounds", "married", "pride")
