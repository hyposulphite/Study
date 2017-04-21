

library(tm)

library(dplyr)

library(tidytext)

library(ggplot2)

library(methods)

library(tidyr)

library(stringr)

library(janeaustenr)

library(tm.plugin.webmining)

library(purrr)

###5.1 Tidying a document-term matrix

###5.1.1 Tidying DocumentTermMatrix objects

#?# Get data AssociatedPress from "topicmodels"

#?# Create terms: terms of the document

#?# Create ap_td: Transform AssociatedPress into one-token-per-document-per-row format

#?# conduct sentiment analysis using "bing"

#?# Visualize most positive and most negative words

###5.1.2 Tidying dfm objects

#?# get data data_corpus_inaugural from <quanteda>

#?# create inaug_dfm: use quanteda::dfm

#?# create inaug_td: transform dfm into tidy text

#?# create inaug_tf_idf: get tf_idf

#?# pick several words and visualize how they changed in frequency over time

#?#   words: ("god", "america", "foreign", "union", "constitution", "freedom")

###5.2 Casting tidy text data into a matrix

#?# cast tidy data [ap_td] into dtm/dfm

#?# cast ap_td into a Matrix object

###5.3 Tidying corpus objects with metadata

#?# get corpus "acq" from <tm>

#?# create acq_td: tidy acq

#?# create acq_tokens: tokenize acq_td column "text", remove places and remove stop words

#?# most common words

#?# tf-idf

###5.3.1 Example: mining financial articles

#?# Create stock_articles: Get all articles. One corpus per row

#?# create stock_tokens: get all words from each corpus

#?#  select: company, datetimestamp, word, id, heading

#?# create stock_tf_idf: get tf_idf by company

#?# and try to plot the words by company, ordered by tf_idf

#?# sentiment analysis: get strongest words from stock_tokens

#?#  sentiment: afinn

#?# sentiment: loughran

#?# spread sentiment count table: company by sentiment type

###
