
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
data(AssociatedPress, package="topicmodels")

#?# Create terms: terms of the document
terms = Terms(AssociatedPress)

#?# Create ap_td: Transform AssociatedPress into one-token-per-document-per-row format
ap_td = tidy(AssociatedPress)

#?# conduct sentiment analysis using "bing"
ap_sentiments = ap_td %>% 
  inner_join(get_sentiments("bing"), by=c("term"="word"))
ap_sentiments

#?# Visualize most positive and most negative words
ap_sentiments %>% 
  group_by(term, sentiment) %>% 
  summarise(count = sum(count)) %>% 
  group_by(sentiment) %>% 
  top_n(10, wt=count) %>% 
  mutate(term=reorder(term, count)) %>% 
  ggplot(aes(term, count, fill=sentiment)) +
  geom_bar(stat="identity") +
  facet_wrap(~sentiment, scale="free") +
  coord_flip()

###5.1.2 Tidying dfm objects
#?# get data data_corpus_inaugural from <quanteda>
data(data_corpus_inaugural, package="quanteda")

#?# create inaug_dfm: use quanteda::dfm
inaug_dfm = quanteda::dfm(data_corpus_inaugural)

#?# create inaug_td: transform dfm into tidy text
inaug_td = tidy(inaug_dfm)

#?# create inaug_tf_idf: get tf_idf
inaug_tf_idf = inaug_td %>% 
  bind_tf_idf(term, document, count)

#?# pick several words and visualize how they changed in frequency over time
#?#   words: ("god", "america", "foreign", "union", "constitution", "freedom")
inaug_tf_idf %>% 
  separate(document, c("year","name"), sep="-") %>% 
  filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>% 
  complete(year, term, fill=list(count=0)) %>% 
  group_by(term) %>% 
  mutate(year_total = sum(count)) %>% 
  ungroup() %>% 
  ggplot(aes(x=year, y=count/year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~term, scale="free")

###5.2 Casting tidy text data into a matrix
#?# cast tidy data [ap_td] into dtm/dfm
ap_td %>% 
  cast_dtm(document, term, count)
ap_td %>% 
  cast_dfm(document, term, count)

#?# cast ap_td into a Matrix object
m = ap_td %>% 
  cast_sparse(document, term, count)

###5.3 Tidying corpus objects with metadata
#?# get corpus "acq" from <tm>

#?# create acq_td: tidy acq

#?# create acq_tokens: tokenize acq_td column "text", remove places and remove stop words

#?# most common words

#?# tf-idf

###5.3.1 Example: mining financial articles
#_# create symbol list
company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook",
             "Twitter", "IBM", "Yahoo", "Netflix")
symbol <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", "TWTR", "IBM", "YHOO", "NFLX")
download_articles <- function(symbol) {
  WebCorpus(GoogleFinanceSource(paste0("NASDAQ:", symbol)))
}

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
