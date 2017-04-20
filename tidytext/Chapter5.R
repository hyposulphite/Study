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
data("AssociatedPress", package = "topicmodels")
AssociatedPress

#?# Create terms: terms of the document
terms <- Terms(AssociatedPress)
head(terms)

#?# Create ap_td: Transform AssociatedPress into one-token-per-document-per-row format
ap_td <- tidy(AssociatedPress)
ap_td

#?# conduct sentiment analysis using "bing"
ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))
ap_sentiments

#?# Visualize most positive and most negative words
ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()


###5.1.2 Tidying dfm objects
#?# get data data_corpus_inaugural from <quanteda>
#?# create inaug_dfm: use quanteda::dfm
data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural)
inaug_dfm

#?# create inaug_td: transform dfm into tidy text
inaug_td <- tidy(inaug_dfm)
inaug_td

#?# create inaug_tf_idf: get tf_idf
inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf

#?# pick several words and visualize how they changed in frequency over time
#?#   words: ("god", "america", "foreign", "union", "constitution", "freedom")

year_term_counts <- inaug_td %>%
  # extract(document, "year", "(\\d+)", convert = TRUE) %>%
  mutate(year=str_extract(document, "(\\d+)")) %>% 
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))

year_term_counts %>%
  filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("% frequency of word in inaugural address")


###5.2 Casting tidy text data into a matrix
#?# cast tidy data [ap_td] into dtm/dfm
ap_td %>%
  cast_dtm(document, term, count)
ap_td %>%
  cast_dfm(term, document, count)


#?# cast ap_td into a Matrix object
m <- ap_td %>%
  cast_sparse(document, term, count)
class(m)
dim(m)


# cast austen_books from <janeaustenr> into austen_dtm

austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)

austen_dtm


###5.3 Tidying corpus objects with metadata
#?# get corpus "acq" from <tm>
data("acq")
acq
acq[[1]]

#?# create acq_td: tidy acq
acq_td <- tidy(acq)
acq_td

#?# create acq_tokens: tokenize acq_td column "text", remove places and remove stop words
acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

#?# most common words
acq_tokens %>%
  count(word, sort = TRUE)

#?# tf-idf
acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))


###5.3.1 Example: mining financial articles

#_# create symbol list
company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook",
             "Twitter", "IBM", "Yahoo", "Netflix")
symbol <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", "TWTR", "IBM", "YHOO", "NFLX")

download_articles <- function(symbol) {
  WebCorpus(GoogleFinanceSource(paste0("NASDAQ:", symbol)))
}

#?# Create stock_articles: Get all articles. One corpus per row
stock_articles <- data_frame(company = company,
                             symbol = symbol) %>%
  mutate(corpus = map(symbol, download_articles))

stock_articles

#?# create stock_tokens: get all words from each corpus
#?#  select: company, datetimestamp, word, id, heading
stock_tokens <- stock_articles %>%
  unnest(map(corpus, tidy)) %>%
  unnest_tokens(word, text) %>%
  select(company, datetimestamp, word, id, heading)

stock_tokens

#?# create stock_tf_idf: get tf_idf by company
stock_tf_idf <- stock_tokens %>%
  count(company, word) %>%
  filter(!str_detect(word, "\\d+")) %>%
  bind_tf_idf(word, company, n) %>%
  arrange(-tf_idf)
#?# and try to plot the words by company, ordered by tf_idf


#?# sentiment analysis: get strongest words from stock_tokens
#?#  sentiment: afinn
stock_tokens %>%
  anti_join(stop_words, by = "word") %>%
  count(word, id, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(contribution = sum(n * score),
            abscontribution = abs(contribution)) %>%
  top_n(12, abscontribution) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution)) +
  geom_col() +
  coord_flip() +
  labs(y = "Frequency of word * AFINN score")


#?# sentiment: loughran
stock_tokens %>%
  count(word) %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  group_by(sentiment) %>%
  top_n(5, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ sentiment, scales = "free") +
  ylab("Frequency of this word in the recent financial articles")


#?# spread sentiment count table: company by sentiment type

stock_sentiment_count <- stock_tokens %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  count(sentiment, company) %>%
  spread(sentiment, n, fill = 0)

stock_sentiment_count

stock_sentiment_count %>%
  mutate(score = (positive - negative) / (positive + negative)) %>%
  mutate(company = reorder(company, score)) %>%
  ggplot(aes(company, score, fill = score > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "Company",
       y = "Positivity score among 20 recent news articles")






###
#Questions:
# 1. DTM <==> corpus
