library(tm)

#5.1 Tidying a document-term matrix
#5.1.1 Tidying DocumentTermMatrix objects
#Get data AssociatedPress from "topicmodels"
data("AssociatedPress", package = "topicmodels")
AssociatedPress

#Access terms of the document
terms <- Terms(AssociatedPress)
head(terms)

#transform matrix into one-token-per-document-per-row format
library(dplyr)
library(tidytext)

ap_td <- tidy(AssociatedPress)
ap_td

#conduct sentiment analysis
ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments

#Visualize most positive and most negative words
library(ggplot2)

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

#5.1.2 Tidying dfm objects

library(methods)

#get data data_corpus_inaugural from <quanteda>
data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural)
inaug_dfm

#transform dfm into tidy text
inaug_td <- tidy(inaug_dfm)
inaug_td

#get tf_idf
inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf

#pick several words and visualize how they changed in frequency over time
library(tidyr)

year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>%
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


##5.2
# cast tidy data [ap_td] into dtm/dfm
ap_td %>%
  cast_dtm(document, term, count)
ap_td %>%
  cast_dfm(term, document, count)


# cast into a Matrix object
m <- ap_td %>%
  cast_sparse(document, term, count)
class(m)
dim(m)

# cast austen_books from <janeaustenr> into dtm
library(janeaustenr)

austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)

austen_dtm


##5.3 Tidying corpus objects with metadata
# get corpus "acq" from <tm>
data("acq")
acq
acq[[1]]

# tidy acq
acq_td <- tidy(acq)
acq_td

# tokenize column "text"
acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# most common words
acq_tokens %>%
  count(word, sort = TRUE)

# tf-idf
acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))

##5.3.1 Example: mining financial articles





###
#Questions:
# 1. DTM <==> corpus
