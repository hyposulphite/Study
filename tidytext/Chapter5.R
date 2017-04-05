library(tm)

#5.1 Tidying a document-term matrix
#5.1.1 Tidying DocumentTermMatrix objects
#Get data
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





