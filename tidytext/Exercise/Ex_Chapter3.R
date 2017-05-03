
library(dplyr)
library(janeaustenr)
library(tidytext)
library(ggplot2)
library(gutenbergr)

###3.1 Term frequency in Jane Austen’s novels
#?# create book_words from austen_books: book, word, n=word count
#?#   , total=word count by book. Sort by count
book_words = austen_books() %>% 
  unnest_tokens(word, text, token="words") %>% 
  count(book, word, sort=TRUE)

total_words = book_words %>% 
  group_by(book) %>% 
  summarise(total=n()) %>% 
  ungroup()

book_words = book_words %>% 
  left_join(total_words)

#?# plot word frequency distribution, by book
book_words %>% 
  mutate(freq=n/total) %>% 
  anti_join(stop_words) %>% 
  group_by(book) %>% 
  top_n(10,n) %>%
  mutate(word=reorder(word, freq)) %>% 
  ungroup() %>% 
  ggplot(aes(word, freq, fill=book)) +
  geom_bar(stat="identity") +
  facet_wrap(~book, scale="free") +
  coord_flip()


###3.2 Zipf's law
#?# create freq_by_rank: rank=count rank by book, term frequency
freq_by_rank = book_words %>% 
  mutate(freq=n/total) %>% 
  group_by(book) %>% 
  mutate(rank=row_number()) %>% 
  ungroup()

#?#   plot `term frequency` by rank, overlay by book
freq_by_rank %>% 
  ggplot(aes(x=rank, y=freq, color=book)) +
  geom_line() +
  scale_x_log10() +
  scale_y_log10()

###3.3 The bind_tf_idf function

#?# create book_words1: get bind_tf_idf, sort by tf_idf

#?# plot tf_idf

#?# plot tf_idf by book

###3.4 A corpus of physics texts

#?# create physics_word: gutenberg(37729, 14725, 13476, 5001), in word and count, add author

#?#   1. plot tf_idf by author
