
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
  count(book, word, sort=TRUE) %>% 
  ungroup()

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
book_words1 = book_words %>% 
  bind_tf_idf(n_col = n, term_col=word, document_col=book) %>% 
  arrange(-tf_idf)

#?# plot tf_idf
book_words1 %>% 
  top_n(n=20, wt=tf_idf) %>% 
  mutate(word=reorder(word, tf_idf)) %>% 
  ggplot(aes(x=word, y=tf_idf, fill=book)) +
  geom_bar(stat="identity") +
  coord_flip()

#?# plot tf_idf by book
book_words1 %>% 
  group_by(book) %>% 
  top_n(n=10, wt=tf_idf) %>% 
  ungroup() %>% 
  mutate(word=reorder(word, tf_idf)) %>% 
  ggplot(aes(x=word, y=tf_idf, fill=book)) +
  geom_bar(stat="identity") +
  facet_wrap(~book, ncol=2, scale="free") +
  coord_flip()


###3.4 A corpus of physics texts
#?# create physics_word: gutenberg(37729, 14725, 13476, 5001), in word and count, add author
#?#   1. plot tf_idf by author

physics_word0 = gutenberg_download(gutenberg_id = c(37729, 14725, 13476, 5001), meta_fields = "author")
physics_word = physics_word0 %>% 
  unnest_tokens(word, text, token="words") %>% 
  count(word, author, sort=TRUE) %>% 
  ungroup()

my_stop_words=data_frame(word=c("k1","eq","ac","rc","cm","cg","cb","ak","bn"))

physics_word %>% 
  anti_join(stop_words) %>% 
  anti_join(my_stop_words) %>% 
  bind_tf_idf(word, author, n) %>% 
  group_by(author) %>% 
  top_n(15) %>% 
  mutate(word=reorder(word, tf_idf)) %>% 
  ggplot(aes(x=word, y=tf_idf, fill=author)) +
  geom_bar(stat="identity") +
  facet_wrap(~author, scale="free") +
  coord_flip()






