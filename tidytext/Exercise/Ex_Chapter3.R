
library(dplyr)
library(janeaustenr)
library(tidytext)
library(ggplot2)
library(gutenbergr)

###3.1 Term frequency in Jane Austen’s novels

#?# create book_words from austen_books: book, word, n=word count

#?#   , total=word count by book. Sort by count

#?# plot word frequency distribution, by book

###3.2 Zipf's law

#?# create freq_by_rank: rank=count rank by book, term frequency

#?#   plot `term frequency` by rank, overlay by book

###3.3 The bind_tf_idf function

#?# create book_words1: get bind_tf_idf, sort by tf_idf

#?# plot tf_idf

#?# plot tf_idf by book

###3.4 A corpus of physics texts

#?# create physics_word: gutenberg(37729, 14725, 13476, 5001), in word and count, add author

#?#   1. plot tf_idf by author
