
library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(tidyr)
library(wordcloud)
library(reshape2)

###########################

### Chapter 2

#?# print sentiments files: afinn, bing and nrc
sentiments
sentiments %>% count(lexicon)
sentiments %>% group_by(lexicon) %>% count(sentiment)
sentiments %>% group_by(lexicon) %>% count(score)

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

###2.2 Sentiment analysis with inner join

#?# get tidy_books from austen_books: add linenumber and chapter number
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

#?# get Emma book from tidy_books, and check words in joy sentiment
tidy_books %>% 
  filter(book=="Emma") %>% 
  inner_join(get_sentiments("nrc") %>% filter(sentiment=="joy")) %>% 
  count(word, sort=TRUE)

#?# get positive and negative index of the tidy_books, by blocks of 80 lines
bing_sent <- get_sentiments("bing")

word_cnt <- tidy_books %>% 
  group_by(book) %>% 
  mutate(block=linenumber %/% 80) %>% 
  count(book, block) %>% 
  rename(all_word_cnt=n)

sent_summ = tidy_books %>% 
  group_by(book) %>% 
  mutate(block=linenumber %/% 80) %>% 
  inner_join(bing_sent) %>% 
  count(block, sentiment) %>% 
  ungroup() %>% 
  spread(sentiment, n, fill=0) %>%
  mutate(score=positive-negative) %>% 
  left_join(word_cnt) %>% 
  mutate(proportion=score/all_word_cnt)

#?# plot overall sentiment index by book and block
sent_summ %>% 
  ggplot(aes(x=block, y=proportion, fill=book)) +
  geom_bar(stat="identity") +
  facet_wrap(~book, ncol = 2, scales = "free_x")



###2.3 Comparing the three sentiment dictionaries

#?# create pride_prejudice: "Pride & Prejudice" in tidy_books

#?# create afinn: index=block number, sentiment index and method="AFINN"

#?# pride_prejudice "bing" and "nrc" positive and negative summary by block

#?# plot afinn, bing and nrc scores

###2.4 Most common positive and negative words

#?# tidy_books join "bing" plot most common positive and negative words

###2.5 Wordclouds

#?# plot word cloud for the top 100 words

#?# comparison word cloud 

#?## colors = c("#F8766D", "#00BFC4")

#?# create PandP_sentences: prideprejudice one sentence per row

#?# create austen_chapters: austen_books one chapter per row

#?# get the chapter for each book with the highest ratio of negative words/words
