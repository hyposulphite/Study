
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
pride_prejudice = tidy_books %>% filter(book=="Pride & Prejudice")

#?# create afinn: index=block number, sentiment index and method="AFINN"
afinn = pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  mutate(index = linenumber %/% 80) %>% 
  group_by(index) %>% 
  summarise(sentiments=sum(score)) %>% 
  ungroup() %>% 
  mutate(method = "AFINN")

#?# pride_prejudice "bing" and "nrc" positive and negative summary by block
bing = pride_prejudice %>% 
  inner_join(get_sentiments("bing")) %>% 
  group_by(index=linenumber %/% 80) %>% 
  count(index, sentiment) %>%
  ungroup() %>% 
  mutate(method="BING") %>% 
  spread(key=sentiment, value=n, fill=0)

nrc = pride_prejudice %>% 
  inner_join(get_sentiments("nrc") %>% filter(sentiment %in% c("negative","positive"))) %>% 
  group_by(index=linenumber %/% 80) %>% 
  count(index, sentiment) %>% 
  ungroup() %>% 
  mutate(method="NRC") %>% 
  spread(key=sentiment, value=n, fill=0)

#?# plot afinn, bing and nrc scores
all_scores = bind_rows(afinn %>% select(index, method, score=sentiments),
                       bing %>% mutate(score=positive-negative) %>% select(index, method, score),
                       nrc %>% mutate(score=positive-negative) %>% select(index, method, score))

all_scores %>% 
  ggplot(aes(x=index, y=score, fill=method)) +
  geom_bar(stat="identity") +
  facet_wrap(~method, ncol=1, scales="free_y")

###2.4 Most common positive and negative words
#?# tidy_books join "bing" plot most common positive and negative words
word_sentiment_afinn = 
  pride_prejudice %>% 
    inner_join(get_sentiments("afinn")) %>% 
    group_by(word) %>% 
    summarise(score=sum(score)) %>% 
    mutate(sentiment=ifelse(score>0, "positive","negative"), score=abs(score))

word_sentiment_bing = 
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>% 
    count(word, sentiment) %>% 
    rename(score=n)

word_sentiment_nrc = 
  pride_prejudice %>% 
    inner_join(get_sentiments("nrc") %>% filter(sentiment %in% c("positive","negative"))) %>% 
    count(word, sentiment) %>% 
    rename(score=n)


word_sentiment_afinn %>%
  group_by(sentiment) %>% 
  top_n(n=10, wt=score) %>% 
  mutate(word=reorder(word,score)) %>% 
  ggplot(aes(x=word, y=score, fill=sentiment)) +
  geom_bar(stat="identity") +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip()




###2.5 Wordclouds

#?# plot word cloud for the top 100 words

#?# comparison word cloud 

#?## colors = c("#F8766D", "#00BFC4")

#?# create PandP_sentences: prideprejudice one sentence per row

#?# create austen_chapters: austen_books one chapter per row

#?# get the chapter for each book with the highest ratio of negative words/words
