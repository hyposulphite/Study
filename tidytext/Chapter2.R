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
nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

## An example of gather and spread

stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocksm <- stocks %>% gather(stock, price, -time)
stocksm %>% spread(stock, price)
stocksm %>% spread(time, price)

#?# get positive and negative index of the tidy_books, by blocks of 80 lines
janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#?# plot overall sentiment index by book and block
ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")


###2.3 Comparing the three sentiment dictionaries

#?# create pride_prejudice: "Pride & Prejudice" in tidy_books
pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

pride_prejudice

#?# create afinn: index=block number, sentiment index and method="AFINN"
afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

#?# pride_prejudice "bing" and "nrc" positive and negative summary by block
bing_and_nrc <- bind_rows(
  pride_prejudice %>% 
  inner_join(get_sentiments("bing")) %>%
  mutate(method = "Bing et al."),
    pride_prejudice %>% 
    inner_join(get_sentiments("nrc") %>% 
    filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")
  ) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#?# plot afinn, bing and nrc scores
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)

###2.4 Most common positive and negative words

#?# tidy_books join "bing" plot most common positive and negative words
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts


bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

###2.5 Wordclouds

#?# plot word cloud for the top 100 words
tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#?# comparison word cloud 
#?## colors = c("#F8766D", "#00BFC4")
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

#?# create PandP_sentences: prideprejudice one sentence per row
PandP_sentences <- data_frame(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

#?# create austen_chapters: austen_books one chapter per row
austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()


austen_chapters %>% 
  group_by(book) %>% 
  summarise(chapters = n())

#?# get the chapter for each book with the highest ratio of negative words/words
bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  top_n(1) %>%
  ungroup()





















