
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

###2.2 Sentiment analysis with inner join

#?# get tidy_books from austen_books: add linenumber and chapter number

#?# get Emma book from tidy_books, and check words in joy sentiment

#?# get positive and negative index of the tidy_books, by blocks of 80 lines

#?# plot overall sentiment index by book and block

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
