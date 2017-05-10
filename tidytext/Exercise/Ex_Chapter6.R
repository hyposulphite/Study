
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(gutenbergr)
library(stringr)
library(tidyr)
library(scales)

#?# get data AssociatedPress
data(AssociatedPress)

#?# create ap_lda: conduct LDA with k=2
ap_lda = LDA(AssociatedPress, k=2, control=list(seed=1234))

#?# create ap_topics: tidy beta matrix
ap_topics = tidy(ap_lda, matrix="beta")

#?# create ap_top_terms: get top 10 words that define the two topics
ap_top_terms = ap_topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

#?# plot the words
ap_top_terms %>% 
  mutate(term=reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill=as.factor(topic))) +
  geom_bar(stat="identity") +
  facet_wrap(~topic, scale="free") +
  coord_flip()

#?# find words making greatest difference between topic1 and topic2

#?#   difference defined as log2(topic2/topic1)

### 6.1.2 Document-topic probabilities

#?# create ap_documents: from ap_lda get tidy version of gamma matrix 

#?#   gamma matrix: per-document-per-topic probability

#?# check why document 6 has high topic 2 probability

### 6.2 Example: the great library heist
#_# titles:
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")

#?# create books: download from gutenberg:

#?# create by_chapter: divide into documents, each representing one chapter

#?# create by_chapter_word: split into words

#?# create word_counts: word counts

### 6.2.1 LDA on chapters

#?# create chapters_dtm: get dtm from word_counts

#?# create chapters_lda: LDA with k=4

#?# create chapter_topics: pull out beta matrix for the topics

#?# get top 5 terms by topics and plot

### 6.2.2 Per-document classification

#?# create chapters_gamma: pull out gamm matrix

#?# summarize gamma by book and topic and do classification

#?# check the cases of misclassification

###6.2.3 By word assignments: augment

#?# create assignments: assign words in chapters_lda into each topic

#?# update assignments: check correctness of assignments

#?# plot confusion matrix: how often the misclassification

#?# research on the wrong words

###6.3 Alternative LDA implementations
library(mallet)
